#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(broom)

#UI definition
ui <- fluidPage(
  titlePanel("Mouse Balance Beam Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      #Data loading options so you can use the example data or upload your own CSV with new data!
      radioButtons("data_source", "Data Source:",
                   choices = c("Use Example Data" = "example",
                               "Upload Your Own Data" = "upload"),
                   selected = "example"),
      
      #Only show file upload when upload is selected
      conditionalPanel(
        condition = "input.data_source == 'upload'",
        fileInput("file", "Upload your CSV file",
                  accept = c("text/csv", 
                             "text/comma-separated-values,text/plain", 
                             ".csv"))
      ),
      
      #Analysis options based on individual balance beam data needs
      selectInput("plot_type", "Visualization Type:",
                  choices = c("Boxplot" = "box",
                              "Violin Plot" = "violin",
                              "Bar Plot (means with error bars)" = "bar",
                              "Individual Data Points" = "point"),
                  selected = "box"),
      #Based on mine uploaded to create this (the C57_p14_Balance_Beam.csv in the repository)
      checkboxGroupInput("groups", "Groups to Display:",
                         choices = c("Male Control" = "M CTRL",
                                     "Male CCl4" = "M CCl4",
                                     "Female Control" = "F CTRL",
                                     "Female CCl4" = "F CCl4"),
                         selected = c("M CTRL", "M CCl4", "F CTRL", "F CCl4")),
      #To adjust the comparison (look at sex differences, differences between treatment groups, etc.)
      checkboxInput("compare_sex", "Compare by Sex", value = TRUE),
      checkboxInput("compare_treatment", "Compare by Treatment", value = TRUE),
      checkboxInput("compare_interaction", "Test Sex-Treatment Interaction", value = TRUE),
      
      hr(),
      
      #Creating some advanced options the user can choose from for statistical analysis or adjusted visualization of results
      checkboxInput("show_advanced", "Show Advanced Options", value = FALSE),
      
      conditionalPanel(
        condition = "input.show_advanced == true",
        checkboxInput("add_jitter", "Add Individual Data Points", value = TRUE),
        checkboxInput("add_means", "Add Mean Points", value = TRUE),
        sliderInput("alpha", "Point Transparency:", 
                    min = 0.1, max = 1, value = 0.7, step = 0.1),
        checkboxInput("log_scale", "Use Log Scale for Y-axis", value = FALSE)
      )
    ),
    
    #Create a button so the generated plot for your data based on your selections can be downloaded for use as a figure in a presentation/manuscript
    mainPanel(
      tabsetPanel(
        tabPanel("Visualization",
                 plotOutput("plot", height = "500px"),
                 downloadButton("download_plot", "Download Plot")),
        
        #Text to inform where user can select from
        tabPanel("Statistical Analysis",
                 h3("Summary Statistics"),
                 tableOutput("summary_table"),
                 
                 h3("Statistical Tests"),
                 verbatimTextOutput("anova_results"),
                 
                 conditionalPanel(
                   condition = "input.compare_treatment == true",
                   h4("Treatment Effect (Pooled by Sex)"),
                   verbatimTextOutput("treatment_ttest")
                 ),
                 
                 conditionalPanel(
                   condition = "input.compare_sex == true",
                   h4("Sex Effect (Pooled by Treatment)"),
                   verbatimTextOutput("sex_ttest")
                 ),
                 
                 h4("Pairwise Comparisons"),
                 tableOutput("tukey_results")),
        
        tabPanel("Data",
                 downloadButton("download_data", "Download Processed Data"),
                 br(), br(),
                 dataTableOutput("data_table"))
      )
    )
  )
)

#Required server logic
server <- function(input, output, session) {
  
  #Loading in and processing my own data as an example to populate the Shiny app upon opening
  data_reactive <- reactive({
    if(input$data_source == "example") {
      #My own pre-loaded data (as mentioned previously this link is from the repository)
      file_path <- "https://raw.githubusercontent.com/kathrynnrhodes/ShinyWebPage/refs/heads/main/C57_p14_Balance_Beam.csv"
      df <- read_csv(file_path)
    } else {
      #Using my uploaded file
      req(input$file)
      df <- read_csv(input$file$datapath)
    }
    
    #Processing my own balance beam data
    df <- df %>%
      #Extracting the sex and treatment from the "Animal" column
      mutate(
        Sex = str_sub(Animal, 1, 1),
        Treatment = ifelse(str_detect(Animal, "CTRL"), "Control", "CCl4"),
        Group = str_extract(Animal, "^[MF] (CTRL|CCl4)"),
        #Extracting the animal ID for grouping repeated measures
        AnimalID = str_extract(Animal, "^[MF] (CTRL|CCl4) [0-9]+")
      )
    
    return(df)
  })
  
  #Filtering my data based on selected groups
  filtered_data <- reactive({
    df <- data_reactive()
    df %>% filter(Group %in% input$groups)
  })
  
  #Creating the data visualization
  output$plot <- renderPlot({
    df <- filtered_data()
    
    #Base plot
    p <- ggplot(df, aes(x = Group, y = Slips, fill = Group)) 
    
    #Adding the main plot elements based on the selected plot type
    if(input$plot_type == "box") {
      p <- p + geom_boxplot(alpha = 0.7)
    } else if(input$plot_type == "violin") {
      p <- p + geom_violin(alpha = 0.7, trim = FALSE)
    } else if(input$plot_type == "bar") {
      p <- p + stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
        stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)
    } else if(input$plot_type == "point") {
      p <- p + geom_point(size = 3, position = position_jitter(width = 0.1, height = 0), alpha = input$alpha)
    }
    
    #For adding individual data points if requested (and not already shown)
    if(input$add_jitter && input$plot_type != "point") {
      p <- p + geom_point(position = position_jitter(width = 0.1, height = 0), alpha = input$alpha)
    }
    
    #For adding mean points if requested
    if(input$add_means) {
      p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
                            fill = "white", color = "black")
    }
    
    #For using a log scale if requested
    if(input$log_scale) {
      p <- p + scale_y_continuous(trans = "log1p")
    }
    
    #Customizing the appearance of the visual output of data (labels, etc.)
    p + labs(title = "Mouse Balance Beam Performance",
             subtitle = "Number of Slips During Balance Beam Test",
             x = "",
             y = "Number of Slips",
             fill = "Group") +
      theme_minimal(base_size = 14) +
      theme(
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.title.y = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })
  
  #Creating a summary statistics table of the data that can also be downloaded
  output$summary_table <- renderTable({
    df <- filtered_data()
    
    df %>%
      group_by(Group) %>%
      summarize(
        N = n(),
        Mean = mean(Slips, na.rm = TRUE),
        SD = sd(Slips, na.rm = TRUE),
        SEM = SD / sqrt(N),
        Min = min(Slips, na.rm = TRUE),
        Max = max(Slips, na.rm = TRUE)
      ) %>%
      ungroup()
  })
  
  #Two-way ANOVA if requested by user
  anova_model <- reactive({
    df <- filtered_data()
    
    if(input$compare_interaction) {
      aov(Slips ~ Sex * Treatment, data = df)
    } else {
      aov(Slips ~ Sex + Treatment, data = df)
    }
  })
  
  #Displaying the previous ANOVA results
  output$anova_results <- renderPrint({
    model <- anova_model()
    summary(model)
  })
  
  #Treatment t-test (pooled by sex)
  output$treatment_ttest <- renderPrint({
    df <- filtered_data()
    t.test(Slips ~ Treatment, data = df)
  })
  
  #Sex t-test (pooled by treatment)
  output$sex_ttest <- renderPrint({
    df <- filtered_data()
    t.test(Slips ~ Sex, data = df)
  })
  
  #Tukey's HSD for pairwise comparisons
  output$tukey_results <- renderTable({
    model <- anova_model()
    tukey <- TukeyHSD(model)
    
    #Formatting the results as a data frame
    if(input$compare_interaction) {
      as.data.frame(tukey$`Sex:Treatment`) %>%
        rownames_to_column("Comparison")
    } else {
      #Combining the results from Sex and Treatment
      bind_rows(
        as.data.frame(tukey$Sex) %>%
          rownames_to_column("Comparison") %>%
          mutate(Factor = "Sex"),
        as.data.frame(tukey$Treatment) %>%
          rownames_to_column("Comparison") %>%
          mutate(Factor = "Treatment")
      )
    }
  })
  
  #Data table output
  output$data_table <- renderDataTable({
    filtered_data()
  })
  
  #Downloading handlers for downloading data 
  output$download_data <- downloadHandler(
    filename = function() {
      "processed_balance_beam_data.csv"
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_plot <- downloadHandler(
    filename = function() {
      "balance_beam_plot.png"
    },
    content = function(file) {
      ggsave(file, plot = {
        df <- filtered_data()
        
        #Base plot
        p <- ggplot(df, aes(x = Group, y = Slips, fill = Group)) 
        
        #Adding the main plot elements based on the selected plot type
        if(input$plot_type == "box") {
          p <- p + geom_boxplot(alpha = 0.7)
        } else if(input$plot_type == "violin") {
          p <- p + geom_violin(alpha = 0.7, trim = FALSE)
        } else if(input$plot_type == "bar") {
          p <- p + stat_summary(fun = mean, geom = "bar", alpha = 0.7) +
            stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2)
        } else if(input$plot_type == "point") {
          p <- p + geom_point(size = 3, position = position_jitter(width = 0.1, height = 0), alpha = input$alpha)
        }
        
        #Adding the individual data points if requested (and not already shown)
        if(input$add_jitter && input$plot_type != "point") {
          p <- p + geom_point(position = position_jitter(width = 0.1, height = 0), alpha = input$alpha)
        }
        
        #Adding mean points if requested
        if(input$add_means) {
          p <- p + stat_summary(fun = mean, geom = "point", shape = 23, size = 4, 
                                fill = "white", color = "black")
        }
        
        #Using a log scale if requested
        if(input$log_scale) {
          p <- p + scale_y_continuous(trans = "log1p")
        }
        
        #Customize appearance
        p + labs(title = "Mouse Balance Beam Performance",
                 subtitle = "Number of Slips During Balance Beam Test",
                 x = "",
                 y = "Number of Slips",
                 fill = "Group") +
          theme_minimal(base_size = 14) +
          theme(
            legend.position = "none",
            plot.title = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            axis.title.y = element_text(face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          )
      }, width = 10, height = 7, dpi = 300)
    }
  )
}

#And now we can run the application!!!
shinyApp(ui = ui, server = server)
