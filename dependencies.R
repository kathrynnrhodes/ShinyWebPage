#Dependencies to use the "app.R" that encodes the Balance Beam Assessment Shiny webpage

required_packages <- c("shiny", "ggplot2", "dplyr", "tidyr", "readr", "stringr", "broom")

#Here is a custom function to check and install any missing packages the user does not have
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages)
  } else {
    message("All required packages are installed.") #Message that will pop up if you are missing any of the packages
  }
  
  #Here is when the packages would get installed
  invisible(lapply(packages, library, character.only = TRUE))
}

#Run the function to check if you have the required packages in your library and then install any missing ones
install_if_missing(required_packages)