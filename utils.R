# Utils script that loads packages and contains the data processing function

# Load packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(ggpubr)
library(openxlsx)


# Function to load and process the data
load_and_process_data <- function(file_path) {

  df <- read_excel(file_path, col_names = TRUE)
  
  # update names in case the template is wrong
  colnames(df) <- c("time", "hhb")
  
  # subset data frame and create new column for normalized data
  df <- df[, 1:2] %>%
    drop_na() %>% 
    mutate(hhb = round(hhb, 2),
           hhb_normal = hhb - round(mean(hhb[1:10]), 2))
  
  # Return the processed data frame
  return(df)
}