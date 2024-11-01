library(shiny)
library(shinyalert)
library(tidyverse)
library(dplyr)

source("")

# Define UI for application 
ui <- fluidPage(
  
  titlePanel("Mobil Device Data Exploration"),
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Subset the Data:"),
      selectizeInput("cat_var",
                     "Categorical Variable(s):",
                     # need to add in option for "all"
                     choices = c("dev_mod", "op_sys", "gender", "user_class"), 
                     multiple = TRUE,
                     selected = "dev_mod"), #all
      selectizeInput("num_var1",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = "app_use_time"),
      uiOutput("slider_var1"),
      selectizeInput("num_var2",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = "screen_time"),
      uiOutput("slider_var2"),
      actionButton("subset_sample","Subset the Data!")
      )))