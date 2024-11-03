library(shiny)
library(shinyalert)
library(tidyverse)
library(dplyr)

#source("app_prep.qmd")
dev_data <- read_csv("user_behavior_dataset.csv")

dev_data <- dev_data |>
  rename("user_id" = "User ID",
         "dev_mod" = "Device Model", 
         "op_sys" = "Operating System",
         "app_use_time" = "App Usage Time (min/day)",
         "screen_time" = "Screen On Time (hours/day)",
         "bat_drain" = "Battery Drain (mAh/day)",
         "num_apps" = "Number of Apps Installed",
         "dat_use" = "Data Usage (MB/day)",
         "age" = "Age",
         "gender" = "Gender",
         "user_class" = "User Behavior Class") |>
  mutate(across(c(dev_mod, op_sys, gender, user_class), as.factor)) |>
  mutate(user_id = as.character(user_id))

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
                     selected = NULL),
      selectizeInput("num_var1",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = NULL),
      uiOutput("slider_var1"),
      selectizeInput("num_var2",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = NULL),
      uiOutput("slider_var2"),
      actionButton("subset_sample","Subset the Data!")
    ),
      mainPanel(
        tabsetPanel(
          tabPanel("About", 
                   h3("Purpose of the application!"),
                   p("This Shiny app lets you look at differernt aspects of mobile device data. Try subsetting the data and exploring the numeric and graphical summaries of the data. More to be added later...")),
          tabPanel("Data Download", 
                   h3("Subset and download the data!"),
                   p("You can download any version of the data to your local machine...")),
          tabPanel("Data Exploration", 
                   h3("Numeric and graphic summaries!"),
                   p("Explore the data using different subsets you find interesting..."))
        )
      )
    )
)

server <- function(input, output, session) {
  
  output$slider_var1 <- renderUI({
    print(paste("Numeric Variable 1 Selected:", input$num_var1)) 
    if(is.null(input$num_var1)) {
      return(NULL)}
    sliderInput("slider_var1",
                label = paste("Select values for", input$num_var1),
                min = min(dev_data[[input$num_var1]]),
                max = max(dev_data[[input$num_var1]]),
                value = c(min(dev_data[[input$num_var1]]), 
                          max(dev_data[[input$num_var1]])))
  })
  
  output$slider_var2 <- renderUI({
    print(paste("Numeric Variable 2 Selected:", input$num_var2))
    if(is.null(input$num_var2)){
      return(NULL)}
    sliderInput("slider_var2",
                label = paste("Select values for", input$num_var2),
                min = min(dev_data[[input$num_var2]]),
                max = max(dev_data[[input$num_var2]]),
                value = c(min(dev_data[[input$num_var2]]), 
                          max(dev_data[[input$num_var2]])))
  })
  
}

shinyApp(ui = ui, server = server)