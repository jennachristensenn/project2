library(shiny)
library(shinyalert)
library(tidyverse)
library(dplyr)
library(DT)

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
      # character var to select
      h2("Select Variables to Subset the Data:"),
      radioButtons("char_var1",
                   "Device Model",
                   choiceValues = c("All",
                                    "Google Pixel 5", 
                                    "OnePlus 9",
                                    "Xiaomi Mi 11",
                                    "iPhone 12",
                                    "Samsung Galaxy S21"),
                   choiceNames = c("All",
                                   "Google Pixel", 
                                   "OnePlus",
                                   "Xiaomi Mi",
                                   "iPhone",
                                   "Samsung Galaxy S21")
      ),
      radioButtons("char_var2",
                   "Gender",
                   choiceValues = c("All",
                                    "Male",
                                    "Female"),
                   choiceNames =  c("All",
                                    "Male",
                                    "Female")
      ),
      # numeric var to select
      selectizeInput("num_var1",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = NULL),
      uiOutput("slider_var1"),
      # numeric var to select
      selectizeInput("num_var2",
                     "Numeric Variable:",
                     choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "bat_use", "age"),
                     multiple = FALSE,
                     selected = NULL),
      uiOutput("slider_var2"),
      # button to subset
      actionButton("subset_sample","Subset the Data!")
      ),
      mainPanel(
        tabsetPanel(
          # tabs information
          tabPanel("About", 
                   h3("Purpose of the application!"),
                   p("This Shiny app lets you look at differernt aspects of mobile device data. Try subsetting the data and exploring the numeric and graphical summaries of the data. More to be added later...")),
          tabPanel("Data Download", 
                   h3("Subset and download the data!"),
                   p("Explore the mobile device data below. You can download the full dataset, or select variables on the sidebar to subset the data. Click the 'Download Data' button to save a copy to your computer. "),
                   DT::dataTableOutput("data_table"),
                   downloadButton("download_data", "Download Data")),
          tabPanel("Data Exploration", 
                   h3("Numeric and graphic summaries!"),
                   p("Explore the data using different subsets you find interesting..."))
        )
      )
    )
)

server <- function(input, output, session) {
  
  # slider num_var1 conditional 
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
  
  # slider num_var2 conditional 
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
  
  # subset data (reacts to slider every time values are changed after button is hit once)
  filtered_data <- reactive({
    req(input$subset_sample)
    data_subset <- dev_data
    
    ## debug
    print(paste("Device Model Selected:", input$char_var1))
    print(paste("Gender Selected:", input$char_var2))
    
    # radio button subset (char)
    if (input$char_var1 != "All") {
      data_subset <- data_subset %>%
        filter(as.character(dev_mod) == input$char_var1)
    }
    
    if (input$char_var2 != "All") {
      data_subset <- data_subset %>%
        filter(as.character(gender) == input$char_var2)
    }
    
    # select subset (num)
    if (!is.null(input$num_var1)) {
      data_subset <- data_subset %>%
        filter(get(input$num_var1) >= input$slider_var1[1],
               get(input$num_var1) <= input$slider_var1[2])
    }
    if (!is.null(input$num_var2)) {
      data_subset <- data_subset %>%
        filter(get(input$num_var2) >= input$slider_var2[1],
               get(input$num_var2) <= input$slider_var2[2])
    }
    
    ## debug
    print(paste("Number of Rows After Filtering:", nrow(data_subset))) 
    return(data_subset)
  })
  
  # data table output
  output$data_table <- DT::renderDataTable({
    if (input$subset_sample == 0) {
      return(dev_data)
    } else {
      return(filtered_data())}
  })
  
  # download the data
  output$download_data <- downloadHandler(
    filename = function() {
      paste("mobile_device_data_", ".csv", sep = "") 
    },
    content = function(file) {
      write_csv(filtered_data(), file)  
    }
  )
}

shinyApp(ui = ui, server = server)