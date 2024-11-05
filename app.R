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
  
  titlePanel("Mobile Device Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      # character var to select
      h3("Select Variables to Subset the Data:"),
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
      # numeric var to select -- find a way to fix names
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
      actionButton("subset_sample","Subset the Data")
      ),
      mainPanel(
        tabsetPanel(
          # tabs information
          tabPanel("About", 
                   h3("Purpose of the application"),
                   p("This Shiny app lets you explore differernt aspects of mobile device data by subsetting the data, allowing you to save a copy to your computer, and investigate different numeric and grpahic summaries."),
                   p("This data is posted on kaggle, where it is owned an updated by vala khorasani. The data includes 11 variables and 700 samples of user data for the purpose of 'analyzing mobile usage patterns and user behavior classification across devices.'"),
                   p("For more information, view the original data source on kaggle: "),
                   a("Mobile Device Usage and User Behavior Dataset", href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset"),
                   h3("Layout of the Application"),
                   p("The sidebar is where you can select character and numeric variables to subset the data. Note that your selections will only change the data after the 'Subset the Data' button is pressed."),
                   p("Here in the Abount tab you have an overview of the application and information about the mobile device dataset."),
                   p("In the Download Data tab you will see a preview of a data table, and can make adjustents to this by subsetting on the sidebar. Additionally, you can download a copy of the original or subsetted data to your computer with the 'Download Data' button."),
                   p("In the Data Exploration tab is where you can explore both qualitative and quantitative summaries as well as graphical representations of the data. Note that you must select desired variables with the sidebar in order to see the output display. "),
                   img(src = "phonesBetter.jpg", width = "60%")
                   ),
          
          tabPanel("Data Download", 
                   h3("Subset and download the data!"),
                   p("Explore the mobile device data below. You can download the full dataset, or select variables on the sidebar to subset the data. Click the 'Download Data' button to save a copy to your computer. "),
                   DT::dataTableOutput("data_table"),
                   downloadButton("download_data", "Download Data")
                   ),
          
          tabPanel("Data Exploration", 
                   h3("Numeric and graphic summaries!"),
                   p("Explore the data using different subsets you find interesting..."),
                   tabsetPanel(
                     
                     tabPanel("Qualitative Summaries", 
                              p("Select one or two character variables to display a contingency table."),
                              selectizeInput("cont_var", 
                                             "Character Variable(s):",
                                             choices = c("dev_mod", "op_sys", "age", "gender", "user_class"),
                                             multiple = TRUE,
                                             selected = NULL),
                              actionButton("cont_button", "Show Contingency Table"),
                              verbatimTextOutput("contingency_table")
                              ),
                     
                     tabPanel("Numeric Summaries",
                              p("Select a numeric variable to display a summary of the values."),
                              selectizeInput("sum_var", 
                                             "Numeric Vriable:",
                                             choices = c("app_use_time", "screen_time", "bat_drain", "num_apps", "dat_use"),
                                             multiple = FALSE,
                                             selected = NULL),
                              actionButton("sum_button", "Show Numeric Summaries"),
                              verbatimTextOutput("numeric_summary")
                              ),
                     
                     tabPanel("Graphical Summaries", 
                              p("Explore visualizations here."))
                              #plotOutput("data_plot"))
                   ))
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
  
  # subset data 
  filtered_data <- reactiveVal(dev_data)
    observeEvent(input$subset_sample, {
      data_subset <- dev_data
    
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
  
    filtered_data(data_subset)
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
      paste("mobile_device_data", ".csv", sep = "") 
    },
    content = function(file) {
      write_csv(filtered_data(), file)  
    }
  )
  
  # null values for buttons
  tab_char_var <- reactiveVal(NULL)
  sum_num_var <- reactiveVal(NULL)
  
  # contingency table output
  observeEvent(input$cont_button, {
    tab_char_var(input$cont_var)
    
    output$contingency_table <- renderPrint({
      req(tab_char_var())
      
      if (length(tab_char_var()) == 1) {
        table(dev_data[[tab_char_var()[1]]])
        
      } else if (length(tab_char_var()) == 2) {
        table(dev_data[[tab_char_var()[1]]], dev_data[[tab_char_var()[2]]])
        
      } else {
        "Please select one or two character variables."
      }
    })
  })
  
  # numeric summaries output
  observeEvent(input$sum_button, {
    sum_num_var(input$sum_var)
    
    output$numeric_summary <- renderPrint({
      req(sum_num_var())
      
      var_data <- dev_data[[sum_num_var()]]
      list(
        Summary = summary(var_data),
        Sd = sd(var_data)
      )
    })
  })
  
}

shinyApp(ui = ui, server = server)