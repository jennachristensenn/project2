library(shiny)
library(shinyalert)
library(tidyverse)
library(dplyr)
library(DT)
library(shinycssloaders)


# additions:
# - add spinners
# - write the readme
# - add names so the variable choices are nicer 
# - look into reset buttons 
# - deploy to shiny

# reading in and manipulating data
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

# define UI for application 
ui <- fluidPage(
  
  titlePanel("Mobile Device Data Exploration"),
  
  sidebarLayout(
    sidebarPanel(
      # character var to select
      h4("Select Variables to Subset the Data:"),
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
                                   "Samsung Galaxy")
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
                     choices = list(
                       "",
                       "App Usage Time (min/day)" = "app_use_time",
                       "Screen On Time (hours/day)" = "screen_time",
                       "Battery Drain (mAh/day)" = "bat_drain",
                       "Number of Apps Installed" = "num_apps",
                       "Data Usage (MB/day)" = "dat_use",
                       "Age" = "age"
                     ),
                     multiple = FALSE,
                     selected = ""),
      uiOutput("slider_var1"),
      
      selectizeInput("num_var2",
                     "Numeric Variable:",
                     choices = list(
                       "",
                       "App Usage Time (min/day)" = "app_use_time",
                       "Screen On Time (hours/day)" = "screen_time",
                       "Battery Drain (mAh/day)" = "bat_drain",
                       "Number of Apps Installed" = "num_apps",
                       "Data Usage (MB/day)" = "dat_use",
                       "Age" = "age"
                     ),
                     multiple = FALSE,
                     selected = ""),
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
                 p("In the Data Exploration tab is where you can explore both qualitative and quantitative summaries as well as graphical representations of the data."),
                 img(src = "phonesBetter.jpg", width = "60%")
        ),
        
        tabPanel("Data Download", 
                 h3("Subset and download the data"),
                 p("Explore the mobile device data below. You can download the full dataset, or select variables on the sidebar to subset the data. Click the 'Download Data' button to save a copy to your computer. "),
                 DT::dataTableOutput("data_table"),
                 downloadButton("download_data", "Download Data")
        ),
        
        tabPanel("Data Exploration", 
                 h3("Numeric and graphic summaries"),
                 p("Explore the data using different methods such as contingency tables, numeric summaries, and graphical diaplays. Use the sidepanel to subset the data as well as the available selections in each of the tabs below."),
                 tabsetPanel(
                   
                   tabPanel("Categorical Summaries", 
                            p("Select one or two character variables to display a contingency table. Please note your subset of the data will be reflected in the summary."),
                            selectizeInput("cont_var", 
                                           "Character Variable(s):",
                                           choices = c("dev_mod", "op_sys", "age", "gender", "user_class"),
                                           multiple = TRUE,
                                           selected = NULL),
                            actionButton("cont_button", "Show Categorical Summary"),
                            verbatimTextOutput("contingency_table")
                   ),
                   
                   tabPanel("Numeric Summaries",
                            p("Select a numeric variable to display a summary of the values. Please note your subset of the data will be reflected in the summary."),
                            selectizeInput("sum_var", 
                                           "Numeric Variable:",
                                           choices = list(
                                             "",
                                             "App Usage Time (min/day)" = "app_use_time",
                                             "Screen On Time (hours/day)" = "screen_time",
                                             "Battery Drain (mAh/day)" = "bat_drain",
                                             "Number of Apps Installed" = "num_apps",
                                             "Data Usage (MB/day)" = "dat_use",
                                             "Age" = "age"
                                           ),
                                           multiple = FALSE,
                                           selected = ""),
                            actionButton("sum_button", "Show Numeric Summary"),
                            verbatimTextOutput("numeric_summary")
                   ),
                   
                   tabPanel("Categorical Visualizations", 
                            p("Select a categorical variable for the x-axis to explore bar charts. Additionally select a variable to differentiate between groups. Please note your subset of the data will be reflected in the graph. "),
                            selectInput("cat_x_var",
                                        "x-axis:",
                                        choices = c("","dev_mod", "op_sys", "age", "gender", "user_class"),
                                        selected = ""),
                            selectInput("cat_fill_var",
                                        "Group By:",
                                        choices = c("","dev_mod", "op_sys", "age", "gender", "user_class"),
                                        selected = ""),
                            actionButton("bar_button", "Show Categorical Visualization"),
                            withSpinner(plotOutput("categorical_plot"))
                   ),
                   
                   tabPanel("Numeric Visualizations",
                            p("Select a numeric variable for the x-axis to explore numeric graphs. Additionally, select a y-axis variable and a fill variable to display variations or groupings in the data. Please note your subset of the data will be reflected in the graph."),
                            selectInput("num_x_var", "x-axis:", 
                                        choices = list(
                                          "",
                                          "App Usage Time (min/day)" = "app_use_time",
                                          "Screen On Time (hours/day)" = "screen_time",
                                          "Battery Drain (mAh/day)" = "bat_drain",
                                          "Number of Apps Installed" = "num_apps",
                                          "Data Usage (MB/day)" = "dat_use",
                                          "Age" = "age"
                                        ),
                                        selected = ""),
                            selectInput("num_y_var", "y-axis:", 
                                        choices = list(
                                          "",
                                          "App Usage Time (min/day)" = "app_use_time",
                                          "Screen On Time (hours/day)" = "screen_time",
                                          "Battery Drain (mAh/day)" = "bat_drain",
                                          "Number of Apps Installed" = "num_apps",
                                          "Data Usage (MB/day)" = "dat_use",
                                          "Age" = "age"
                                        ),
                                        selected = ""),
                            selectInput("num_fill_var", "Group By:", 
                                        choices = c("", "dev_mod", "op_sys", "age", "gender", "user_class"),
                                        selected = ""),
                            actionButton("plot_button", "Show Numeric Visualization"),
                            withSpinner(plotOutput("numeric_plot")))
                 ))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactiveVal(dev_data)
  
  # null values for sliders
  slide_num_var1 <- reactiveVal(NULL)
  slide_num_var2 <- reactiveVal(NULL)
  
  observeEvent(input$num_var1, {
    slide_num_var1(input$num_var1)
  })
  
  # slider num_var1 conditional 
  output$slider_var1 <- renderUI({
    req(slide_num_var1())
    sliderInput("slider_var1",
                label = paste("Select values for", slide_num_var1()),
                min = min(dev_data[[slide_num_var1()]], na.rm = TRUE),
                max = max(dev_data[[slide_num_var1()]], na.rm = TRUE),
                value = c(min(dev_data[[slide_num_var1()]], na.rm = TRUE), 
                          max(dev_data[[slide_num_var1()]], na.rm = TRUE)))
  })
  
  observeEvent(input$num_var2, {
    slide_num_var2(input$num_var2)
  })
  
  # slider num_var2 conditional 
  output$slider_var2 <- renderUI({
    req(slide_num_var2())
    sliderInput("slider_var2",
                label = paste("Select values for", input$num_var2),
                min = min(dev_data[[input$num_var2]], na.rm = TRUE),
                max = max(dev_data[[input$num_var2]], na.rm = TRUE),
                value = c(min(dev_data[[input$num_var2]], na.rm = TRUE), 
                          max(dev_data[[input$num_var2]], na.rm = TRUE)))
  })
  
  # subset data 
  observeEvent(input$subset_sample, {
    data_subset <- dev_data
    
    # radio button subset (character variables)
    if (input$char_var1 != "All") {
      data_subset <- data_subset %>%
        filter(dev_mod == input$char_var1)
    }
    
    if (input$char_var2 != "All") {
      data_subset <- data_subset %>%
        filter(gender == input$char_var2)
    }
    
    # numeric variables subset with conditional checks
    if (input$num_var1 != "") {  
      data_subset <- data_subset %>%
        filter(get(input$num_var1) >= input$slider_var1[1],
               get(input$num_var1) <= input$slider_var1[2])
    }
    
    if (input$num_var2 != "") {  
      data_subset <- data_subset %>%
        filter(get(input$num_var2) >= input$slider_var2[1],
               get(input$num_var2) <= input$slider_var2[2])
    }
    
    filtered_data(data_subset)
  })
  
  # using the filtered data unless not set 
  sub_data <- reactive({
    if (input$subset_sample > 0) {
      filtered_data()
    } else {
      dev_data
    }
  })
  
  # data table output
  output$data_table <- DT::renderDataTable({
    sub_data()
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
  
  # null values for summary buttons
  tab_char_var <- reactiveVal(NULL)
  sum_num_var <- reactiveVal(NULL)
  
  # contingency table output
  observeEvent(input$cont_button, {
    tab_char_var(input$cont_var)
    
    output$contingency_table <- renderPrint({
      req(tab_char_var())
      
      if (length(tab_char_var()) == 1) {
        table(filtered_data()[[tab_char_var()[1]]])
        
      } else if (length(tab_char_var()) == 2) {
        table(filtered_data()[[tab_char_var()[1]]], filtered_data()[[tab_char_var()[2]]])
        
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
      
      var_data <- filtered_data()[[sum_num_var()]]
      list(
        Summary = summary(var_data),
        Sd = sd(var_data, na.rm = TRUE)
      )
    })
  })
  
  # null values for graph buttons
  char_x_var <- reactiveVal(NULL)
  char_fill_var <- reactiveVal(NULL)
  num_x_var <- reactiveVal(NULL)
  num_y_var <- reactiveVal(NULL)
  num_fill_var <- reactiveVal(NULL)
  
  # cat var graphs
  observeEvent(input$bar_button, {
    char_x_var(input$cat_x_var)
    char_fill_var(input$cat_fill_var)
  })
  
  output$categorical_plot <- renderPlot({
    req(char_x_var())
    current_data <- filtered_data()  
    
    plot <- ggplot(current_data, aes_string(x = char_x_var())) +
      geom_bar(position = "dodge") +
      labs(x = char_x_var(), title = char_x_var())
    
    if (char_fill_var() != "") {
      plot <- plot + aes_string(fill = char_fill_var(), group = char_fill_var()) +
        labs(fill = char_fill_var())
    }
    plot
  })
  
  # num var plot
  observeEvent(input$plot_button, {
    num_x_var(input$num_x_var)
    num_y_var(input$num_y_var)
    num_fill_var(input$num_fill_var)
  })
  
  output$numeric_plot <- renderPlot({
    req(num_x_var())
    current_data <- filtered_data()  
    
    if (num_y_var() == "" || is.null(num_y_var())) {
      plot <- ggplot(current_data, aes_string(x = num_x_var())) +
        geom_histogram(bins = 30, aes_string(fill = if (num_fill_var() != "") num_fill_var() else NULL)) +
        labs(x = num_x_var(), title = num_x_var())
      
      if (num_fill_var() != "") {
        plot <- plot + labs(fill = num_fill_var())
      }
    } else {
      plot <- ggplot(current_data, aes_string(x = num_x_var(), y = num_y_var())) +
        geom_point(aes_string(color = if (num_fill_var() != "") num_fill_var() else NULL)) +
        labs(x = num_x_var(), y = num_y_var(), title = paste(num_y_var(), "vs", num_x_var()))
      
      if (num_fill_var() != "") {
        plot <- plot + labs(color = num_fill_var())
      }
    }
    plot  
  })
  
}

shinyApp(ui = ui, server = server)