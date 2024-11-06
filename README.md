# ST 558 - Project2
#### Author: Jenna Christensen

### About
This project is about exploring the use of Shiny applications in R. I chose to do this project with a mobile device dataset that can be found on kaggle at https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset. The data includes 11 variables and 700 samples of user data for the purpose of 'analyzing mobile usage patterns and user behavior classification across devices.' See variables below:
-   User ID: Unique identifier for each user.
-   Device Model: Model of the user's smartphone.
-   Operating System: The OS of the device (iOS or Android).
-   App Usage Time: Daily time spent on mobile applications, measured in minutes.
-   Screen On Time: Average hours per day the screen is active.
-   Battery Drain: Daily battery consumption in mAh.
-   Number of Apps Installed: Total apps available on the device.
-   Data Usage: Daily mobile data consumption in megabytes.
-   Age: Age of the user.
-   Gender: Gender of the user (Male or Female).
-   User Behavior Class: Classification of user behavior based on usage patterns (1 to 5).

### Purpose 
This Shiny app lets the user explore different aspects of mobile device data by subsetting the data, saving a copy to their computer, and investigate different numeric and graphic summaries. There are four main sections of the application.

1) Sidebar: The user can select character and numeric variables to subset the data that will be reflected in the summaries and visualizations throughout the application. 
2) About Tab: The user will be able to read some background on the dataset and visit the original data source on kaggle. Additionally, the user can read about the purpose of each main component of the app, and what they will be able to do in each section. 
3) Download Data Tab: The user will initially see a preview of the complete dataset until a subset is selected. Here the user can download the data to their computer. 
4) Data Visualization Tab: The user can explore the different subtabs in this section that display categorical or numeric summaries and graphs. There are varying inputs here that the user can select to investigate components of the data they find interesting.

