
library(shiny)
library(bslib)
library(bsicons)
# Define UI for application that draws a histogram
ui <- fluidPage(

    navset_pill(
      id = "tab",
      
      nav_panel("CS for All"),
      
      nav_panel(
        "CS Degree Trends",
        
        sidebarLayout(
          
          sidebarPanel(
            selectInput(
              inputId = "degree_choice",
              label   = "Select Degree Level",
              choices = c(
                "Bachelor's" = "bachelor",
                "Master's"   = "master",
                "Doctor's"   = "doctor"
                ),
              selected = "bachelor"
              ),
            
            checkboxGroupInput(
              inputId = "gender_choice",
              label = "Select Gender",
              choices = c("Male", "Female"),
              selected = c("Male", "Female")
            )
          ),
          
          mainPanel(
            plotOutput("cs_trend_plot", height = "550px")
          )
        
        )
      ),
    
      nav_panel("CS Degrees by State"),
      
      nav_panel("National AP CS Participation"),
      
      nav_panel(
        "About",
        h3("About This Project"),
        p("Created by Michelle Cui."),
        p(
          a(
            "GitHub Repository",
            href = "https://github.com/Go-M-C/Student_Access_to_AP_CS",
            target = "_blank"
            )
        )
      )
    )
)
