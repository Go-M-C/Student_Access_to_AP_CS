
library(shiny)
library(bslib)
library(bsicons)
# Define UI for application that draws a histogram
ui <- fluidPage(

    navset_pill(
      id = "tab",
      
      nav_panel("CS for All"),
      nav_panel("CS Degree Trends in the U.S."),
      nav_panel("CS Degrees by State"),
      nav_panel("National AP CS Participation"),
      
      nav_menu(
        "About",
        h3("About This Project"),
        p("Created by [Michelle Cui]."),
        p(
          a("GitHub Repository",
            href = "https://github.com/Go-M-C/Student_Access_to_AP_CS",
            target = "_blank")
        ),
        "----",
        nav_item(
          a("Shiny", href = "http://shiny.posit.co", target = "_blank")
        )
      )
    )
    )
