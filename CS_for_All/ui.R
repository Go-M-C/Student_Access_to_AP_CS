
library(shiny)
library(bslib)
# Define UI for application that draws a histogram
ui <- fluidPage(

    navset_pill(
      id = "tab",
      
      nav_panel("A", "Page A content"),
      nav_panel("B", "Page B content"),
      nav_panel("C", "Page C content"),
      
      nav_menu(
        "Other links",
        nav_panel("D", "Panel D content"),
        "----",
        "Description:",
        nav_item(
          a("Shiny", href = "http://shiny.posit.co", target = "_blank")
        )
      )
    )
    )
