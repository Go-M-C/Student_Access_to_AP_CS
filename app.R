library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(scales)
library(here)


# loading data

cs <- read.csv(here("data","cs_degree_clean.csv"))
cs <- cs %>% 
  mutate(
    degree_level = factor(degree_level,
                          levels = c("bachelor","master","doctor")),
    gender = factor(gender,
                    levels = c("male","female"),
                    labels = c("Male","Female"))
  )


# UI
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
          plotOutput("cs_trend_plot", height = "550px"),
          br(), # adds spacing
          h4("Animated Trends for Bachelor's Degree"), # adds heading
          tags$video(src = "cs_ba_anim.mp4", type = "video/mp4", 
                     autoplay = TRUE,
                     loop = TRUE,
                     height = "500px")
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



# SERVER

server <- function(input, output, session){

  
  filtered_cs <- reactive({
    cs %>% 
      filter(
        degree_level == input$degree_choice,
        gender %in% input$gender_choice
      )
  })
  
  # Output
  
  output$cs_trend_plot <- renderPlot({
    
    ggplot(filtered_cs(),
           aes(x = academic_year, y = value, color = gender)) +
      geom_line(size = 1) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(1965, 2022, by = 4))+
      scale_color_viridis_d(begin = 0.2, end = 0.8) +
      labs(
        title = paste(
          "CS", tools::toTitleCase(input$degree_choice),
          "Degree by Gender (1965-2022)"
        ),
        x = "Year",
        y = "Number of Degrees",
        color = "Gender"
      )+
      theme_minimal(base_size = 14)
  })
}


# RUN APP
shinyApp(ui,server)
#rsconnect::deployApp()