library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(scales)
library(here)
library(plotly)

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


##############################################################################
#UI
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty"),
  
  navset_pill(
    id = "tab",
    
    nav_panel(
      "CS for All", 
      h2("Research Questions:"),
      h4("1.	How have computer and information sciences degrees conferred in the U.S. changed over time by gender and degree level?"),
      h4("2.	Where does Oregon stand in the national CS degree landscape?"),
      h4("3.	What gender and racial disparities exist in participation in secondary-level computer science coursework nationwide?"),
    ),
    
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
          plotlyOutput("cs_trend_plot", height = "550px"),
          hr(), # adds spacing
          div(
            style = "text-align: center; 
            background-color: #f8f9fa; 
            padding: 20px;
            border-radius: 10px",
            h4("Historical Context: animation of conferred bachelor's growth"),
            img(src = "cs_ba_anim.gif", 
                width = "100%", 
                style = "max-width: 800px;height = auto;")
        )
      )
    )
    ),
    
    nav_panel(
      "CS Degrees by State", 
      h2("State Map")),
    
    nav_panel(
      "National AP CS Participation",
      h2("Treemap")),
    
    nav_panel(
      "About",
      h3("About This Project"),
      p("Created by Michelle Cui"),
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


#############################################################################
# SERVER

server <- function(input, output, session){
  
  filtered_cs <- reactive({
    cs %>% 
      filter(
        degree_level == input$degree_choice,
        gender %in% input$gender_choice)
  })
  
  # Output
  
  output$cs_trend_plot <- renderPlotly({
    
    p1 <- ggplot(filtered_cs(),
           aes(x = academic_year, y = value, 
               color = gender, frame = academic_year)) +
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
      theme_minimal()
    
    ggplotly(p1) %>% 
      animation_opts(frame = 100, transition = 0, redraw = FALSE) %>% 
      animation_slider(currentvalue = list(prefix = "Year: "))
    
  })

}

##############################################################################
# RUN APP
shinyApp(ui,server)
#rsconnect::deployApp()