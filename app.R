library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(ggplot2)
library(scales)
library(here)
library(plotly)
library(purrr)

library(base64enc)
gif_base64 <- dataURI(file = here("www:", "cs_ba_anim.gif"), mime = "image/gif")

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
  theme = bs_theme(
    bg = "white",fg = "#180e00", primary = "#ab9f7a",
    base_font = font_google("Open Sans"),
    code_font = font_google("Open Sans")
  ),
  
  navset_pill(
    id = "tab",
    
    nav_panel(
      "Main", 
      # 
      # fluidRow(
      #   
      #          h4("Historical Context: animation of conferred bachelor's growth"),
      #          tags$img(src = gif_base64, 
      #              width = "100%", 
      #              style = "max-width: 700px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"),
      #          p(tags$em("This project explores the equity access status of computer science(CS) in the U.S. from secondary participation to higher education Degrees"),
      #            style = "margin-top: 10px; color: #666;")
      #          )
               ),
    
    nav_panel(
      "The 60-Year Gender Gap in Computer Science",
      
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
          plotlyOutput("cs_trend_plot", height = "550px")
)
)
),

    nav_panel(
      "Oregon Landscape", 
      h2("Use OLDC/CS for Oregon data to show the teacher-student gap")),
    
    nav_panel(
      "The High School Pipeline",
      h2("Use CRDC data to show AP enrollment disparities")),

    nav_panel(
      "Policy Impact"
    ),
    
    nav_panel(
      "About",
      h3("About This Project"),
      p("Data Source"),
      p(
        a(
          "Overview of Computer Science Implementation Plan",
          href = "https://www.oregon.gov/ode/schools-and-districts/grants/Pages/Computer-Science-Implementation-Plan.aspx",
          target = "_blank"
        )
      ),
      p(
        a(
          "GitHub Repository",
          href = "https://github.com/Go-M-C/Student_Access_to_AP_CS",
          target = "_blank"
        )
      ),
      p("References"),
      p("Michelle Cui"),
    )
  )
)


#############################################################################
# SERVER

server <- function(input, output){
  
  filtered_cs <- reactive({
    
    cs %>% 
      filter(degree_level == input$degree_choice) %>% 
      filter(gender %in% input$gender_choice)})
    
  # Output
  
  output$cs_trend_plot <- renderPlotly({
    
    p1 <- ggplot(filtered_cs(), 
                 aes(x = academic_year, y = value,color = gender)) +
      geom_line(size = 1) +
      geom_point(alpha = 0.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(1965, 2022, by = 4))+
      scale_color_manual(values = c("Male" = "#21908c", "Female" = "#440154")) +
      geom_vline(xintercept = 2016, linetype = "dashed", color = "red")
      labs(title = paste(
          "United States", tools::toTitleCase(input$degree_choice),
          "Degree Trends"
        ),
        x = "Academic Year",
        y = "Number of Degrees") +
      theme_minimal()
    
    ggplotly(p1)
    
  })

}

##############################################################################
# RUN APP
shinyApp(ui = ui,server = server)
#rsconnect::deployApp()