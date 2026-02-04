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
      "CS for All", 
      fluidRow(
        column(width = 8, offset = 2,
               h2("Research Questions:"),
               tags$ul(
                 tags$li(h5("How have computer and information sciences degrees conferred in the U.S. changed over time by gender and degree level?")),
                 tags$li(h5("Where does Oregon stand in the national CS degree landscape?")),
                 tags$li(h5("What gender and racial disparities exist in participation in secondary-level computer science coursework nationwide?"))
               ),
               hr(),
               div(
               style = "text-align: center; margin-top:30px;",

               h4("Historical Context: animation of conferred bachelor's growth"),
               img(src = gif_base64, 
                   width = "100%", 
                   style = "max-width: 700px; border-radius: 15px; box-shadow: 0 4px 8px rgba(0,0,0,0.1);"),
               p(tags$em("This project explores the equity access status of computer science(CS) in the U.S. from secondary participation to higher education Degrees"),
                 style = "margin-top: 10px; color: #666;")
               )
               )
        )
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
          plotlyOutput("cs_trend_plot", height = "550px")
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
      p("Data Source"),
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

server <- function(input, output, session){
  
  accumulated_cs <- reactive({
    
    req(input$gender_choice)
    
    df_ba <- cs %>% 
      filter(
        degree_level == input$degree_choice,
        gender %in% input$gender_choice)
    
    purrr::map_df(unique(df_ba$academic_year), function(yr){
      df_ba %>% 
        filter(academic_year <= yr) %>% 
        mutate(frame_year = yr)
    })
    
  })
  
  # Output
  
  output$cs_trend_plot <- renderPlotly({
    
    p1 <- ggplot(accumulated_cs(),
           aes(x = academic_year, y = value, 
               color = gender, frame = frame_year)) +
      geom_line(aes(group = gender), linewidth = 1) +
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
shinyApp(ui = ui,server = server)
#rsconnect::deployApp()