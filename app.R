# For shiny app structure ui
library(shiny)
library(bslib)
library(bsicons)

# For data wrangling
library(dplyr)
library(ggplot2)
library(scales)
library(here)
library(plotly)
library(purrr)
library(tidyr)
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(janitor)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE) # save the shapefiles downloads locally
# library(base64enc)
# gif_base64 <- dataURI(file = here("www:", "cs_ba_anim.gif"), mime = "image/gif")



##############################################################################
#UI
ui <- page_sidebar(
  
  title = div(
    style = "width:100%; display:flex; 
    justify-content:space-between; align-items:center;",
    
    div(
      style = "line-height:1.2; flex:1;",
      h3("Data Science Capstone"),
      tags$small("Computer Science Education Trends in Oregon")
      ),
    tags$a(
      href = "https://github.com/Go-M-C/Student_Access_to_AP_CS",
      target = "_blank",
      icon("github",style = "font-size:22px; color: #180d00;"),
    )
  ),
  
    tags$style(HTML("
    .nav-tabs .nav-link {
      border: none !important;
      color: #180e00;
      }
      
      .nav-tabs .nav-link.active{
      color: #ab9f7a !important;
      font-weight: 600;
      background-color: transparent !important;
      }
      ")),

    theme = bs_theme(
      bg = "white",
      fg = "#180e00", 
      primary = "#ab9f7a",
      base_font = font_google("Open Sans"),
      code_font = font_google("Open Sans")
      ),
   #==========================SIDEBAR=========================# 
    sidebar = sidebar(
      
      navset_tab(
      id = "tabs",

      nav_panel(
        title = tagList(icon("house"), " Welcome"),
        value = "welcome",
        ),
      
      nav_panel(
        title = tagList(icon("map"), " Access"),
        value = "access",
        ),
      
      nav_panel(
        title = tagList(icon("graduation-cap"), " Degrees"),
        value = "degrees",
        ),

      nav_panel(
        title = tagList(icon("chart-line"), " Trends"),
        value = "trends") # trends nav_panel ends
        ) # navset_tab ends
    ),
  
  # ========================== MAIN CONTENT============================#
      uiOutput("main_content")
    )

#############################################################################
# SERVER

server <- function(input, output){
  
  output$main_content <- renderUI({
    switch(input$tabs,
           "welcome" = fluidPage(
             h2("Welcome"),
             br(),
             p("By 2026, the Computer Science for All initiative will reach its 
               10-year milestone since its launch in the United States. 
               The initiative marked a proactive move and a commitment 
               to expanding Computer Science (CS) education 
               from kindergarten through high school, 
               aiming to equip students with computational thinking (CT) skills 
               for participation in a fast-shifting and technology-driven economy."),
             br(),
             p("In Oregon, the Oregon Department of Education (ODE) 
               and the Higher Education Coordination Commission (HECC) 
               initiated a statewide implementation plan in 2022 to expand access 
               to CS education for all public-school students by the 2027-2028 academic year. 
               For school district leaders and educators, this raises important questions:"),
             br(),
             p("-Who is currently participating in computer science courses?"),
             p("-Are participation patterns changing over time?"),
             p("-What does Oregon’s CS education ecosystem look like?"),
             br(),
             p("This interactive dashboard intends to provide a data-informed picture of 
               computer science participation and degree attainment in Oregon by bringing together 
               multiple public data source, including the Civil Rights Data Collection and 
               the National Center for Educational Statistics, and CODE.org."),
             br(),
             p("Use the sidebar to navigate through national trends,
               Oregon's landscape, and policy impact.")
           ),
           
           "access" = fluidPage(
             h2("Advanced Placement Computer Science Course Enrollment Across the U.S."),
             plotlyOutput("ap_cs_map"),
             plotlyOutput("ac_cs_treemap"),
             p("Circle size represnets enrollment. 
               Treemap shows aggregated enrollment by group")
           ),
     #=====================================================================#      
           "degrees" = fluidPage(
             h2("Conferred CS Bachelor Degrees"),
             
             fluidRow(
               column(
                 width = 10,
                 plotlyOutput("cs_ba_map", height = "550px")
               ),
               column(
                 width = 2,
                 selectInput(
                   inputId = "year_choice",
                   label = "Select Year",
                   choices = sort(unique(cs_state_long$year)),
                   selected = max(cs_state_long$year)
                 ),
                 checkboxGroupInput(
                   inputId = "degree_gender_choice",
                   label = "Select Gender",
                   choices = c("male","female"),
                   selected = c("male","female")
               )
               )
             ),
             
             br(),
             p("Oregon relative to national averages.")
           ),
     #==============================================================#      
           "trends" = fluidPage(
             h2("National and Oregon Trends"),
             fluidRow(
               column(
                 width = 10,
                 plotlyOutput("cs_trend_plot", height = "550px")
               ),
               column(
                 width = 2,
                 selectInput(
                   inputId = "degree_choice",
                   label = "Select Degree Level",
                   choices = c(
                     "Bachelor's" = "bachelor",
                     "Master's" = "master",
                     "Doctor's" = "doctor"
                   ),
                   selected = "bachelor"
                 ),
                 checkboxGroupInput(
                   inputId = "trend_gender_choice",
                   label = "Select Gender",
                   choices = c("Male", "Female"),
                   selected = c("Male", "Female")
                 ),
                 br(),
                 p("This plot shows longitudinal trends of computer science degrees by gender across the U.S."),
                 p("The vertical dashed grey line indicates 2016, the year AP Computer Science Principles was introduced."),
                 p("The vertical dashed red line indicates 2020, the year COVID-19 started.")
               )
             )
           )
           )
  })

############################## ACCESS PLOT ##############################
  
  
  
############################## DEGREE PLOT ##############################
  
  cs_ba_national <- read_csv(here("data","cs_ba_us_12_22.csv"), skip = 3)
  
  national_clean <- cs_ba_national %>% 
    filter(!is.na(State), State != "Total") %>% 
    filter(!is.na(Total)) %>% 
    mutate(
      year = as.numeric(str_extract(`Completion Year`,"\\d{4}"))
    ) %>% 
    mutate(
      Total = as.numeric(Total),
      Male = as.numeric(Male),
      Female = as.numeric(Female)
    ) %>% 
    clean_names()
  
  cs_state_long <- national_clean %>% 
    pivot_longer(
      cols = c(total, male, female),
      names_to = "gender",
      values_to = "degrees") %>% 
    mutate(gender = factor(gender,
                           levels = c("total", "male","female"),
                           labels = c("total", "male", "female")))
  

  us_states <- states(cb = TRUE)
  
  filtered_state <- reactive({
    cs_state_long %>% 
      filter(year == as.numeric(input$year_choice),
             gender %in% input$degree_gender_choice) %>% 
      group_by(state) %>% 
      summarise(degrees = sum(degrees, na.rm = TRUE))
  })
  
  cs_map_data <- reactive({
    us_states %>% 
      left_join(filtered_state(), by = c("NAME" = "state"))
    
  })
  # degree(output)
  output$cs_ba_map <- renderPlotly({
    
    p2 <- ggplot(cs_map_data()) +
      geom_sf(aes(fill = degrees), color = "white") +
      scale_fill_viridis_c(option = "E", trans = "sqrt") +
      coord_sf(xlim = c(-125,-65), ylim = c(25, 50)) +
      theme_minimal() +
      theme(panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA)) +
      labs(fill = "CS BA Degrees",
           title = paste("CS Bachelor Degrees in", input$year_choice))
    
    ggplotly(p2)
  })
  

############################## TRENDS PLOT ##############################
  cs <- read.csv(here("data","cs_degree_clean.csv"))
  cs <- cs %>% 
    mutate(
      degree_level = factor(degree_level,
                            levels = c("bachelor","master","doctor")),
      gender = factor(gender,
                      levels = c("male","female"),
                      labels = c("Male","Female"))
    )
  
   filtered_cs <- reactive({

    cs %>%
      filter(degree_level == input$degree_choice) %>%
      filter(gender %in% input$trend_gender_choice)})

  # trends(output)

  output$cs_trend_plot <- renderPlotly({

    p1 <- ggplot(filtered_cs(),
                 aes(x = academic_year, y = value,color = gender)) +
      geom_line(size = 1) +
      geom_point(alpha = 0.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(1965, 2022, by = 4))+
      scale_color_manual(values = c("Male" = "#21908c", "Female" = "#440154")) +
      geom_vline(xintercept = 2016, linetype = "dashed", color = "grey70")+
      geom_vline(xintercept = 2020, linetype = "dashed", color = "red")+
      annotate("text", x = 2016, y = max(filtered_cs()$value),
               label = "AP Computer Science Principle introduced", 
               angle = 90, vjust = -0.5, color = "grey50")+
      labs(title = paste(
          "United States", tools::toTitleCase(input$degree_choice),
          "Degree Trends"
        ),
        x = "Academic Year",
        y = "Number of Degrees") +
      theme_minimal()

    ggplotly(p1)
  })
######################### TRENDS ENDS ###########################
  

  
}

##############################################################################
# RUN APP
shinyApp(ui = ui,server = server)
#rsconnect::deployApp()