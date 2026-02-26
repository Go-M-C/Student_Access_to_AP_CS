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

oregon_shape <- states(cb = TRUE, resolution = "20m") %>% 
  filter(NAME == "Oregon") %>% 
  st_transform(4326)

##########################DATA LOADING################################

part_202021 <- readRDS("data/or_apcs_202021.rds")
eco_202122 <- readRDS("data/or_cs_eco_2122.rds")
nat_deg <- readRDS("data/nat_degrees_long.rds")
nat_trend <- readRDS("data/nat_cs_trends.rds")


########################## UI DESIGN ##################################

ui <- page_navbar(
  
  title = div(
    style = "display: flex; flex-direction:column; justify-content:center; line-height:1.2;",
    span("DATA SCIENCE CAPSTONE PROJECT",
         style = "font-size: 20px; font-weight: 800; color: #180d00;letter-spacing:0.5px;"),
    span("Oregon Computer Science Education Overview",
         style = "font-size: 16px; font-weight: 400; color: #6c757d; margin-top: -2px;")
    ),
  
  
  theme = bs_theme(
    version = 5,
    primary = "#ab9f7a", 
    "nav-link-font-size" = "15px"),
  
  # WELCOME PAGE
  
  nav_panel("Welcome", icon = icon("house"),
            fluidPage(
            h2("Project Overview"),
            p("By 2026, the Computer Science for All initiative will reach its 
               10-year milestone since its launch in the United States. 
               The initiative marked a proactive move and a commitment 
               to expanding Computer Science (CS) education 
               from kindergarten through high school, 
               aiming to equip students with computational thinking (CT) skills 
               for participation in a fast-shifting and technology-driven economy."),
            br(),
            p("In 2022, Oregon Department of Education (ODE) 
               and the Higher Education Coordination Commission (HECC) 
               initiated a statewide implementation plan to expand access 
               to CS education for all public-school students by the 2027-2028 academic year. 
               For school district leaders and educators, this raises important questions:"),
            br(),
            p("-Who is currently participating in computer science courses?"),
            p("-Are participation patterns changing over time?"),
            p("-What does Oregon’s overall CS education ecosystem look like?"),
            br(),
            p("This interactive dashboard intends to provide a data-informed picture of 
               computer science participation and degree attainment in Oregon by bringing together 
               multiple public data source, including the Civil Rights Data Collection and 
               the National Center for Educational Statistics, and CODE.org."),
            br(),
            p("Use the sidebar to navigate through national trends,
               Oregon's landscape, and policy impact.")
            )
            ),#nav_panel("Welcome") ends
  
  # OREGON PARTICIPATION (2020-21)
  
  nav_panel("Participation", icon = icon("users"),
            fluidPage(
              tags$div(
                style = "background-color: #f8f9fa; padding: 15px;",
                
                # Georgraphic map
                card(
                  height = "75vh",
                  card_header("Geographic Distribution of AP CS Enrollment (2020-21)"),
                  card_body(
                    padding = 0,
                    plotlyOutput("participation_map", height = "100%")
                  )
                ),
                
                # Middle section: Filter
                card(
                  card_body(
                    layout_column_wrap(
                    width = 1,
                    selectInput("district_filter", 
                                "Filter by District:",
                                choices = c("ALL", sort(unique(part_202021$district_name))))
                  )
                )),
                # BOTTOM SECTION: Descriptive Statistics

                layout_column_wrap(
                  width = 1/2,
                  card(height = "500px",
                       card_header("Enrollment by Race/Ethnicity"),
                       plotlyOutput("race_bar_plot")),
                  
                  card(height = "500px",
                       card_header("Enrollment by Gender"),
                       plotlyOutput("gender_bar_plot"))
                )
                
              ) 
             
            )
            ), #nav_panel("Participation") ends
  
  
  # OREGON ECOSYSTEM (2021-22)
  nav_panel("Capacity", icon = icon("map"),
            layout_sidebar(
             sidebar = sidebar(
               title = "Geography Filters",
               checkboxGroupInput("locale_filter", "Locale Type:",
                                  choices = unique(eco_202122$locale),
                                  selected = unique(eco_202122$locale)),
             ) ,
             card(plotlyOutput("eco_map", height = "600px"))
            )
            
            ),#nav_panel(Capacity) ends
  
  # POST SECONDARY DEGREES
  
  nav_panel("CS Bachelor Degrees", icon = icon("graduation-cap"),
            layout_sidebar(
             sidebar = sidebar(
               selectInput("year_choice", "Year:", choices = sort(unique(nat_deg$year))),
               checkboxGroupInput("gender_choice", "Gender:", choices = c("male","female"), selected = c("male","female"))
             ) ,
             card(plotlyOutput("degree_map"))
            )
  ), #nav_panel(cs ba degrees ends)
  
  # NATIONAL TRENDS
  nav_panel("Trends", icon = icon("chart-line"),
            layout_sidebar(
              sidebar = sidebar(
                selectInput("level_choices", "Degree Level:", choices = c("bachelor", "master","doctor"))
              ),
              card(plotlyOutput("trend_plot"))
            )
    
  ),#nav_panel(trends end)
  
  nav_spacer(),
  nav_item(
    tags$a(
      href = "https://github.com/Go-M-C/Student_Access_to_AP_CS",
      target = "_blank",
      bs_icon("github", size = "1.5em", title = "GitHub"),
      style = "color: #180d00; padding-top: 10px;"
    )
  )
    ) # ui page_nav ends

#############################################################################
# SERVER

server <- function(input, output){
  

############################## PARTICIPATION PLOT #######################

  
  filtered_part_data <- reactive({
    
    part <- part_202021
    if (input$district_filter != "ALL") {
      part <- part %>% filter(district_name == input$district_filter)
    }
    part
  })
  
  output$participation_map <- renderPlotly({
    req(filtered_part_data())
    
    map_sf <- filtered_part_data() %>% 
      filter(!is.na(lat), !is.na(lon), cs_ap_total > 0) %>% 
      st_as_sf(coords = c("lon","lat"), crs = 4326)
    
    p_part <- ggplot()+
      geom_sf(data = oregon_shape, fill = "white", color = "#d3d3d3", size = 0.3) +
      geom_sf(data = map_sf,
              aes(size = cs_ap_total,
                  text = paste0("<b>", school_name, "</b><br>Enrollment ", cs_ap_total)),
              color = "#ab9f7a", 
              alpha = 0.6) +
      scale_size_continuous(range = c(2,12), name = "CS Enrollment") +
      theme_void() +
      theme(
        panel.background = element_rect(fill = "white", color = NA),
        plot.background = element_rect(fill = "white", color = NA)
      )
    
    ggplotly(p_part, tooltip = "text") %>% 
      layout(margin = list(l=0, r=0, t=0, b=0),
             yaxis = list(scaleanchor = "x", scaleratio = 1))
    
      })
  
############################# CAPACITY MAP #######################
  
  
  
  
  
  
############################## DEGREE PLOT ##############################
  
  
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
  
} #SERVER ENDS

##############################################################################
# RUN APP
shinyApp(ui = ui,server = server)
#rsconnect::deployApp()