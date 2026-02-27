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
              h3("Oregon Schools: Total Enrollment vs. AP CS Participation (2020-21)"),
              plotlyOutput("participation_map", height = "95vh"),
              
              br(),
              layout_column_wrap(
                width = 1/2,
                card(height = "450px",
                     card_header("Statewide Race/Ethnicity (AP CS)"),
                     plotlyOutput("race_bar_plot", height = "400px")),
                card(height = "450px",
                     card_header("Statewide Gender(AP CS)"),
                     plotlyOutput("gender_bar_plot", height ="400px"))
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
             ),
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

  
  output$participation_map <- renderPlotly({
    
    req(part_202021)
    
    map_data <- part_202021 %>% 
      filter(!is.na(lat), !is.na(lon)) %>% 
      mutate(
        cs_ap_total = if_else(is.na(cs_ap_total), 0, cs_ap_total),
        total_enrollment_202021 = as.numeric(total_enrollment_202021),
        status = if_else(cs_ap_total > 0, 
                         "AP CS Enrolled", 
                         "No AP CS Reported")
             )
    validate(need(nrow(map_data)>0, "No map data available"))
      
    map_sf <- st_as_sf(map_data, coords = c("lon","lat"), crs = 4326)
    
    p_part <- ggplot()+
      geom_sf(data = oregon_shape, fill = "white", color = "#d3d3d3", linewidth = 0.3) +
      geom_sf(data = map_sf,
              aes(size = total_enrollment_202021,
                  color = status,
                  text = paste0("<b>", school_name, "</b><br>",
                                "District: ", district_name, "<br>",
                                "Total Enrollment ", total_enrollment_202021,"<br>",
                                "AP CS Enrollment ", cs_ap_total)),
              alpha = 0.6) +
      scale_color_manual(values = c("AP CS Enrolled" = "pink", "No AP CS Reported" = "lightblue")) +
      scale_size_continuous(range = c(1,10)) +
      coord_sf(expand = FALSE)+
      theme_void() +
      theme(
        legend.position = "none")
    
    ggplotly(p_part, tooltip = "text") %>% 
      layout(
        autosize = TRUE,
        margin = list(l=0, r=0, t=10, b=50),
        annotates = list(
        x = 0, y = -0.05,
        text = "Pink: AP CS Enrolled | Blue: No AP CS Reported",
        showarrow = F, xref='paper', yref='paper',
        align = 'left',
        font = list(size = 2, color = "grey")
        ),
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