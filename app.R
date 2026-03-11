# For shiny app structure ui
library(shiny)
library(bslib)
library(bsicons)

# For data wrangling
library(dplyr)
library(ggplot2)
library(patchwork)
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
library(DT)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE) # save the shapefiles downloads locally
# library(base64enc)
# gif_base64 <- dataURI(file = here("www:", "cs_ba_anim.gif"), mime = "image/gif")

oregon_shape <- states(cb = TRUE, resolution = "20m") %>% 
  filter(NAME == "Oregon") %>% 
  st_transform(4326)

us_states <- states(cb = TRUE, resolution = "20m") %>% 
  shift_geometry()

##########################DATA LOADING################################

part_202021 <- readRDS("data/or_apcs_202021.rds")
eco_202122 <- readRDS("data/or_cs_eco_2122.rds")
eco_full_202122 <- readRDS("data/or_cs_eco_with_enroll_2122.rds")
nat_cs_ba <- readRDS("data/nat_cs_ba_final.rds")
nat_trend <- readRDS("data/nat_cs_trends.rds")
ap_cs_model <- readRDS("data/ap_cs_model.rds")
ap_locale_summary <- readRDS("data/ap_cs_locale_summary.rds")
or_eco_summary <- readRDS("data/or_eco_locale_summary.rds")
or_locale_summary <- readRDS("data/locale_summary.rds")

########################## UI DESIGN ##################################

ui <- page_navbar(
  
  id = "main_tabs",
  title = div(
    style = "display: flex; flex-direction:column; justify-content:center; line-height:1.2;",
    span("DATA SCIENCE CAPSTONE PROJECT",
         style = "font-size: 20px; font-weight: 800; color: #180d00;letter-spacing:0.5px;"),
    span("Oregon Computer Science Education Overview",
         style = "font-size: 16px; font-weight: 400; color: #6c757d; margin-top: -2px;")
    ),
  position = "fixed-top",
  
  header = tags$head(
    tags$style(HTML("
                    body{
                    padding-top: 100px;
                    }
                    h2{
                    padding-top:20px;
                    margin-top: 50px;
                    clear: both;
                    }
                    .navbar{
                    z-index: 2000;
                    background-color: white;
                    border-bottom: 2px solid #dee2e6;
                    box-shadow: 0 2px 4px rgba(0,0,0,.05);
                    min-height: 80px;
                    }
                    .container-fluid{
                    margin-top: 20px;
                    }
                    "))
  ),
  
  
  theme = bs_theme(
    version = 5,
    primary = "#ab9f7a", 
    "nav-link-font-size" = "15px"),
  
  # WELCOME PAGE
  
  nav_panel("Welcome", icon = icon("house"),
            fluidPage(
              div(
                style = "background-image: linear-gradient(150deg,#004d40 0%, #0072b2 80%);
                padding: 100px 0px 150px 0px;
                margin: 0px -40px 0px -40px;
                "
              ),# top,right,bottom,left
            h2("Project Overview", style = "text-align: left; margin-bottom: 30px;color:#ab9f7a"),
            p("This interactive dashboard intends to provide a data-informed picture of
               computer science education in Oregon."),
            p("By incorporating data from Civil Rights Data Collection(CRDC),
               the National Center for Educational Statistics(NCES), and CODE.org, the dashbord focuses on depict Oregon
              high school student's participation and access to computer science in the most recent year. 
              Also included is an overview of higher education attainment in CS and its national trends during the past 50 years."),
            p("This project is inspired by the Computer Science for All initiative that was launched 10 years ago in the United States. 
               The initiative marked a proactive move and a commitment 
               to expanding Computer Science (CS) education 
               from kindergarten through high school, 
               aiming to equip students with computational thinking (CT) skills 
               for participation in a fast-shifting and technology-driven economy."),
            p("In 2022, Oregon Department of Education (ODE) 
               and the Higher Education Coordination Commission (HECC) 
               initiated a statewide implementation plan to expand access 
               to CS education for all public-school students by the 2027-2028 academic year. 
               For school district leaders and educators, this raises important questions:"),
            tags$ul(
              style = "padding-left: 20px;font-size: 1.1rm; line-height:1.8;margin-bottom:30px;list-style-type: ' - '",
              tags$li(
                strong("Who is participating in advanced computer science courses?")
              ),
              tags$li(
                strong("Who is offering computer science courses?")
              ),
              tags$li(
                strong("What does higher education attainment look like in computer science?")
              )
            ),
            
            h3("How to Use the Dashboard", style = "text-align: left; margin-bottom: 30px;color:#ab9f7a"),
            
            p("The dashboard on each tab has multiple ways to filter what data is displayed. Feel free to 
              switch between categories by selecting or clicking on the provided interactive elements, 
              the dashboard will automatically update to refelct your selections."),
            
            h3("Explore the Insights", style = "text-align: left; margin-bottom: 30px;color:#ab9f7a"),
            
            layout_columns(
              col_widths = c(6,6),
              fill = FALSE,
              gap = "20px",
              
              card(
                card_image(file = "www:/classparticipation.png", 
                           style ="height: 250px; object-fit: cover; border-radius: 15px 15px 0 0;"),
                card_body(
                  style = "text-align: center;height: 80px;display:flex;align-items:center;
                  justify-content: center;",
                  actionLink("go_to_participation", "Advanced Placement CS Participation", 
                             style = "font-weight: bold; text-decoration: none;")
                )
              ),
              card(
                card_image(file = "www:/edtech tree.jpg", 
                           style ="height: 250px; object-fit: cover; border-radius: 15px 15px 0 0;"),
                card_body(
                  style = "text-align: center;",
                  actionLink("go_to_capacity", "Snapshot of Oregon CS course", 
                             style = "font-weight: bold; text-decoration: none;")
                )
              ),
              card(
                card_image(file = "www:/degree.jpg", 
                           style ="height: 250px; object-fit: cover; border-radius: 15px 15px 0 0;"),
                card_body(
                  style = "text-align: center;",
                  actionLink("go_to_degree", "Computer Science Degree Attainment", 
                             style = "font-weight: bold; text-decoration: none;")
                )
              ),
              card(
                card_image(file = "www:/Trends.png", 
                           style ="height: 250px; object-fit: cover; border-radius: 15px 15px 0 0;"),
                card_body(
                  style = "text-align: center;",
                  actionLink("go_to_trends", "Historical Trends", 
                             style = "font-weight: bold; text-decoration: none;")
                )
              ),
            ),
            h3("Data", style = "text-align: left; margin-bottom: 30px;color:#ab9f7a"),
            tags$ul(
              style = "padding-left:20px;font-size: 1.1rm; line-height:1.8;margin-bottom:30px;list-style-type: '  -  '",
              tags$li(
                p("ODE School level data 2020/21 - 2021/22"),
                p("Enrollment, demographics")
              ),
              tags$li(
                p("CRDC School level data 2020/21"),
                p("Advanced Placement CS enrollment, demographics, gender")
              ),
              tags$li(
                p("CODE.ORG School level data 2021/22"),
                p("Subcatory of CS courses, locale, school location")
              ),
              tags$li(
                p("NCES State level data 1964/65-2021/22"),
                p("Conferred number of degrees, state, degree level, gender, completion year")
              ),
              tags$li(
                p("NCES State level data 2002/03-2023/24"),
                p("Number of bachelor degees/certificates awarded, state, completion year")
              )
            )
            
            )),#nav_panel("Welcome") ends
  
  # OREGON PARTICIPATION (2020-21)
  
  nav_panel("AP CS Participation", icon = icon("users"),
            
            fluidPage(
              div(
                style = "background-image: linear-gradient(135deg,#004d40 0%, #0072b2 70%);
                padding: 100px 0px 150px 0px;
                margin: 0px -40px 0px -40px;"
              ),# top,right,bottom,left# top,right,bottom,left
              
              h2("Oregon Schools: Total Enrollment vs. AP CS Participation (2020-21)"),
              br(),
              h4("Introduction"),
              p("Advanced Placement (AP) courses are college-level classes offered in high schools, providing
              students the opportunity to earn college credit through structured coursework and standardized exam. 
              Though taking the AP courses is not a prerequisite for the exams,
                students are encouraged to take the AP course as a preparation for this high-stake exam. 
                This part of analyisis focus on AP Computer Science (AP CS) participation in Oregon."),
              h4("Data"),
              p("The final dataset is prepared through joining several dataset from the 2020-21 academic year: "), 
              p(" - Civil Rights Data Collection (CRDC) from the U.S. Department of Education provides student enrollment in AP CS, 
              disaggregated by race/ethnicity, and gender."),
              p(" - ODE Fall Membership Report from the Oregon Department of Education offers total school and enrollment to calculate access and participation rates."),
              p(" - Oregon CresMap from the CODE.org, provides geographic meta data and NCES locale classifications(City, Suburb, Rural, Town)."),
              p(" - This multi-source dataset allow us to examine equity gaps by the physical locations of the schools."),
              
              
              h4("AP CS Participation Map"),
              p("This interactive map visualizes the distribution of AP CS access across Oregon. Each circle represents a unique high school.
              The size of the circle correspons to the school's total enrollment, while the color indicates whether the school reported AP CS participation.
              Hover over any circle to view specific school name, district, and total student number."),
              plotlyOutput("participation_map", height = "65vh"),
              
              br(),
              h4("Findings"),
              layout_column_wrap(
                width = 1/2,
                card(height = "450px",
                     card_header("AP CS Participation by Locale"),
                     plotlyOutput("ap_locale_bar_plot")
                     ),
                
                card(height = "450px",
                     card_header("Summary Table: Access & Enrollment"),
                     DT::DTOutput("ap_locale_summary_table")
                     ),
              ),
              
              br(),
              layout_column_wrap(
                width = 1/2,
                card(height = "450px",
                     card_header("Statewide Race/Ethnicity (AP CS)"),
                     plotlyOutput("race_bar_plot", height = "400px")),
                card(height = "450px",
                     card_header("Statewide Gender(AP CS)"),
                     plotlyOutput("gender_bar_plot", height ="400px"))
              ),
              
              br(),
              
              tags$ul(
                style = "padding-left:20px;font-size: 1.1rm; line-height:1.8;
                margin-bottom:30px;list-style-type: '  -  '",
                tags$li(
                  p("Percentage of Students with AP CS by Locale"),
                  p("In the 2020-21 academic year, Oregon's high school landscape shows access divide in AP Computer Science availability. 
                  With the available data, we identified 329 traditional high schools with valid geographic data. The 'Access Rate'(the percentage
                  of schools offering the AP CS course) varies differently by locale: "),
                  p("Suburban area has the highest AP CS participation. Among the 47 suburban high schools identified, 17% offered AP CS.
                  When look at the rural area, among 131 high schools, only 2 schools reported AP CS enrollment. The access rate in this area is just 1.5%.
                    It is even interesting given that the average enrollment in the rural schools is 37, implying higher student teacher ratio
                    and needs for staffing supports"),
                  p("The highest access rate appears(18%) in the City area, but the average enrollment in AP CS course is considerably low(16.9) 
                    given the average enrollment in this area is as high as 1023. Town area also has a lower access rate(5.6%).")
                ),
                tags$li(
                  p("Percentage of Students with AP CS by race/ethnicity"),
                  p("Historically underrepresented students continue to face persistent opportunity gaps."),
                  p("The 2020-21 data reveals a large percentage of enrollment within specific demographics:"),
                  p("White students: 61.3% of all AP CS enrollments."),
                  p("Asian students: 17%"),
                  p("Hispanic students: 14%"),
                  p("Other Groups: Black, Native American, Hawaii and Pacific Islander, and Multi-racial students representing
                    the remaining share.")),
                  br(),
                tags$li(
                  p("Gender disparity remains one of the most visible gaps in this dataset."),
                  p("Female students: 158(approximately 25%"),
                  p("Male students: 459(approximately 75%)")
                )
              
            ),
            h4("Sources"),
            p("U.S. Department of Education, Office for Civil Rights, 2020-21 Civil Rights Data Collection (CRDC) Reports."),
            p("Oregon Department of Education, 2020-21 Student Membership Reports"),
            p("CODE.org, 2021-22 Oregon Course Mapping")
            
              )), #nav_panel("Participation") ends
  
  
  # OREGON ECOSYSTEM (2021-22)
  nav_panel("Beyond AP CS", icon = icon("map"),
           
            fluidPage(
              
              div(
                style = "background-image: linear-gradient(135deg,#004d40 0%, #0072b2 70%);
                padding: 100px 0px 150px 0px;
                margin: 0px -40px 0px -40px;"
              ),# top,right,bottom,left
              
              h2("Oregon Schools: The Variety and Enrollment of Computer Science Course (2021-22)"),
              br(),
              h4("Introduction"),
              p("While AP CS courses(2020-21) reflects participation in high-level academic tracks, this 2021-22 data shows the state's landscape of CS variety in Oregon.
              It represents the diverse pathways available to Oregon Students and school-level initiative to offer CS beyond traditional learning. These offerings include
              Web Development, Cybersecurity, Information System, Information Technology, Foundational CS, and Core CS."),
              h4("Data"),
              p("The final dataset is prepared through joining several dataset from the 2021-22 academic year: "),
              p(" - ODE Fall Membership Report from the Oregon Department of Education offers total school and enrollment to calculate access and participation rates."),
              p(" - Oregon CresMap from the CODE.org, provides geographic meta data and NCES locale classifications(City, Suburb, Rural, Town)."),
              
              
              h4("Oregon High School CS Course Map"),
              p("This interactive map visualizes the variety of CS course offered across Oregon. 
                Each circle represents a unique high school. The size of the circle correspons to the relative number of CS course count, 
                while the color indicates which course is offered. Hover over any circle to view specific school name, locale, and total CS course.
                The map will automatically update to reflect your selections from the side bar."),
              layout_sidebar(
                sidebar = sidebar(
                  selectInput("eco_locale_filter", "Filter by Locale: ",
                              choices = c("All", sort(unique(eco_full_202122$locale)))),
                  selectInput("eco_course_filter", "Select CS Course Categories: ",
                              choices = c("All", sort(unique(eco_full_202122$subcategory))),
                              selected = "All"
                              )
              ),
              plotlyOutput("eco_map", height = "85vh")
            ),
            
            br(),
            h4("Findings"),
            layout_column_wrap(
              width = 1/2,
              card(
                height = "450px",
                card_header("CS Course Diversity by Locale"),
                plotlyOutput("eco_locale_bar_plot")
              ),
              card(
                height = "450px",
                card_header("CS Course Access Rate by Locale"),
                DT::DTOutput("eco_summary_table")
              ),
            ),
            
            br(),
            layout_column_wrap(
              width = 1/2,
              card(
                height = "450px",
                card_header("Oregon High School CS Course Distribution"),
                plotlyOutput("eco_subcategory_bar_chart", height = "400px")
              ),
              card(
                height = "450px",
                card_header("Oregon High School Demographics"),
                plotOutput("p_demo", height = "400px")
              )
              ),
            
            tags$ul(
              style = "padding-left:20px;font-size: 1.1rm; line-height:1.8;
                margin-bottom:30px;list-style-type: '  -  '",
              tags$li("City: Schools in this area serve massive populations. On average, each school has 1,043 students. 
                      Given the large enrollment size, there is only 0.42 courses exist per 100 students,
                      the 4.35 variety means students can choose a more specialized pathway that fits their needs."),
              tags$li("Rural: Schools in this area are generally small. 
                      Their access rate is the highest (1.06). The reason could be that one course in a tiny school creates a high ratio. 
                      However, the 2.1 variety score suggests there are not many choices for a student in this area."),
              tags$li("White the variety of offerings is encouraging. 
              It is critical for researchers and policy makers to understand the curriculum and learning objectives covered in these CS courses.
                      It is an essential move to ensure students are gaining the high-level computational thinkings skills training.")
            ),
            br(),
            h4("Sources"),
            p("Oregon Department of Education, 2021-22 Student Membership Reports"),
            p("CODE.org, 2021-22 Oregon Course Mapping")
            )
            
            ),#nav_panel(Capacity) ends
  
  # POST SECONDARY DEGREES
  
  nav_panel("CS Degrees", icon = icon("graduation-cap"),
            
            fluidPage(
              
              div(
                style = "background-image: linear-gradient(135deg,#004d40 0%, #0072b2 70%);
                padding: 100px 0px 150px 0px;
                margin: 0px -40px 0px -40px;"
              ),# top,right,bottom,left
              
              h2("U.S. Computer Science Degrees (2003-24)"),
              br(),
              h4("Introduction"),
              p("This section presents the conferred bachelor degrees in computer science 
                degree. By tracking data from 2003 to 2024, this tab provides visualization of how each states responding to the surging needs for talents with CS degrees.
                It also offers an overview of where Oregon posits among the rest of states."),
              h4("Data"),
              p("U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS)"),
              p(" - Number of degrees/certificates awarded at postsecondary institutions, by state and gender, CIP Code 11"),
              p(" - Number of degrees/certificates awarded at postsecondary institutions, by state and CIP code"),
              br(),
              
              h4("CS Degrees State Map"),
              p("This interactive map visualizes the changing proportion of Computer Science among all bachelor degress over 21 years.
              It allows for a directcomparison of Oregon's degree output against national average.
              Hover over any state to view state name and the percentage of CS bachelor degrees.
                The map will automatically update to reflect your selections of year and metrics from the side bar."),
              layout_columns(
                col_widths = c(7,5),
                card(
                  card_header("Computer Sciences Degrees Awarded by states(%)"),
                  plotlyOutput("degree_map", height = "500px")
                ),
                card(
                  card_header("Oregon and Other States vs. the National Average(%)"),
                  plotOutput("trend_line_plot", height = "500px")
                )),
              br(),
              card(
                card_body(
                  layout_columns(
                    col_widths = c(4,8),
                    selectInput("map_metric", "Select Metric: ",
                                choices = c("Percentage of CS of All Degrees" = "cs_percent",
                                            "Percentage of Female Students" = "cs_female_percent"
                                            )),
                    sliderInput("map_year", "Select Year: ",
                                min = 2003, max = 2024, value = 2024,
                                sep = "", animate = TRUE) 
                    )
                  )
                ),
              br(),
              card(
                card_header("Details of State by State CS Bachelor Degrees"),
                DT::DTOutput("state_data_table")
              ),
              br(),
              h5("In this table, likelyhood means how much times male students are
                           as likely to obtain a CS degree, 
                           compared with female students"),
              br(),
              h4("Source"),
              p(" - U.S. Department of Education, National Center for Education Statistics, Integrated Postsecondary Education Data System (IPEDS), 
                Completions component final data (2001-02 - 2022-23) and provisional data (2023-24).")
                )
              
  ), #nav_panel(cs ba degrees ends)
  
  # NATIONAL TRENDS
  nav_panel("Trends", icon = icon("chart-line"),
            
            fluidPage(
              
              div(
                style = "background-image: linear-gradient(135deg,#004d40 0%, #0072b2 70%);
                padding: 100px 0px 150px 0px;
                margin: 0px -40px 0px -40px;"
              ),# top,right,bottom,left
              
              h2("U.S. Computer Science Degrees Trends(1965-24)"),
              br(),
              h4("Introduction"),
              p("This part of the project uses longitudinal data that record the changing pattern 
                of computer science by different degree levels"),
              br(),
              h4("Data"),
              p("Degrees in computer and information sciences conferred by postsecondary institutions, by level of degree and sex of 
              student: Academic years 1964-65 through 2021-22"),
              p("Level's of Degrees, gender"),
              
              layout_sidebar(
                sidebar = sidebar(
                selectInput("level_choices", "Degree Level:", 
                            choices = c("Bachelor's" = "bachelor", 
                                        "Master's" = "master",
                                        "Doctoral" = "doctor")),
                selectInput("gender_choices", "Gender:",
                            choices = c("All","Male", "Female"),
                            selected = "All")
              ),
              card(plotlyOutput("cs_trend_plot"))
            ),
            card(
              card_header("Details of degrees in CS conferred by postsecondary institutions(1965-2022)"),
              DT::DTOutput("trend_data_table")
            )
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

server <- function(input, output, session){
  
  observeEvent(input$go_to_participation, {
    updateNavbarPage(session, "main_tabs", selected = "AP CS Participation")
  })
  
  observeEvent(input$go_to_capacity, {
    updateNavbarPage(session, "main_tabs", selected = "Beyond AP CS")
  })
  
  observeEvent(input$go_to_degree,{
    updateNavbarPage(session, "main_tabs", selected = "CS Degrees")
  })
  
  observeEvent(input$go_to_trends,{
    updateNavbarPage(session, "main_tabs", selected = "Trends")
  })

############################## PARTICIPATION PLOT #######################

  # map
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
      scale_color_manual(values = c("No AP CS Reported" = "#40b0a6", "AP CS Enrolled" = "#E1BE6A")) +
      scale_size_continuous(range = c(1,10)) +
      coord_sf(expand = FALSE)+
      theme_void() +
      theme(
        legend.position = "none")
    
    ggplotly(p_part, tooltip = "text") %>% 
      layout(
        autosize = TRUE,
        margin = list(l=0, r=0, t=10, b=50),
        annotations = list(
        x = 0, y = -0.05,
        text = "Yellow: AP CS Enrolled | Blue: No AP CS Reported",
        showarrow = F, xref='paper', yref='paper',
        align = 'left',
        font = list(size = 12, color = "grey")
        ),
        yaxis = list(scaleanchor = "x", scaleratio = 1))
      })
  
  # Locale chart
  output$ap_locale_bar_plot <- renderPlotly({
  locale_plot_data <- ap_cs_model %>% 
    group_by(locale) %>% 
    summarise(total_cs_students = sum(cs_ap_total, na.rm = TRUE)) %>% 
    arrange(desc(total_cs_students))
  
  locale_chart <- ggplot(locale_plot_data, aes(x = locale, y = total_cs_students,
                                               fill = locale)) +
    geom_col(aes(text = paste0("Locale: ", locale, "<br>Total Students:", total_cs_students))) +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal() +
    labs(x = NULL, y = "Total AP CS Students Enrolled(2020-21)") +
    theme(legend.position = "none")
  
  ggplotly(locale_chart, tooltip = "text")
  
  })# Locale chart ends
  
  
  
  # Locale table
  output$ap_locale_summary_table <- DT::renderDT({
    datatable(ap_locale_summary,
              colnames = c("Locale","Total High Schools",
                           "Offer CS", "Access Rate",
                           "Avg Size", "Avg Enroll in AP CS Offering Schools"),
              options = list(
                dom = 't',
                pageLength = 6,
                scrollX = TRUE
              ),
              rownames = FALSE) %>% 
    formatStyle('access_rate',
                color = "white",
                backgroundColor = styleInterval(c(20,50), c("#d9534f","#f0ad4e","#5cb85c")))
    
    
    
    
    
  })# Locale table ends
    
    
  
  # Race plot
  output$race_bar_plot <- renderPlotly({
    
    race_summary <- part_202021 %>% 
      filter(cs_ap_total > 0) %>% 
      summarise(
        Hispanic = sum(cs_ap_count_hispanic, na.rm = TRUE),
        Black = sum(cs_ap_count_black, na.rm = TRUE),
        Asian = sum(cs_ap_count_asian, na.rm = TRUE),
        White = sum(cs_ap_count_white, na.rm = TRUE),
        Native = sum(cs_ap_count_american_indian, na.rm = TRUE),
        Multi = sum(cs_ap_count_multi_racial, na.rm = TRUE),
        Ha_pac_islander = sum(cs_ap_count_native_ha_pa_islander, na.rm = TRUE),
        Total = sum(cs_ap_total, na.rm = TRUE)
      ) %>% 
      pivot_longer(cols = -Total, names_to = "Race", values_to = "Count") %>% 
      mutate(Percent = (Count/Total)*100)
    
    race_chart <- ggplot(race_summary, aes(x = Race, y = Percent, fill = Race)) +
      geom_col(aes(text = paste0("Group: ", Race,
                                 "<br>Count: ", Count,
                                 "<br>Percent:", round(Percent, 1), "%")))+
      scale_fill_brewer(palette = "BrBG")+
      theme_minimal()+
      labs(x = NULL, y = "Statewide CS AP Course Enrollment Ratio by Race (%)") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
      
  
  }) # Race plot ends
  
  # Gender plot
  
  output$gender_bar_plot <- renderPlotly({
    
    gender_summary <- part_202021 %>% 
      filter(cs_ap_total > 0, na.rm = TRUE) %>% 
      summarise(
        Male = sum(cs_ap_count_male, na.rm = TRUE),
        Female = sum(cs_ap_count_female, na.rm = TRUE),
        Total = sum(cs_ap_total, na.rm = TRUE)
      ) %>% 
      pivot_longer(cols = -Total, names_to = "Gender", values_to = "Count") %>% 
      mutate(Percent = (Count/Total)*100)
    
    gender_chart <- ggplot(gender_summary, aes(x = Gender, y = Percent, fill = Gender)) +
      geom_col(aes(text = paste0("Group: ", Gender,
                                 "<br>Count: ", Count,
                                 "<br>Percent:", round(Percent, 1), "%"))
      ) +
      scale_fill_brewer() +
      theme_minimal() +
      labs(x = NULL, y = "Statewide CS AP Course Enrollment Ratio by Gender (%)") +
      theme(legend.position = "none")
    
    
  })# Gender plot ends
    
  
############################# BEYOND CS AP TAB #######################
  
  all_subcategories <- sort(unique(eco_full_202122$subcategory))
  
  output$eco_map <- renderPlotly({
    
    eco_map_data <- eco_full_202122 %>% 
      filter(number_of_courses > 0) %>% 
      mutate(subcategory = factor(subcategory, levels = all_subcategories)) %>% 
      filter(subcategory %in% input$eco_course_filter | input$eco_course_filter == "All") %>% 
      filter(!is.na(latcod), !is.na(loncod))
    
    if(input$eco_course_filter != "All") {
      eco_map_data <- eco_map_data %>% 
        filter(subcategory == input$eco_course_filter)
    }
    
    if (input$eco_locale_filter != "All") {
      eco_map_data <- eco_map_data %>% 
        filter(locale == input$eco_locale_filter)
    }
    
    p_eco <- ggplot() +
      geom_sf(data = oregon_shape, fill = "white", color = "black") +
      geom_point(data = eco_map_data,
                 aes(x = loncod, y = latcod,
                     size = number_of_courses,
                     color = subcategory,
                     text = paste0("<b>", school_name, "</b><br>",
                                   "Locale: ", locale, "<br>",
                                   "Course Category: ", subcategory,"<br>",
                                   "Course Offered: ", number_of_courses)),
                 alpha = 0.3)+
      scale_size_continuous(range = c(3,12), name = "Number of Courses") +
      scale_color_viridis_d(option = "D", drop = FALSE) +
      theme_void()+
      theme(legend.position = "none")
    
    ggplotly(p_eco, tooltip = "text") %>% 
      layout(margin = list(l=0, r=0, t=0, b=0))
  })
  
  # ECO map ends
  
  # ECO course by locale
  
  output$eco_locale_bar_plot <- renderPlotly({
    
    locale_course <- eco_full_202122 %>% 
      group_by(locale) %>% 
      summarise(Total_Offerings = sum(number_of_courses, na.rm = TRUE),
                Avg_Variety = round(sum(number_of_courses)/n_distinct(school_match), 2),
                .groups = "drop") %>% 
      pivot_longer(cols = c(Total_Offerings, Avg_Variety),
                   names_to = "Metric",
                   values_to = "Value")
    
    p_locale_bar <- ggplot(locale_course, aes(x = locale,y = Value,fill = Metric))+
      geom_col(position = "dodge") +
      facet_wrap(~Metric, scales = "free_y")+
      scale_fill_brewer(palette = "Set2") +
      theme_minimal() +
      labs(x = NULL, y = "Count/Average Variety") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p_locale_bar)
  })
  
  # ECO course by locale ends
  
  # ECO summary table
  
  output$eco_summary_table <- DT::renderDT({
    datatable(
      or_eco_summary,
      colnames = c("Locale", "Total Schools", "Number of Schools with CS",
                   "9-12 Enrollment", "Avg School Size", "Total Offerings",
                   "CS Access Rate (per 100 students)", "Avg Categories"),
      rownames = FALSE,
      options = list(
        dom = "t", #no filter boxes,
        pageLength = 10,
        order = list(list(4,'desc')),#sort by avg variety
        columnDefs = list(list(className = 'dt-center', targets = "_all"))      
        ),
      caption = "Table: Comparison of CS Capacity and Density by Locale (2021-22)"
    )
    
  }) # ECO summary table ends
  
  
  # ECO subcategory chart
  output$eco_subcategory_bar_chart <- renderPlotly({
    subcategory_counts <- eco_full_202122 %>% 
      filter(number_of_courses > 0) %>% 
      group_by(subcategory) %>% 
      summarise(school_count = n_distinct(school_match)) %>% 
      arrange(desc(school_count))
    
    p_subcat_bar <- ggplot(subcategory_counts, 
                          aes(x = reorder(subcategory, -school_count), 
                           y = school_count, 
                           fill = subcategory,
                           text = paste0("Category: ", subcategory, "<br>Schools: ", school_count)))+
      geom_col() +
      scale_fill_brewer(palette = "Set3") +
      theme_minimal() +
      labs(x = "Course Category", y = "Number of Schools") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
    
    ggplotly(p_subcat_bar, tooltip = "text")
    
  })
  
  # ECO subcategory chart ends
  
  # ECO Combined locale plot
  output$p_demo <- renderPlot({
    
    locale_long <- or_locale_summary %>% 
      pivot_longer(
        cols = c(Hispanic, Asian,White, Black, Native_Ha_Pa_Islander,American_Indian, Multi),
        names_to = "Race",
        values_to = "Student_Count"
      )
    
    p_demo <- ggplot(locale_long, aes(x = locale, y = Student_Count, fill = Race)) +
      geom_bar(stat = "identity", position = "fill")+
      scale_y_continuous(labels = percent_format()) +
      scale_fill_brewer(palette = "Set3", name = "Race/Ethnicity")+
      labs(
        title = "Oregon High School Demographics",
        x = NULL,
        y = "Demographic (%)") +
      theme_minimal()
    
    p_demo
  })
  
  # ECO Combined locale plot ends
############################## DEGREE TAB ##############################
  
  # Degree Map
  
  legend_title <- reactive({
    if (input$map_metric == "cs_percent") return ("CS Percentage of Degrees")
    if (input$map_metric == "cs_female_percent") return ("Percentage of Female Students")
  })
  
  output$degree_map <- renderPlotly({
    
    val_min <- min(nat_cs_ba[[input$map_metric]], na.rm = TRUE)
    val_max <- max(nat_cs_ba[[input$map_metric]], na.rm = TRUE)
  
   
    cs_ba_data <- us_states %>% 
      left_join(nat_cs_ba %>% filter(year == input$map_year),
                by = c("NAME" = "state"))
    
    p_degree_map <- ggplot(cs_ba_data) +
      geom_sf(aes(fill = .data[[input$map_metric]],
                  text = paste0("<b>", NAME, "</b><br>",
                                input$map_metric, ": ",
                                round(.data[[input$map_metric]], 2))),
              color = "white", size = 0.1) +
      scale_fill_viridis_c(name = legend_title(), option = "inferno", limits = c(val_min, val_max)) +
      theme_void()
    
    ggplotly(p_degree_map)
    
  })
 # Degree Map ends
  
  # Degree line chart
  
  output$trend_line_plot <- renderPlot({
    
    national_avg <- nat_cs_ba %>% 
      group_by(year) %>% 
      summarise(avg_val = mean(.data[[input$map_metric]], na.rm = TRUE))
    
    p_ba_trend <- ggplot() +
      geom_line(data = nat_cs_ba %>% filter(state != "Oregon"),
                aes(x = year, y = .data[[input$map_metric]], group = state),
                color = "grey75", alpha = 0.5, size = 0.5) +
      geom_line(data = national_avg,
                aes(x = year, y = avg_val, group = 1),
                color = "#5d3a9b", linetype = "dashed", size = 1.2) +
      geom_line(data = nat_cs_ba %>% filter(state == "Oregon"),
                aes(x = year, y = .data[[input$map_metric]]),
                color = "#E66100")+
      labs(title = "Oregon's Trend in Awarding CS Bachelor Degree(2003-24)",
           subtitle = "Bold line = Oregon | Dashed line = National Avg | Grey line = Other States",
           x = "Year", y = "Percentage") +
      theme_minimal()
    
    p_ba_trend
    
  })
  
  
  # Degree line chart ends
  
  
  # Degree interactive table
  
  output$state_data_table <-  DT::renderDT({
    
    nat_cs_ba_table <- nat_cs_ba %>% 
      select(year, state, total_all, total_cs, cs_percent,male,
             cs_male_percent,female,cs_female_percent,cs_m_f_ratio) %>% 
      mutate(
        year = as.factor(year),
        state = as.factor(state),
        across(where(is.numeric), ~round(., 2)),
        cs_m_f_ratio = paste0(round(cs_m_f_ratio, 1), "x")
      )
    
    display_names <- c(
      "Year" = "year", 
      "State" = "state",
      "Total BA Degrees Awarded" = "total_all",
      "Total CS BA Degrees Awarded" = "total_cs", 
      "CS BA(%)" = "cs_percent",
      "CS_Male" = "male",
      "CS_Male(%)" = "cs_male_percent",
      "CS_Female" = "female",
      "CS_Female(%)" = "cs_female_percent",
      "Likelihood" = "cs_m_f_ratio"
    )
    
    datatable(nat_cs_ba_table,
              colnames = display_names,
              class = "display",
              style = "bootstrap4",
              rownames = FALSE,
              options = list(scrollX = TRUE, autoWidth = TRUE),
              caption = htmltools::tags$caption(
                style = 'caption-side:bottom;text-align:left;',
                'Source: IPEDS Completions data (2002-2024)'
              )
    )
    
  })
  
  # Degree interactive table

############################## TRENDS TAB ##############################
  
   filtered_cs <- reactive({

     req(input$level_choices, input$gender_choices)
     
     trend_data <- nat_trend
       
         trend_data <- trend_data %>% 
           filter(degree_level == input$level_choices)

       if (input$gender_choices != "All") {
         trend_data <- trend_data %>%
           filter(gender == input$gender_choices)
       }
       
     return(trend_data)

  })# trends(output)

  output$cs_trend_plot <- renderPlotly({
    

    p_trend <- ggplot(filtered_cs(),
                 aes(x = academic_year, y = value,
                     color = gender, group = gender)) +
      geom_line(linewidth = 1) +
      geom_point(alpha = 0.5) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_continuous(breaks = seq(1965, 2022, by = 5))+
      scale_color_manual(values = c("Male" = "#21908c", "Female" = "#440154")) +
      geom_vline(xintercept = 2016, linetype = "dashed", color = "grey70")+
      geom_vline(xintercept = 2020, linetype = "dashed", color = "red")+
      labs(title = paste(
          "United States", tools::toTitleCase(input$level_choices),
          "CS Degree Trends"
        ),
        x = "Academic Year",
        y = "Number of Degrees",
        color = "Gender") +
      theme_minimal()

    ggplotly(p_trend)
  })
  
  output$trend_data_table <- renderDT({
    
    datatable(nat_trend,
              colnames = c("Year", "Degree Level","Gender","Degree conferred"),
              class = "display",
              style = "bootstrap4",
              rownames = FALSE,
              options = list(pageLength = 10, autoWidth = TRUE),
              caption = htmltools::tags$caption(
                style = 'caption-side:bottom;text-align:left;',
                'Source: U.S. Department of Education, National Center for Education Statistics, 
                Earned Degrees Conferred, 1964-65 through 1969-70; 
                Higher Education General Information Survey (HEGIS), 
                "Degrees and Other Formal Awards Conferred" surveys, 
                1970-71 through 1985-86; Integrated Postsecondary Education Data System (IPEDS), 
                "Completions Survey" (IPEDS-C:87-99); Completions component, 
                IPEDS Fall 2000 through Fall 2021 (final data) and Fall 2022 (provisional data).  
                (This table was prepared November 2023.)'
              ))
  })
  
} #SERVER ENDS

##############################################################################
# RUN APP
shinyApp(ui = ui,server = server)
#rsconnect::deployApp()