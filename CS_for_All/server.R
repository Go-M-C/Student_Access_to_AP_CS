
library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(here)

server <- function(input, output, session){

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
