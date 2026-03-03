library(tidyr)
library(here)
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(janitor)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE) 

cs_ba_national <- read_csv(here("data","cs_ba_us_12_22.csv"), skip = 3)
all_ba_national <- read_excel(here("data", "all_ba_us.xlsx"), skip = 4)

cs_ba_clean <- cs_ba_national %>% 
  filter(!State %in% c("Total", "United States", "US Total","National")) %>% 
  filter(!is.na(Total)) %>% 
  mutate(
    year = as.numeric(str_extract(`Completion Year`,"\\d{2,4}$")),
    year = ifelse(year <100, 2000+year, year),
    across(c(Total, Male, Female), as.numeric)) %>% 
  select(everything(), Total_cs = Total) %>% 
  clean_names()

cs_ba_ratio <- cs_ba_clean %>% 
  mutate(
    cs_female_percent = round((female/total_cs) * 100,digits = 2),
    cs_male_percent = round((male/total_cs)*100,digits = 2),
    m_f_ratio = round(male/female, digits = 2)
  ) %>% 
  select(completion_year,year, everything())

all_ba_clean <- all_ba_national %>% 
  clean_names() %>%
  filter(!state %in% c("Total", "United States", "US Total", "National")) %>% 
  filter(!is.na(total)) %>% 
  mutate(
    year = as.numeric(str_extract(completion_year,"\\d{2,4}$")),
    year = ifelse(year <100, 2000+year, year)) %>% 
  select(year, completion_year, state, total_all = total, everything(), - undesignated_field_of_study)

all_ba_enroll <- all_ba_clean %>% 
  select(year, state,total_all)

cs_ba_final <- cs_ba_ratio %>% 
  inner_join(all_ba_enroll, by = c("year", "state")) %>% 
  select(completion_year,year, state,total_cs,total_all, everything(), cs_m_f_ratio = m_f_ratio)



selected_year <- 2017


us_states <- states(cb = TRUE)

selected_year <- 2017
selected_gender <- "Female"

cs_map_data <- us_states %>% 
  left_join(
    national_ratio %>% 
      filter(year == selected_year,
             gender == selected_gender) %>% 
      select(State,degrees),
    by = c("NAME" = "State")
  )


cs_map_data %>%
  ggplot() +
  geom_sf(aes(fill = degrees), color = "white") +
  scale_fill_viridis_c(option = "C", na.value = "grey90")+
  coord_sf(xlim = c(-125, -65), ylim = c(25, 50)) +
  theme_minimal() +
  labs(title = paste("CS Bachelor's Degrees in", selected_year),
       size = "Degrees")



# ## Verify sums
# # national_clean %>%
# #   mutate(check = Male + Female - Total) %>%
# #   summarise(max_diff = max(abs(check), na.rm = TRUE))
# 
# ## Verify state number
# # national_clean %>% 
# #   group_by(year) %>% 
# #   summarise(n_states = n_distinct(State)) %>% 
# #   arrange(year)
# 
# us_total <- national_clean %>% 
#   group_by(year) %>% 
#   summarise(
#     Total = sum(Total),
#     Male = sum(Male),
#     Female = sum(Female),
#     State = "United States"
#   )
# 
# 
# national_clean <- bind_rows(national_clean, us_total)
# 
# cs_latest <- national_clean %>% 
#   filter(year == 2022) %>% 
#   mutate(State = tolower(State)) # match with map data
# 
# # Get U.S. map data
# us_states <- map_data("state")
# 
# cs_map <- us_states %>% 
#   left_join(cs_latest, by = c("region" = "State"))
# 
# p2 <- ggplot(cs_map, aes(x = long, y = lat, group = group, fill = Total)) +
#   geom_polygon(color = "white") +
#   scale_fill_viridis_c(option = "plasma", na.value = "grey70") +
#   coord_fixed(1.3) +
#   labs(
#     title = "CS Bachelor's Degrees by State (2022)",
#     fill = "Number of Awarded Degrees",
#     caption = "SOURCE: U.S. Department of Education, 
#     National Center for Education Statistics, 
#     Integrated Postsecondary Education Data System (IPEDS), 
#     Completions component final data (2001-02 - 2022-23) 
#     and provisional data (2023-24)."
#   ) +
#   theme_minimal()
#   
# 
# p2




###########################################################################

# Data clean and wrangling to build a clean longitudinal state-level dataset
# of CS bachelor's degrees from 2012-13 through 2021-22. 

## Import NCES data for 
## conferred bachelor degree by postsecondary institutions

# cs_ba_2012 <- read_excel(here("data","conferred degree_national_12_13.xls"), 
#                       skip = 1, 
#                       col_names = TRUE) %>% 
#   clean_names() %>% 
#   
#   filter(!is.na(state_or_jurisdiction),
#          str_detect(state_or_jurisdiction, "[A-Za-z]"),
#          !str_detect(state_or_jurisdiction, 
#                      regex("United States|Other jurisdictions", 
#                            ignore_case = TRUE)),
#          !is.na(computer_sciences)
#          ) %>% 
# 
#   select(state_or_jurisdiction, computer_sciences) %>% 
#   rename(
#     state = state_or_jurisdiction,
#     cs_ba_degrees = computer_sciences
#   ) %>% 
#   mutate(
#     cs_ba_degrees = as.numeric(str_remove_all(cs_ba_degrees, ",")),
#     academic_year = "2012-13",
#     state = str_replace(state, "[^A-Za-z]+$",""),# remove any non-letter cha at end
#     state = str_squish(state) # remove extra space
#   )
   
## A function to clean the rest NCES data

# clean_cs_data <- function(path, year_label, skip_rows = 1) {
#   
#   df <- read_excel(path, skip = skip_rows) %>% 
#     clean_names()
#   
#   cs_col <- intersect(c("computer_sciences",
#                         "computer_and_information_sciences_and_support_services"), names(df))
#   
#   if (length(cs_col) == 0){
#     stop("No CS column found in this file.")
#   }
#   
#   cs_col <- cs_col[1]
#   
#   df_clean <- df %>% 
#     filter(
#       !is.na(state_or_jurisdiction),
#       str_detect(state_or_jurisdiction, "[A-Za-z]"),
#       !str_detect(state_or_jurisdiction, 
#                   regex("United States|Other jurisdictions", 
#                         ignore_case = TRUE)),
#       !is.na(.data[[cs_col]])
#     ) %>% 
#     select(state_or_jurisdiction, all_of(cs_col)) %>% 
#     rename(
#       state = state_or_jurisdiction,
#       cs_ba_degrees = all_of(cs_col)
#     ) %>% 
#     mutate(
#       cs_ba_degrees = as.numeric(str_remove_all(cs_ba_degrees, ",")),
#       academic_year = year_label,
#       state = str_replace(state,"[^A-Za-z]+$", ""),
#       state = str_squish(state)
#     )
#   return(df_clean)
# } 
#   
# 
# ## data clean
# cs_ba_2012 <- clean_cs_data(
#   path = here("data","conferred degree_national_12_13.xls"),
#   year_label = "2012-13",
#   skip_rows = 1
# )
# 
# 
# cs_ba_2013 <- clean_cs_data(
#   path = here("data","conferred degree_national_13_14.xls"),
#   year_label = "2013-14",
#   skip_rows = 1
# )
# 
# 
# cs_ba_2014 <- clean_cs_data(
#   path = here("data","conferred degree_national_14_15.xls"),
#   year_label = "2014-15",
#   skip_rows = 1
# )
# 
# cs_ba_2015 <- clean_cs_data(
#   path = here("data","conferred degree_national_15_16.xls"),
#   year_label = "2015-16",
#   skip_rows = 1
# )
# 
# cs_ba_2016 <- clean_cs_data(
#   path = here("data","conferred degree_national_16_17.xls"),
#   year_label = "2016-17",
#   skip_rows = 1
# )
# 
# cs_ba_2017 <- clean_cs_data(
#   path = here("data","conferred degree_national_17_18.xls"),
#   year_label = "2017-18",
#   skip_rows = 1
# )
# 
# cs_ba_2018 <- clean_cs_data(
#   path = here("data","conferred degree_national_18_19.xls"),
#   year_label = "2018-19",
#   skip_rows = 1
# )
# 
# 
# cs_ba_2019 <- clean_cs_data(
#   path = here("data","conferred degree_national_19_20.xls"),
#   year_label = "2019-20",
#   skip_rows = 1
# )
# 
# cs_ba_2020 <- clean_cs_data(
#   path = here("data","conferred degree_national_20_21.xlsx"),
#   year_label = "2020-21",
#   skip_rows = 1
# )
# 
# cs_ba_2021 <- clean_cs_data(
#   path = here("data","conferred degree_national_21_22.xlsx"),
#   year_label = "2021-22",
#   skip_rows = 1
# )
# 
# ## Combine all years
# cs_ba_all <- bind_rows(
#   cs_ba_2012,
#   cs_ba_2013,
#   cs_ba_2014,
#   cs_ba_2015,
#   cs_ba_2016,
#   cs_ba_2017,
#   cs_ba_2018,
#   cs_ba_2019,
#   cs_ba_2020,
#   cs_ba_2021
# )
# 
# ## Keep only states and DC for National trend analysis and mapping
# #cs_ba_all %>% 
#   #count(academic_year)
# 
# cs_ba_all <- cs_ba_all %>% 
#   filter(state != "U.S. Service Academies")
# 
# juristictions <- c(
#   "American Samoa",
#   "Guam",
#   "Northern Marianas",
#   "Puerto Rico",
#   "U.S. Virgin Islands",
#   "Palau",
#   "Marshall Islands",
#   "Federated States of Micronesia"
# )
# 
# cs_ba_states <- cs_ba_all %>% 
#   filter(!state %in% juristictions)
# 
# cs_ba_states %>% 
#   count(academic_year)



## National trend analysis
## Choronical mapping
## Fixed effects regression
## Difference-in-differences 




