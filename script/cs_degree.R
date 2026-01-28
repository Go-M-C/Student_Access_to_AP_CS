# NCSE Digest of Education Statistics:
# Degrees in computer and information sciences conferred by postsecondary 
# institutions, by level of degree and sex of student: 
# Academic years 1964-65 through 2021-22

library(tidyr)
library(here)
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(janitor)
# remotes::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)

## Reading data
cs_raw <- read_xlsx(here("data","cs_degree_trends.xlsx"), skip = c(3))

## Data cleaning

### 1. Extract the numeric values
### 2. Convert year into a sample numeric variable
### 3. Reshape the table so that each row is one combination of year,
###.   degree level, sex
### 4. Drop or keep percent change variable

cs_clean <- cs_raw[-1,]
cs_clean <- cs_clean[1:58,]

glimpse(cs_clean)

cs_clean <- cs_clean%>% 
  clean_names() %>% 
  mutate(academic_year = as.numeric(str_sub(year, 6, 7)) +
           if_else(as.numeric(str_sub(year, 6, 7)) <= 30, 2000, 1900)) %>% 
  mutate(percent_female = as.numeric(percent_female)) %>% 
  mutate(annual_percent_change = as.numeric(annual_percent_change) %>% 
           str_replace_all("[^0-9.-]","") %>% 
           na_if("") %>% 
           as.numeric()
  ) %>% 
  select(1,13,everything())

cs_rename <- cs_clean %>% 
  rename(bachelor_total = number,
         bachelor_annual_percent_change = annual_percent_change,
         bachelor_male = male_4,
         bachelor_female = female_5,
         bachelor_female_percent = percent_female,
         
         master_total = total_7,
         master_male = male_8,
         master_female = female_9,
         
         doctor_total = total_10,
         doctor_male = male_11,
         doctor_female = female_12)
  

cs_degree <- cs_rename %>% 
  select(2,5,6,9,10,12,13) %>% 
  pivot_longer(
    cols = starts_with(c("bachelor","master","doctor")),
    names_to = c("degree_level","gender"),
    names_sep = "_",
    values_to = "value"
  )

write.csv(cs_degree, file = "data/cs_degree_clean.csv", row.names=FALSE)

#########################################################################

cs <- read.csv(here("data","cs_degree_clean.csv"))

cs <- cs %>% 
  mutate(
    degree_level = factor(degree_level,
                          levels = c("bachelor","master","doctor")),
    gender = factor(gender,
                    levels = c("male","female"),
                    labels = c("Male","Female"))
  )

cs %>% 
  filter(degree_level == "bachelor") %>% 
  ggplot(aes(x = academic_year, y = value, color = gender)) +
  geom_line(size = 1) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks = seq(1965, 2022, by = 4))+
  labs(
    title = "CS Bachelor's Degree by Gender (1965-2021)",
    x = "Year",
    y = "Number of Degrees",
    color = "Gender"
  )+
  theme_minimal(base_size = 14)

# In the plot above, x-axis labels were intentionally limited to maintain
# readability and emphasize long-term trends.

cs_ba_anim <- cs %>% 
  filter(degree_level == "bachelor") %>% 
  ggplot(aes(x = academic_year, y = value, group = gender, color = gender)) +
  geom_line(size = 1.2, alpha = 0.8) +
  geom_point(size = 3) +
  geom_text(aes(label = paste0(gender, ":", value)),
            hjust = -0.2, vjust = 0.5, size = 4, fontface = "bold") +
  scale_color_viridis_d(begin = 0.2, end = 0.8) +
  scale_y_continuous()+
  scale_x_continuous(breaks = seq(1965, 2022, by = 1))+
  coord_cartesian(clip = "off") +
  labs(title = "CS Bachelor's Degree by Gender (1965-2021)",
       x = "Year",
       y = "Number of Degrees",
       color = "Gender") +
  theme_minimal()+
  theme(
    plot.margin = margin(10,100,10,10),
    legend.position = "none",
    plot.title = element_text(size = 20, face = "bold"))+
  transition_reveal(academic_year)

animate(cs_ba_anim, nframes = 300, 
        fps = 50, duration = 15, 
        width = 800, height = 500,
        renderer = gifski_renderer())
