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




