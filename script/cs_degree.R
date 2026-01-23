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
           

  
