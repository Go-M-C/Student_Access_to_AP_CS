library(tidyr)
library(here)
library(tidyverse)
library(readxl)
library(gganimate)
library(gifski)
library(janitor)


# Data clean and wrangling

## Import NCES data for 
## conferred bachelor degree by postsecondary institutions

cs_2012 <- read_excel(here("data","conferred degree_national_12_13.xls"), 
                      skip = 1, 
                      col_names = TRUE) %>% 
  clean_names() %>% 
  
  filter(!is.na(state_or_jurisdiction)) %>% 
  filter(str_detect(state_or_jurisdiction, "[A-Za-z]")) %>% 
  filter(!str_detect(state_or_jurisdiction, 
                     regex("United States|Other jurisdictions", 
                           ignore_case = TRUE))) %>% 
  filter(!is.na(computer_sciences)) %>% 

  select(state_or_jurisdiction, computer_sciences) %>% 
  rename(
    state = state_or_jurisdiction,
    cs_ba_dgrees = computer_sciences
  ) %>% 
  mutate(
    cs_ba_dgrees = as.numeric(str_remove_all(cs_ba_dgrees, ",")),
    academic_year = "2012-13",
    state = str_replace(state, "[^A-Za-z]+$",""),# remove any non-letter cha at end
    state = str_squish(state) # remove extra space
  )
   







