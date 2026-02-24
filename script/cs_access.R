library(tidyverse)
library(readxl)
library(janitor)
library(here)
# read in 17-18 and 20-21 CRDC Climate data

or_school_1718 <- read_xlsx(here("data","ESSA_201718_CRDC_SCHOOL_CLIMATE.xlsx"), 
                            sheet = 6) %>% 
  clean_names() %>% 
  select(district_id_nces, district_name, school_id_nces, school_name, total_students_enrolled)

or_school_2021 <- read_xlsx(here("data","ESSA_202021_CRDC_SCHOOL_CLIMATE.xlsx"),
                           sheet = 6) %>% 
  clean_names() %>% 
  select(district_id_nces, district_name, school_id_nces, school_name, total_count_all_students)

ap_cs_1718 <- read_xlsx(here("data","ESSA_201718_CRDC_AP_IB_DE.xlsx"),
                        sheet = 2) %>% 
  clean_names() %>% 
  filter(str_detect(course, "Computer Science"))

ap_cs_2021 <- read_xlsx(here("data","ESSA_202021_CRDC_AP_IB_DE.xlsx"),
                        sheet = 2) %>% 
  clean_names()