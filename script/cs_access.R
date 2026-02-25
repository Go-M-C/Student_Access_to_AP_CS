library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(sf)
library(plotly)
# read in 20-21 enrollment data from ODE
# read in 20-21 Oregon AP course count data from CRDC


or_fall_202021 <- read_xlsx(here("data","fallmembershipreport_20202021.xlsx"),
                          sheet = 4) %>% 
  clean_names() %>% 
  select(
    district_id = attending_district_institution_id,
    district_name,
    school_id = attending_school_institution_id,
    school_name = school,
    total_enrollment_202021 = x2020_21_total_enrollment,
    american_indian = x2020_21_american_indian_alaska_native,
    asian = x2020_21_asian,
    native_ha_pa_islander = x2020_21_native_hawaiian_pacific_islander,
    black = x2020_21_black_african_american,
    hispanic = x2020_21_hispanic_latino,
    white = x2020_21_white,
    multi_racial = x2020_21_multi_racial,
    kindergarten = x2020_21_kindergarten,
    grade_one = x2020_21_grade_one,
    grade_two = x2020_21_grade_two,
    grade_three = x2020_21_grade_three,
    grade_four = x2020_21_grade_four,
    grade_five = x2020_21_grade_five,
    grade_six = x2020_21_grade_six,
    grade_seven = x2020_21_grade_seven,
    grade_eight = x2020_21_grade_eight,
    grade_nine = x2020_21_grade_nine,
    grade_ten = x2020_21_grade_ten,
    grade_eleven = x2020_21_grade_eleven,
    grade_twelve = x2020_21_grade_twelve
  ) %>% 
  mutate(
    school_id = as.character(school_id)
  )
  


or_ap_202021 <- read_xlsx(here("data","ESSA_202021_CRDC_AP_IB_DE.xlsx"),
                        sheet = 2) %>% 
  clean_names() %>% 
  mutate(across(starts_with("total_count_"), ~ na_if(., "*") %>% as.numeric())) %>% 
  mutate(school_id = as.character(school_id_nces)) 

or_ap_cs_202021 <- or_ap_202021 %>% 
  filter(course == "AP Computer Science") %>% 
  select(school_id,
         cs_all_count_students = total_count_all_students,
         cs_count_american_indian = total_count_american_indian_or_alaska_native,
         cs_count_asin = total_count_asian,
         cs_count_black = total_count_black_or_african_american,
         cs_count_hispanic = total_count_hispanic_or_latino,
         cs_count_native_ha_pa_islander = total_count_native_hawaiian_or_pacific_islander,
         cs_count_multi_racial = total_count_two_or_more_races,
         cs_count_white = total_count_white,
         cs_count_student_with_disability = total_count_students_with_disabilities,
         cs_count_female = total_count_females,
         cs_count_male = total_count_males,
         cs_count_ell = total_count_english_learners)


or_apcs_capacity <- or_fall_202021 %>% 
  left_join(or_ap_cs_202021, by = "school_id") %>% 
  mutate(ap_cs_offered = if_else(!is.na(cs_all_count_students), 1,0))


or_apcs_capacity <- or_apcs_capacity %>% 
  mutate(
    school_type = case_when(
      grepl("elementary", school_name, ignore.case = TRUE) ~ "Elementary",
      grepl("middle", school_name, ignore.case = TRUE) ~ "Middle",
      grepl("high", school_name, ignore.case = TRUE) ~ "High",
      grepl("charter", school_name, ignore.case = TRUE) ~ "Charter",
      grepl("international", school_name, ignore.case = TRUE) ~ "International",
      grepl("online", school_name, ignore.case = TRUE) ~ "Online",
      TRUE ~ "Other"
    )
  )


capacity_summary <- or_apcs_capacity %>% 
  group_by(school_type) %>% 
  summarise(
    total_schools = n(),
    schools_offering_cs = sum(ap_cs_offered, na.rm = TRUE),
    pct_offering_cs = round(schools_offering_cs/total_schools * 100, 1)
  )
capacity_summary





