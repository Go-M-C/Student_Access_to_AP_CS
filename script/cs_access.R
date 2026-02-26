library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(sf)
library(plotly)
library(readr)
library(dplyr)
# read in 20-21 enrollment data from ODE
# read in 20-21 Oregon AP course count data from CRDC

# 1. CRDC AP ESSA DATA(2020-21)
or_ap_raw<- read_xlsx(here("data","ESSA_202021_CRDC_AP_IB_DE.xlsx"),
                          sheet = 2) %>% 
  clean_names() %>% 
  mutate(across(starts_with("total_count_"), ~ na_if(., "*") %>% as.numeric()))

or_ap_cs_prepared <- or_ap_raw %>% 
  filter(course == "AP Computer Science") %>% 
  mutate(
    school_match = str_to_lower(str_trim(school_name)),
    district_match = str_to_lower(str_trim(district_name))
  ) %>% 
  select(school_match, 
         district_match, 
         school_name, 
         district_name,
         cs_ap_total = total_count_all_students,
         cs_ap_count_american_indian = total_count_american_indian_or_alaska_native,
         cs_ap_count_asian = total_count_asian,
         cs_ap_count_black = total_count_black_or_african_american,
         cs_ap_count_hispanic = total_count_hispanic_or_latino,
         cs_ap_count_native_ha_pa_islander = total_count_native_hawaiian_or_pacific_islander,
         cs_ap_count_multi_racial = total_count_two_or_more_races,
         cs_ap_count_white = total_count_white,
         cs_ap_count_student_with_disability = total_count_students_with_disabilities,
         cs_ap_count_female = total_count_females,
         cs_ap_count_male = total_count_males,
         cs_ap_count_ell = total_count_english_learners)

# 2. ODE Fall Membership (2020-21)
or_fall_raw <- read_xlsx(here("data","fallmembershipreport_20202021.xlsx"),sheet = 4) %>% 
  clean_names() 

or_fall_subset <- or_fall_raw %>% 
  mutate(
    school_match = str_to_lower(str_trim(school)),
    district_match = str_to_lower(str_trim(district_name))
  ) %>% 
  select(
    school_match,district_match,
    district_id = attending_district_institution_id,
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
  )

or_fall_enrollment <- or_fall_subset %>% 
  select(
    school_match,
    district_match,
    total_enrollment_202021
  )
# 3. CrseMap-OR (2021-22)
  or_crsemap_2122 <- read_tsv(here("data","CrseMap-OR_Full Data_data.csv"),
                              locale = locale(encoding = "UTF-16")) %>% 
  clean_names()
  
  or_geo_subset <- or_crsemap_2122 %>% 
    mutate(
      school_match = str_to_lower(str_trim(campusname)),
      district_match = str_to_lower(str_trim(districtname))
    ) %>% 
    dplyr::group_by(school_match, district_match) %>% 
    dplyr::summarise(
      lat = dplyr::first(latcod),
      lon = dplyr::first(loncod),
      locale = dplyr::first(locale),
      total_cs_capacity = sum(as.numeric(number_of_courses), na.rm = TRUE),
      subcategory_list = paste(unique(subcategory), collapse = ", "),
      unique_course_types = n_distinct(subcategory)
    ) %>% 
    dplyr::ungroup()

  
# Join
  

  apcs_fall_join <- or_ap_cs_prepared %>% 
    left_join(or_fall_enrollment, by = c("school_match", "district_match"))
  # match_summary <- test_join %>% 
  #   summarise(
  #     total_schools = n(),
  #     matched_enrollment = sum(!is.na(total_enrollment_202021)),
  #     missing_enrollment = sum(is.na(total_enrollment_202021))
  #   )
  
  or_final_ecosystem <- apcs_fall_join %>% 
    left_join(or_geo_subset, by = c("school_match","district_match")) %>% 
    mutate(
      total_cs_capacity = replace_na(total_cs_capacity, 0),
      unique_course_types = replace_na(unique_course_types, 0)
    )
  #cat("Schools with Map Coordinates:", sum(!is.na(or_final_ecosystem$lat)), "\n")


or_final_ecosystem


