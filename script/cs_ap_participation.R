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
# ==============================PARTICIPATION========================#
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

# Join
apcs_fall_join <- or_ap_cs_prepared %>% 
  left_join(or_fall_enrollment, by = c("school_match", "district_match"))


# 3. CrseMap-OR (2021-22)
or_crsemap_2122 <- read_tsv(here("data","CrseMap-OR_Full Data_data.csv"),
                            locale = locale(encoding = "UTF-16")) %>% 
  clean_names()

or_geo_simple <- or_crsemap_2122 %>% 
  mutate(
    school_match = str_to_lower(str_trim(campusname)),
    district_match = str_to_lower(str_trim(districtname))
  ) %>% 
  group_by(school_match, district_match) %>% 
  summarise(
    lat = first(latcod),
    lon = first(loncod),
    locale = first(locale))%>% 
  ungroup()


# Join

or_apcs_202021 <- apcs_fall_join %>% 
  left_join(or_geo_simple, by = c("school_match", "district_match"))


saveRDS(or_apcs_202021, "data/or_apcs_202021.rds")
#============================================
# Analysis

or_hs_enrollment <- or_fall_subset %>% 
  filter(school_match != district_match) %>% 
  mutate(across(starts_with("grade_"), ~as.numeric(as.character(.)))) %>% 
  mutate(across(starts_with("grade_"), ~replace_na(., 0))) %>% 
  mutate(hs_enrollment = rowSums(across(c(grade_nine, grade_ten,
                                          grade_eleven,grade_twelve)), 
                                 na.rm = TRUE)) %>% 
  filter(hs_enrollment > 0) %>% 
  select(school_match, district_match,hs_enrollment,total_enrollment_202021)

ap_model <- or_hs_enrollment %>% 
  left_join(or_ap_cs_prepared, by = c("school_match", "district_match")) %>% 
  left_join(or_geo_simple, by = c("school_match", "district_match")) %>% 
  mutate(
    has_ap_cs = if_else(!is.na(cs_ap_total) & cs_ap_total>0, 1, 0),
    cs_ap_total = replace_na(cs_ap_total, 0),
    locale = as.character(locale),
    locale = replace_na(locale),
    locale = as.factor(locale)
  ) %>% 
  select(school_match,school_name,district_match,district_name,
         total_enrollment_202021, hs_enrollment,
         lat, lon,locale,has_ap_cs,cs_ap_total, everything()) %>% 
  filter(hs_enrollment > 0)

ap_model_classified <- ap_model %>% 
  mutate(
    mapping_status = if_else(!is.na(lat) & !is.na(lon),
                          "Mapped School",
                          "Unmapped Program"),
    locale_report = if_else(mapping_status == "Unmapped Program", "Non-Traditional Program", as.character(locale))
  )

saveRDS(ap_model_classified, "data/ap_cs_model.rds")


logit_model <- glm(has_ap_cs ~ locale + size,???
                   data = ap_model,
                   family = binomial)
summary(logit_model)

exp(coef(logit_model))

ggplot(ap_model, aes(x = hs_enrollment, y = has_ap_cs))+
  geom_point(aes(color = locale), alpha = 0.5) +
  stat_smooth(method = "glm", method.args = list(family = "binomial"),
              se = TRUE, color = "#ab9f7a") +
  theme_minimal()+
  labs(
    title = "Probability of Offering AP CS by School Size",
    x = "High School Enrollment",
    y = "Probability (0 to 1)"
  )

locale_summary <- ap_model_classified %>% 
  group_by(locale_report) %>% 
  summarise(
    total_schools = n(),
    schools_with_cs = sum(has_ap_cs, na.rm = TRUE),
    access_rate = round((schools_with_cs/total_schools)*100,digits = 1),
    avg_enrollment = round(mean(hs_enrollment, na.rm = TRUE),digits = 1),
    avg_cs_in_offering_schools = round(mean(cs_ap_total[cs_ap_total > 0], na.rm = TRUE), digits = 1)
  ) %>% 
  arrange(desc(access_rate))

saveRDS(locale_summary, "data/ap_cs_locale_summary.rds")

table(ap_model$locale, ap_model$has_ap_cs)


ggplot(ap_model, aes(x = locale, y = ))

# ================================BEYOND AP CS=======================#
or_fall_raw_2122 <- read_xlsx(here("data","fallmembershipreport_20212022.xlsx"),sheet = 4) %>% 
  clean_names() 

or_fall_subset_2122 <- or_fall_raw_2122 %>% 
  mutate(
    school_match = str_to_lower(str_trim(school)),
    district_match = str_to_lower(str_trim(district_name))
  ) %>% 
  select(
    school_match,district_match,
    district_id = attending_district_institution_id,
    school_id = attending_school_institution_id,
    school_name = school,
    total_enrollment_202122 = x2021_22_total_enrollment,
    american_indian = x2021_22_american_indian_alaska_native,
    asian = x2021_22_asian,
    native_ha_pa_islander = x2021_22_native_hawaiian_pacific_islander,
    black = x2021_22_black_african_american,
    hispanic = x2021_22_hispanic_latino,
    white = x2021_22_white,
    multi_racial = x2021_22_multi_racial,
    kindergarten = x2021_22_kindergarten,
    grade_one = x2021_22_grade_one,
    grade_two = x2021_22_grade_two,
    grade_three = x2021_22_grade_three,
    grade_four = x2021_22_grade_four,
    grade_five = x2021_22_grade_five,
    grade_six = x2021_22_grade_six,
    grade_seven = x2021_22_grade_seven,
    grade_eight = x2021_22_grade_eight,
    grade_nine = x2021_22_grade_nine,
    grade_ten = x2021_22_grade_ten,
    grade_eleven = x2021_22_grade_eleven,
    grade_twelve = x2021_22_grade_twelve
  )



or_cs_eco_with_enroll_2122 <- eco_202122 %>% 
  left_join(or_fall_subset_2122, by = c("school_match", "district_match")) %>% 
  mutate(
    total_enrollment_202122 = as.numeric(as.character(total_enrollment_202122)),
    total_enrollment_202122 = replace_na(total_enrollment_202122, 0),
    number_of_courses = as.numeric(as.character(number_of_courses))
    ) %>% 
  select("school_year","locale","districtname","school_name",
         "total_enrollment_202122","subcategory","number_of_courses","latcod","loncod", everything())

saveRDS(or_cs_eco_with_enroll_2122, "data/or_cs_eco_with_enroll_2122.rds")

eco_enroll_202122 <- readRDS("data/or_cs_eco_with_enroll_2122.rds")

or_eco_summary <- eco_enroll_202122 %>% 
  group_by(school_name, locale) %>% 
  mutate(grade_nine = as.numeric(as.character(grade_nine)),
         grade_ten = as.numeric(as.character(grade_ten)),
         grade_eleven = as.numeric(as.character(grade_eleven)),
         grade_twelve = as.numeric(as.character(grade_twelve))) %>% 
  summarise(
    school_total_courses = sum(number_of_courses, na.rm = TRUE),
    hs_enroll_9_12 = first(coalesce(grade_nine, 0)+
                             coalesce(grade_ten,0)+
                             coalesce(grade_eleven,0)+
                             coalesce(grade_twelve,0)),
    .groups = "drop"
  ) %>% 
  group_by(locale) %>% 
  summarise(
    Total_Schools = n_distinct(school_name),
    Schools_with_CS = sum(school_total_courses>0),
    Total_HS_Enrollment = sum(hs_enroll_9_12, na.rm = TRUE),
    Avg_School_Size = round(mean(hs_enroll_9_12, na.rm = TRUE),0),
    Total_CS_Offerings = sum(school_total_courses, na.rm = TRUE),
    .groups = "drop"
  ) %>% 
  mutate(CS_Access_Rate = round((Total_CS_Offerings/Total_HS_Enrollment) *100, 2),
         Avg_Variety_Per_School = round(Total_CS_Offerings/Total_Schools,2))

saveRDS(or_eco_summary, "data/or_eco_locale_summary.rds")
#The Urban "Breadth" Strategy: Large city schools serve massive populations (Avg. 1,043 students). While their "per-capita" density is lower (0.42), they offer more than double the variety (4.35 subcategories). A student in a city likely has choices: AP CS, Robotics, Web Dev, and AI.
#The Rural "Presence" Strategy: Rural schools are small (Avg. 198 students). They have a high "per-capita" density (1.06) because even one course in a tiny school creates a high ratio. However, their variety is limited (2.10 subcategories). A student there likely only has: Intro to CS and maybe one other option.

locale_demo_plot <- eco_enroll_202122 %>% 
  mutate(grade_nine = as.numeric(as.character(grade_nine)),
         grade_ten = as.numeric(as.character(grade_ten)),
         grade_eleven = as.numeric(as.character(grade_eleven)),
         grade_twelve = as.numeric(as.character(grade_twelve))) %>%
  mutate(hs_total = grade_nine+grade_ten+grade_eleven+grade_twelve) %>%
  mutate(across(c(hispanic,asian, white,black,american_indian, native_ha_pa_islander, multi_racial),
         ~as.numeric(as.character(.x)))) %>% 
  filter(hs_total > 0) %>% 
  group_by(school_name, locale) %>% 
  summarise(
    Hispanic = first(hispanic),
    Asian = first(asian),
    White = first(white),
    Black = first(black),
    American_Indian = first(american_indian),
    Native_Ha_Pa_Islander = first(native_ha_pa_islander),
    Multi = first(multi_racial),
    .groups = "drop"
  ) %>% 
  group_by(locale) %>% 
  summarise(
    across(c(Hispanic, Asian, White, Black, American_Indian, Native_Ha_Pa_Islander, Multi),
           ~sum(.x, na.rm = TRUE)),
    .groups = "drop"
  )
  
saveRDS(locale_demo_plot, "data/locale_summary.rds")


