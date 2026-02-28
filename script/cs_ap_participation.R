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
  mutate(across(starts_with("grade_"), ~as.numeric(as.character(.)))) %>% 
  mutate(across(starts_with("grade_"), ~replace_na(., 0))) %>% 
  mutate(hs_enrollment = rowSums(across(c(grade_nine, grade_ten,
                                          grade_eleven,grade_twelve)), 
                                 na.rm = TRUE)) %>% 
  filter(hs_enrollment > 0) %>% 
  select(school_match, district_match,hs_enrollment,total_enrollment_202021)

ap_model <- or_ap_cs_prepared %>% 
  inner_join(or_hs_enrollment, by = c("school_match", "district_match")) %>% 
  left_join(or_geo_simple, by = c("school_match", "district_match")) %>% 
  filter(!is.na(locale)) %>% 
  mutate(
    has_ap_cs = if_else(cs_ap_total>0, 1, 0),
    locale = as.factor(locale)
  ) %>% 
  select(school_match,school_name,district_match,district_name,
         total_enrollment_202021, hs_enrollment,
         lat, lon,locale,has_ap_cs,cs_ap_total, everything())
  
saveRDS(ap_model, "data/ap_cs_model.rds")
logit_model <- glm(has_ap_cs ~ hs_enrollment + locale,
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

locale_summary <- ap_model %>% 
  group_by(locale) %>% 
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



