library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(sf)
library(plotly)
library(readr)
library(dplyr)



# CrseMap-OR (2021-22)

  or_cs_eco_2122 <- read_tsv(here("data","CrseMap-OR_Full Data_data.csv"),
                              locale = locale(encoding = "UTF-16")) %>% 
  clean_names() %>% 
    mutate(
      school_match = str_to_lower(str_trim(campusname)),
      district_match = str_to_lower(str_trim(districtname))
    )
    
saveRDS(or_cs_eco_2122, "data/or_cs_eco_2122.rds")