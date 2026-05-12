
# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

# ============ DATA ============ #
cad <- read.csv("data/processed/eugene_cad_2015_2025.csv")
spd <- read.csv("data/processed/spd_2015_2025.csv")
mcslc <- read.csv("data/processed/mcs_lc.csv")

glimpse(cad)
glimpse(spd)
glimpse(mcslc)

# =========== prep data to harmonize ============ #

# prevent same incident_id in data
# rename and select columns
cad_processed <- cad %>%
  mutate(incident_id = paste0("cad_", incident_id),
         source = "CAD") %>%
  rename(outcome = closed_as) %>%
  select(timestamp,
         incident_id,
         EPD,
         CAHOOTS,
         city,
         nature,
         outcome,
         priority,
         source)

spd_processed <- spd %>%
  mutate(incident_id = paste0("spd_", incident_id),
         source = "SPD") %>%
  rename(outcome = close_code_definition) %>%
  select(timestamp,
         incident_id,
         SPD,
         CAHOOTS,
         city,
         nature,
         outcome,
         priority,
         source)

mcslc_processed <- mcslc %>%
  mutate(incident_id = paste0("mcs_", incident_id),
         source = "MCS") %>%
  rename(outcome = call_outcome,
         nature = call_type) %>%
  select(timestamp,
         incident_id,
         MCSLC,
         city,
         nature,
         outcome,
         priority,
         source)

glimpse(cad_processed)
glimpse(spd_processed)
glimpse(mcslc_processed)

# =========== full join =========== #

data_merged <- cad_processed %>%
  full_join(spd_processed, 
            by = c("timestamp", "incident_id", "city", "nature", "outcome", "priority", "source", "CAHOOTS")) %>%
  full_join(mcslc_processed, 
            by = c("timestamp", "incident_id", "city", "nature", "outcome", "priority", "source")) %>%
  mutate(across(c(EPD, CAHOOTS, SPD, MCSLC), ~ replace_na(.x, 0))) %>%
  select(
    timestamp,
    incident_id,
    EPD,
    SPD,
    CAHOOTS,
    MCSLC,
    city,
    nature,
    outcome,
    priority,
    source
  )

glimpse(data_merged)

# ============ save ============ #

write.csv(data_merged, "data/processed/data_merged.csv")




  

