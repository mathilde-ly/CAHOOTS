
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

data_merged <- bind_rows(cad_processed, spd_processed, mcslc_processed) %>%
  mutate(across(c(EPD, CAHOOTS, SPD, MCSLC), ~ replace_na(.x, 0))) %>%
  mutate(outcome = str_to_title(outcome)) %>%
  mutate(outcome = case_when(
    outcome %in% c("Building Check Secure", "Building Checked Secure") ~ "Building Checked Secure",
    outcome %in% c("Fire - No Damage", "Fire No Damage") ~ "Fire No Damage",
    outcome %in% c("Patient Transported", "Patient Trasnported") ~ "Patient Transported",
    outcome %in% c("Refused Service", "Refused Services") ~ "Refused Services",
    outcome %in% c("Refused Service - Cahoots", "Refused Services (Cahoots)") ~ "Refused Services (Cahoots)",
    outcome %in% c("Relayed To Lane County Sheriff's Office", "Relayed To Lane County Sheriffs Office") ~ "Relayed To Lane County Sheriff's Office",
    outcome %in% c("Tagged Ie Parking Cite Issued", "Tagged Parking Cite Issued") ~ "Tagged Parking Cite Issued",
    outcome %in% c("Unhoused-Related Issue", "Unhoused Related Issue") ~ "Unhoused Related Issue",
    outcome %in% c("Vehicle Towed - Not Impounded", "Vehicle Towed Non Impound") ~ "Vehicle Towed Non Impound",
    TRUE ~ outcome
  )) %>%
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

write.csv(data_merged, "data/clean/data_merged.csv")




  

