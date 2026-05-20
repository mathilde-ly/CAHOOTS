
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
         dispatch_status,
         priority,
         source)

glimpse(cad_processed)
glimpse(spd_processed)
glimpse(mcslc_processed)

# =========== full join =========== #

data_merged <- bind_rows(cad_processed, spd_processed, mcslc_processed) %>%
  mutate(across(c(EPD, CAHOOTS, SPD, MCSLC), ~ replace_na(.x, 0))) %>%
  mutate(
    outcome = str_to_title(outcome),
         dispatch_status = coalesce(dispatch_status, "Non MCSLC"),
         
         nature = str_to_title(nature),
         nature = str_trim(nature),
         nature = str_remove(nature, ", Cahoots$"),
         
         nature = case_when(
         str_detect(nature, "^Gas Leak|Outside Gas Leak") ~ "Gas Leak",
         str_detect(nature, "Motor Veh.*Acc") ~ "Motor Vehicle Accident",
         str_detect(nature, "^Alarm") ~ "Alarm",
         nature == "Overdo" ~ "Overdose",
         nature == "Gunsho" ~ "Gunshot",
         nature %in% c("Assist Public- Police") ~ "Public Assist",
         nature %in% c("Test", "Test Call", "Test Call Epd") ~ "Test Call",
         nature %in% c("Suicidal Subject", "Suicidality Or Suicide Attempt") ~ "Suicidal Subject",
         nature %in% c("Animal Attack/Bite", "Animal Bite") ~ "Animal Attack/Bite",
         nature %in% c("Assist Fd", "Assist Fire Department") ~ "Assist Fire Department",
         nature %in% c("Assist Pd", "Assist Police") ~ "Assist Police",
         TRUE ~ nature)
         ) %>%
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
    dispatch_status,
    priority,
    source
  ) %>%
  mutate(
    timestamp = as_datetime(timestamp),
    across(c(city, nature, outcome, dispatch_status, priority, source), as.factor),
    across(c(EPD, SPD, CAHOOTS, MCSLC), as.factor)
  )
  

glimpse(data_merged)

data_merged %>%
  count(nature) %>%
  arrange()

unique(data_merged$outcome)
unique(data_merged$nature)




# ============ save ============ #

write.csv(data_merged, "data/clean/data_merged.csv")
saveRDS(data_merged, "data/clean/data_merged.rds")




  

