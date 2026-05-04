
# First data exploration

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)
library(ggplot2)

# ============ Data ============ #

mcslc <- read_csv("data/processed/mcs_lc.csv")
spd <- read_csv("data/processed/spd_2015_2025.csv")
cad <- read_csv("data/processed/eugene_cad_2015_2025.csv")

glimpse(mcslc)
glimpse(spd)
glimpse(cad)

# ============ Exploration ============ #

# CAHOOTS IDENTIFICATION IN CAD 


unique(cad$agency)
cad %>% 
  count(agency, sort = TRUE)

unique(mcslc$call_outcome)
unique(spd$close_code_definition)
unique(cad$closed_as)

# =========== Mapping Call Outcome ============ #
# Mapping all call_outcome categories in (arrest, health_emergency, community_support, other)

# CAD

cad_mapped <- cad %>%
  mutate(outcome_category = case_when(
    closed_as %in% c("ARREST", "CITED IN LIEU OF CUSTODY", "JUVENILE TAKEN INTO CUSTODY") ~ "Arrest",
    closed_as %in% c("PATIENT TRANSPORTED", "MEDICAL AID EPD", "TRANSPORT MADE") ~ "Emergency Department",
    closed_as %in% c("RESOLVED", "ASSISTED", "WELFARE CHECK DONE", "REFUSED SERVICES (CAHOOTS)", "NON CRIMINAL HOLD") ~ "Community Support",
    TRUE ~ "Other"
  ))

# SPD
spd_mapped <- spd %>%
  mutate(outcome_category = case_when(
    close_code_definition %in% c("Arrest", "Cited in Lieu of Custody", "Juvenile Taken Into Custody") ~ "Arrest",
    close_code_definition %in% c("Patient Trasnported", "Med Express", "Transport Made") ~ "Emergency Department",
    close_code_definition %in% c("Resolved", "Assisted", "Welfare Check Done", "Non Criminal Hold") ~ "Community Support",
    TRUE ~ "Other"
  ))

# MCS LC
mcslc_mapped <- mcslc %>%
  mutate(outcome_category = case_when(
    call_outcome == "Arrest" ~ "Arrest",
    call_outcome == "Emergency Department" ~ "Emergency Department",
    call_outcome %in% c("Remained in community", "Crisis walk-in Center", "Respite", "Sobering or Detox Facility") ~ "Community Support",
    TRUE ~ "Other"
  ))



glimpse(mcslc_mapped)
glimpse(spd_mapped)
glimpse(cad_mapped)

write.csv(mcslc_mapped, "data/processed/category_mcslc_outcome.csv", row.names = FALSE)
write.csv(cad_mapped, "data/processed/category_cad_outcome.csv", row.names = FALSE)
write.csv(spd_mapped, "data/processed/category_spd_outcome.csv", row.names = FALSE)















