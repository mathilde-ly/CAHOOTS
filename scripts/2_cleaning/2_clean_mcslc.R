
# Script for cleaning MCS LC data

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

library(ggplot2)

# ============ MCS LC Data ============ #

mcslc_raw <- read_csv("data/processed/mcs_lc.csv")


mcslc <- mcslc_raw %>%
  clean_names() %>%
  rename(
    incident_id = id,
    timestamp = dispatch_request_date_time,
    dispatch_time = dispatch_date_time,
    arrival_time = arrival_on_scene_date_time,
    client_engagement_time = engagement_with_client_date_time,
    clear_time = mcit_departure_date_time,
  ) %>%
  pivot_longer(
    cols = starts_with("reason_for_dispatch_number_"),
    values_to = "call_type",
    values_drop_na = TRUE
  ) %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    across(ends_with("_time"), as.POSIXct),
    
    agency = "MCS LC",
    agency = as.factor(agency),
    
    city = as.factor(city),
    call_type = as.factor(call_type),
    
    call_outcome = fct_collapse(disposition,
      "Sobering or Detox Facility" = c("Sobering/Detox Facility","Sobering or Detox Facility")
    ),
    dispatch_status = case_match(end_point_of_dispatch,
                                 c("Engaged client", "Engaged Client") 
                                 ~ "Engaged",
                                 
                                 c("Client declined to engage", "Client declined  to engage", "Refused") 
                                 ~ "Refused",
                                 
                                 c("Unable to locate client", "Unable to Locate") 
                                 ~ "Unable to Locate",
                                 
                                 c("Cancelled", "Dispatch canceled before arrival") 
                                 ~ "Cancelled",
                                 
                                 "No contact due to safety concern" 
                                 ~ "No Contact due to Safety Concern",
                                 
                                 .default = "Other"
    ),
    dispatch_status = as.factor(dispatch_status),
  ) %>%
  select(
    #context
    timestamp,
    incident_id,
    agency,
    city,
    #call nature
    call_type,
    call_outcome,
    #chronology
    dispatch_time,
    client_engagement_time,
    arrival_time,
    clear_time,
    dispatch_status,
    minutes_request_dispatch,
    minutes_dispatch_arrival,
    minutes_arrival_engagement,
    minutes_arrival_departure
  )

unique(mcslc$call_type)
glimpse(mcslc)


write_csv(mcslc, "data/processed/mcs_lc.csv")






