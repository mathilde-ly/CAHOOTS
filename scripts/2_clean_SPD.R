
# Script for cleaning SPD data (2015-2025)

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

library(ggplot2)

# ============ SPD Data ============ #


# --- Calls for Services ---
spd_calls <- readRDS("data/intermediate/spd_calls_raw.rds") %>%
  clean_names() %>%
  mutate(
    incident_id = as.numeric(incident_number),
    timestamp = ymd_hms(call_creation_time),
    # Conversion des dates sérielles Excel
    across(
      c(first_dispatched_time, first_arrival_time, clear_time),
      ~ as_datetime(as.numeric(if_else(.x == "NULL", NA_character_, .x)) * 86400, 
                    origin = "1899-12-30")
    ),
    dispatch_time = as_hms(round_date(first_dispatched_time, "minute")),
    arrival_time  = as_hms(round_date(first_arrival_time, "minute")),
    clear_time    = as_hms(round_date(clear_time, "minute")),
    prime_unit    = coalesce(primary_responding_unit, "NONE")
  ) %>%
  select(year, incident_id, call_type = initial_call_type, final_call_type, 
         responding_agency, prime_unit, timestamp, dispatch_time, 
         arrival_time, clear_time, priority, call_creation_mechanism)

# --- Responding Units ---
spd_response <- readRDS("data/intermediate/spd_response_raw.rds") %>%
  clean_names() %>%
  mutate(
    id_incident = as.numeric(inci_id),
    timestamp   = parse_date_time(call_time, orders = "mdyHM"),
    units       = coalesce(units_dispatched_in_order, "NONE")
  ) %>%
  select(year, id_incident, units)

# --- Close Codes ---
spd_codes <- readRDS("data/intermediate/spd_codes_raw.rds") %>%
  clean_names() %>%
  mutate(incident_id = as.numeric(incident_number)) %>%
  select(year, incident_id, close_code)

spd_defs <- readRDS("data/intermediate/spd_defs_raw.rds") %>%
  clean_names() %>%
  rename(close_code_definition = definition)

# --- Fusion and Final export ---
spd_final <- spd_calls %>%
  left_join(spd_response, by = c("incident_id" = "id_incident", "year" = "year")) %>%
  left_join(spd_codes, by = c("incident_id", "year")) %>%
  left_join(spd_defs, by = "close_code", relationship = "many-to-many") %>%
  rename(agency = responding_agency, call_source = call_creation_mechanism) %>%
  mutate(across(c(year, agency, call_type, final_call_type, priority, 
                  call_source, close_code, close_code_definition), as.factor),
         city = as.factor("Springfield")) %>%
  rename(
    nature_code = call_type, 
    nature = final_call_type) %>%
  select(
    #context
    timestamp,
    incident_id,
    agency,
    city,
    #call nature
    nature,
    nature_code,
    close_code,
    close_code_definition,
    priority,
    call_source,
    #ressources
    prime_unit,
    units,
    #chronology
    dispatch_time,
    arrival_time,
    clear_time
  ) %>%
  distinct()


glimpse(spd_final)
    
    
write_csv(spd_final, "data/processed/spd_2015_2025.csv")


