
# Script for cleaning EUG CAD data (2015-2025)

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

library(ggplot2)

# ============ EUG CAD Data ============ #


eugene_cad <- readRDS("data/intermediate/eugene_cad_raw.rds") %>%
  clean_names() %>%
  
  rename(
    year                = yr,
    incident_id         = inci_id,
    timestamp           = calltime,
    call_source         = callsource,
    close_code          = closecode,
    dispatch_time       = secs_to_disp,
    arrival_time        = secs_to_arrv,
    clear_time          = secs_to_close,
    dispatch            = disp,
    arrival             = arrv,
    prime_unit          = primeunit,
    nb_units_dispatched = units_dispd,
    nb_units_arrived    = units_arrived
  ) %>%
  
  mutate(
    timestamp   = ymd_hms(timestamp),
    incident_id = as.numeric(incident_id),
    
    across(c(agency, service, call_source, nature, close_code, 
             closed_as, priority, prime_unit), as.factor),
    
    across(c(dispatch_time, arrival_time, clear_time, 
             nb_units_dispatched, nb_units_arrived), as.numeric),
    
    agency = fct_recode(agency, "CAHOOTS" = "CAHE"),
    
    dispatch_time = as_hms(timestamp + dispatch_time),
    arrival_time  = as_hms(timestamp + arrival_time),
    clear_time    = as_hms(timestamp + clear_time),
    
    dispatch = as.factor(dispatch),
    arrival = as.factor(arrival),
    city = as.factor("Eugene")
    
  ) %>%
  
  select(
    #context
    timestamp, 
    incident_id, 
    agency,
    city,
    service,
    #call nature
    nature, 
    close_code, 
    closed_as, 
    priority, 
    call_source, 
    #chronology
    dispatch_time, 
    arrival_time, 
    clear_time, 
    dispatch, 
    arrival, 
    prime_unit, 
    nb_units_dispatched, 
    nb_units_arrived
  ) %>%
  distinct()

glimpse(eugene_cad)

write_csv(eugene_cad, "data/processed/eugene_cad_2015_2025.csv")













