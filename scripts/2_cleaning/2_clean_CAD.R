
# Script for cleaning EUG CAD data (2015-2025)

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)
# ============ EUG CAD Data ============ #
# initial cleaning
# normalize cols names, adjust variable type
# reorder cols

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

# =========== ID CAHOOTS CALLS =========== #
# create binary column to indicate the agency dispatched
# identify calls handled by cahoots

eugene_cad %>%
  filter(
      str_detect(closed_as, "RELAYED")
  ) %>%
  count(prime_unit, closed_as, sort = TRUE) %>%
  print(n = 50)

# Identify CAHOOTS units
eugene_cad %>%
  filter(
    str_detect(nature, "CAHOOTS") |
    str_detect(closed_as, "CAHOOTS")
    ) %>%
  count(prime_unit, agency, nb_units_dispatched, sort = TRUE) %>%
  print(n = 50)

# Check if unit is CAHOOTS or if it's EPD with CAHOOTS support (nb_units_dispatched > 2)
eugene_cad %>%
  filter(service == "OTHR") %>%
  count(prime_unit, service, agency, sort = TRUE) %>%
  print(n = 700)


# Assign CAHOOTS to prime units found
cahoots_units <- c("_4J79", "_1J77", "_CAHOT", "_3J78", "_TESTCA", "_3J77", "_C100", "_3J79")

eugene_cad_mapped <- eugene_cad %>%
  filter(dispatch == 1) %>%
  mutate(
    EPD = if_else(agency == "EPD", 1, 0),
    CAHOOTS = if_else(agency == "CAHOOTS", 1, 0),
    
    # ID cahoots via prime_units
    # Si l'unité est CAHOOTS, on force CAHOOTS à 1. 
    # set EPD to 1 if more than 1 unit dispatched
    EPD = if_else(prime_unit %in% cahoots_units & nb_units_dispatched > 1, 1, EPD),
    CAHOOTS = if_else(prime_unit %in% cahoots_units, 1, CAHOOTS),
    
    # ID cahoots via text detection in nature and closed_as
    # set EPD to 1 if more than 1 unit were dispatched
    is_cah_text = str_detect(nature, "CAHOOTS") | str_detect(closed_as, "CAHOOTS"),
    CAHOOTS = if_else(is_cah_text, 1, CAHOOTS),
    EPD = if_else(is_cah_text & nb_units_dispatched > 1, 1, EPD)
  ) %>%
  
  # as factor 
  select(-is_cah_text) %>%
  mutate(
    across(c(EPD, CAHOOTS), as.factor)
  ) %>%
  select(
    #context
    timestamp, 
    incident_id, 
    agency,
    EPD,
    CAHOOTS,
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
  
table(eugene_cad_mapped$EPD, eugene_cad_mapped$CAHOOTS)
glimpse(eugene_cad_mapped)  


# =========== SAVE ============ #

write_csv(eugene_cad_mapped, "data/processed/eugene_cad_2015_2025.csv")













