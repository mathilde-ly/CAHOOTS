
# Script for cleaning SPD data (2015-2025)

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

# ============ SPD Data ============ #
# Cleaning and merging of SPD calls, response units and close codes

# --- Calls for Services ---
spd_calls <- readRDS("data/intermediate/spd_calls_raw.rds") %>%
  clean_names() %>%
  mutate(
    incident_id = as.numeric(incident_number),
    timestamp = ymd_hms(call_creation_time),
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

# --- Merging all SPD datasets ---
spd <- spd_calls %>%
  left_join(spd_response, by = c("incident_id" = "id_incident", "year" = "year")) %>%
  left_join(spd_codes, by = c("incident_id", "year")) %>%
  left_join(spd_defs, by = "close_code", relationship = "many-to-many") %>%
  rename(agency = responding_agency, call_source = call_creation_mechanism) %>%
  mutate(across(c(year, agency, call_type, final_call_type, priority, 
                  call_source, close_code, close_code_definition), as.factor),
         city = as.factor("Springfield"),
         nb_units_dispatched = case_when(
         is.na(units) | units == "NONE" ~ 0,
         TRUE ~ str_count(units, ",") + 1
         )) %>%
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
    nb_units_dispatched,
    #chronology
    dispatch_time,
    arrival_time,
    clear_time
  ) %>%
  distinct()


glimpse(spd)



# =========== ID CAHOOTS CALLS =========== #

# likely cahoots codes and natures
nature_cahoots <- c(
  # --- Mental health---
  'TRANSPORT', 'SUSPICIOUS SUBJECT', 'SUBJECT SCREAMING', 
  'MENTAL SUBJECT', 'MENTAL TRANSPORT', 'SUICIDAL SUBJECT', 
  'DISORIENTED SUBJECT', 'DECEASED SUBJECT', 'ILL SUBJECT', 
  'SUBJECT DOWN', 'SUICIDE', 'MENTAL/MEDICAL WARRANT',
  
  # --- Social & welfare ---
  'CHECK WELFARE', 'CHECK WELFARE, CAHOOTS', 'WELFARE CHECK DONE', 
  'UNHOUSED RELATED ISSUE', 'ILLEGAL CAMPING', 'PUBLIC ASSIST, CAHOOTS',
  'JUVENILE PROBLEM', 'RUNAWAY JUVENILE', 'MISSING PERSON',
  'DISPUTE', 'DISPUTE FAMILY', 'CIVIL STANDBY',
  
  # --- Substances & Addictions ---
  'DETOXIFICATION', 'INTOXICATED SUBJECT', 'OVERDOSE', 
  'IN POSSESSION OF NARCOTICS', 'DRUG INFO', 'FOUND SYRINGE',
  'POISONING', 'CARDIAC ARREST', 
  
  # --- Public order ---
  'DISORDERLY SUBJECT', 'LOUD NOISE', 'LOUD PARTY', 
  'INCOMPLETE CALL', 'UNKNOWN PROBLEM', 'TRESPASS', 
  'CRIMINAL TRESPASS', 'PUBLIC INDECENCY', 'NUDE SUBJECT',
  
  # --- Support & Assistance ---
  'ASSIST CAHOOTS', 'ASSIST FD, CAHOOTS', 'ASSIST PUBLIC- POLICE',
  'ASSIST MOTORIST', 'EMERGENCY MESSAGE', 'WALKAWAY'
)

close_code_cahoots <- c(
  # --- Interventions ---
  "Assisted", 
  "Transport Made", 
  "Welfare Check Done", 
  "Resolved", 
  "Advised", 
  "Unhoused-Related Issue", 
  "Referred to Other Agency",
  "Civil Issue", # Souvent de la médiation
  
  # --- Medical ---
  "Patient Trasnported", 
  "Non Criminal Hold", 
  "Med Express", 
  "Medical Aid EPD", 
  "No Patient Transport",
  "Dead On Arrival",
  
  # --- Failed ---
  "Refused Service - CAHOOTS", 
  "Refused Service", 
  "Gone on Arrival", 
  "Unable to Locate", 
  "Quiet On Arrival",
  
  # --- Cancel---
  "Cancel While Enroute", 
  "Cancel Fire Unit From Call",
  "No Action Taken", 
  "Action Taken",
  "Assisting Officer",
  "Quality of Life - No Dispatch"
)

# Identify CAHOOTS prime units (most frequent prime_unit for likely intervention motives)
units_summary <- spd %>%
  filter(!is.na(prime_unit),
         #close_code_definition %in% close_code_cahoots,
         #nature %in% nature_cahoots,
         prime_unit != "NONE") %>% 
  count(prime_unit, units, nature, close_code_definition, name = "total_calls") %>%
  group_by(prime_unit) %>%
  arrange(desc(total_calls))


# ============ APPLY ID TO DF ============ #
# map cahoots calls
cahoots_units <- c("CAHOT", "3J81") # deducted from analysis 
cahoots_pattern <- str_c(cahoots_units, collapse = "|")

spd_mapped <- spd %>%
  mutate(
    clean_units = str_replace_all(units, "\\s+", ""),
    
    # Logical flags for identification
    has_cahoots_unit = prime_unit %in% cahoots_units | str_detect(clean_units, cahoots_pattern),
    has_cahoots_text = str_detect(nature, "(?i)CAHOOTS") | str_detect(close_code_definition, "(?i)CAHOOTS"),
    
    # Binary tracking assignment
    CAHOOTS = if_else(has_cahoots_unit | has_cahoots_text, 1, 0),
    
    # Allocation logic for law enforcement presence
    SPD = case_when(
      # Co-response: Cahoots unit present alongside multiple units
      has_cahoots_unit & nb_units_dispatched > 1 ~ 1,
      # Solo response: Only Cahoots unit dispatched
      has_cahoots_unit & nb_units_dispatched == 1 ~ 0,
      # Regular response: Standard law enforcement allocation
      agency == "SPD" ~ 1,
      TRUE ~ 0
    )
  ) %>%
  mutate(across(c(SPD, CAHOOTS), as.factor)) %>%
  distinct() %>%
  select(
    #context
    timestamp, 
    incident_id, 
    agency,
    SPD,
    CAHOOTS,
    city,
    #call nature
    nature, 
    nature_code,
    close_code, 
    close_code_definition, 
    priority, 
    call_source,
    # units
    prime_unit,
    units,
    nb_units_dispatched,
    #chronology
    dispatch_time, 
    arrival_time, 
    clear_time, 
  )

table(spd_mapped$SPD, spd_mapped$CAHOOTS)
glimpse(spd_mapped)  


unique(spd_mapped$priority)

# ============ CHECK NULL VALUES ============ #

spd_mapped %>% 
  filter(SPD == 0, CAHOOTS == 0) %>%
  count(nb_units_dispatched, close_code_definition)

spd_mapped %>% 
  filter(is.na(nature_code)) %>%
  count(nature_code, nb_units_dispatched, nature, close_code_definition) %>%
  print(n = 150)

spd_mapped %>% 
  filter(is.na(CAHOOTS)) %>%
  count(nature_code, nb_units_dispatched, nature, close_code_definition) %>%
  print(n = 150)

spd_mapped %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count")

# ============ clean ============ #

spd_mapped_clean <- spd_mapped %>%
  filter(
    !is.na(timestamp),
    !is.na(nature),
    !is.na(close_code),
    !is.na(close_code_definition),
    !is.na(call_source)
  )


spd_mapped_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "na_count")

table(spd_mapped_clean$SPD, spd_mapped_clean$CAHOOTS)




spd_mapped_clean %>%
  count(SPD, CAHOOTS) %>%
  mutate(
    percentage = (n / sum(n)) * 100,
    total_calls = sum(n)
  )

spd_mapped_clean %>%
  summarise(
    total_calls = n(),
    pct_cahoots = sum(CAHOOTS == "1" & SPD == "0") / n() * 100,
    pct_cahoots_any = sum(CAHOOTS == "1") / n() * 100,
    pct_spd = sum(SPD == "1" & CAHOOTS == "0") / n() * 100,
    pct_spd_any = sum(SPD == "1") / n() * 100,
    pct_both = sum(CAHOOTS == "1" & SPD == "1") / n() * 100
  )


spd_mapped_clean %>%
  filter(CAHOOTS == 1) %>%
  count(nature, SPD, CAHOOTS) %>%
  arrange(n) %>%
  print( n = 500)

# =========== SAVE =========== #
write_csv(spd_mapped_clean, "data/processed/spd_2015_2025.csv")



