
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

unique(eugene_cad$close_code)



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
  count(prime_unit, agency, service, nb_units_dispatched, sort = TRUE) %>%
  print(n = 50)

# Check if unit is CAHOOTS or if it's EPD with CAHOOTS support (nb_units_dispatched >= 2)
eugene_cad %>%
  filter(service == "OTHR") %>%
  count(nature, closed_as, service, sort = TRUE) %>%
  print(n = 100)

eugene_cad %>%
  filter(nature == "TRANSPORT") %>%
  count(closed_as, service, sort = TRUE) %>%
  print(n = 100)

eugene_cad %>% filter(str_detect(nature, "CHECK WELFARE")) %>%
  count(nature, prime_unit) %>%
  arrange() %>% print(n = 1000)

# ============ ASSIGN CAHOOTS =========== #
# Assign CAHOOTS to prime units found
# https://lawenforcementactionpartnership.org/eugene-oregon-cahoots-program/
cahoots_units <- c("_1J77","_3J78", "_3J79", "_CAHOT",  "_TESTCA")


eugene_cad_mapped <- eugene_cad %>% # only dispatched calls
  filter(dispatch == 1,
         prime_unit != "NULL" # checked, every null prime unit is a disregarded or similar close code
         ) %>%
  mutate(
    CAHOOTS = if_else(agency == "CAHOOTS", 1, 0),
    
    CAHOOTS = if_else(prime_unit %in% cahoots_units, 1, CAHOOTS),
    
    EPD = case_when(
      prime_unit %in% cahoots_units & nb_units_dispatched > 1 ~ 1,
      prime_unit %in% cahoots_units ~ 0,
      agency == "EPD" ~ 1,
      TRUE ~ 0
    )
    ) %>%
  
  # as factor 
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
  distinct() %>%
  mutate(priority = fct_explicit_na(priority, "Not Assigned"))
  

eugene_cad_mapped %>%
  count(EPD, CAHOOTS) %>%
  mutate(
    percentage = (n / sum(n)) * 100,
    total_calls = sum(n)
  )

eugene_cad_mapped %>%
  summarise(
    total_calls = n(),
    pct_cahoots = sum(CAHOOTS == "1" & EPD == "0") / n() * 100,
    pct_cahoots_any = sum(CAHOOTS == "1") / n() * 100,
    pct_epd = sum(EPD == "1" & CAHOOTS == "0") / n() * 100,
    pct_epd_any = sum(EPD == "1") / n() * 100,
    pct_both = sum(CAHOOTS == "1" & EPD == "1") / n() * 100
  )

glimpse(eugene_cad_mapped)  

# ============ check for wrong values ============ #

eugene_cad_mapped %>% 
  filter(nature == "CHECK WELFARE") %>%
  count(nature, agency, closed_as, service, EPD, CAHOOTS, prime_unit, nb_units_dispatched ) %>%
  filter(n > 100) %>%
  arrange(-n) %>%
  print( n = 100)

unique(eugene_cad_mapped$nature)

eugene_cad_mapped %>%
  filter(closed_as == "GONE ON ARRIVAL") %>% 
  count(prime_unit, nature, closed_as) %>%
  filter(n > 100) %>%
  arrange(n) %>%
  print( n = 100)

eugene_cad_mapped %>%
  filter(nature == "ASSIST PD, CAHOOTS") %>%
  count(nature, agency, EPD, CAHOOTS, prime_unit, nb_units_dispatched) %>%
  arrange()


# =========== SAVE ============ #

write_csv(eugene_cad_mapped, "data/processed/eugene_cad_2015_2025.csv")













