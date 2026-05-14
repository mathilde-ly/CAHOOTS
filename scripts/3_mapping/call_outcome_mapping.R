
# call outcome mapping in merged data
# -> assign every call outcome to one of 4 broad category of outcome

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

# ============ DATA ============ #

data <- read.csv("data/processed/data_merged.csv")
glimpse(data)

# =========== outcome identification ============ #

data$nature %>% # list all outcomes and put obvious ones in lists
  unique() %>%
  sort()

# Figure out Respite -> Support
data %>%
  filter(outcome == "Respite") %>% 
  count(nature) %>% 
  arrange(nature)

# Figure out Remained In Community -> Support
data %>%
  filter(outcome == "Remained In Community") %>% 
  count(nature) %>% 
  arrange(nature)

# Figure out Welfare Check Done -> Ambiguous, will filter on nature rather than apply list
data %>%
  filter(outcome == "Welfare Check Done") %>% 
  count(nature) %>% 
  arrange(nature)

# Figure out Welfare Check Done -> Ambiguous, will filter on nature rather than apply list
data %>%
  filter(outcome == "Unhoused Related Issue") %>% 
  count(nature) %>% 
  arrange(nature)

data %>%
  filter(MCSLC == 1) %>% 
  count(outcome) %>% 
  arrange(outcome)



# ============ categories ============ #
# defining categories for call outcomes

arrest <- c("Arrest",
            "Cited In Lieu Of Custody",
            "Juvenile Taken Into Custody",
            "Non Criminal Hold", # is it an arrest
            "Warning"
             )

medical <- c("Emergency Department",
             "Medical Aid Epd",
             "Patient Transported"
             )

support <- c("Crisis Walk-In Center",
             "Information Only",
             "Sobering Or Detox Facility",
             "Transport Made" ,
             "Respite",
             "Remained In Community",
             "Welfare Check Done",
             "Sobriety Check"
             )

refused <- c("Refused Services" ,
             "Refused Services (Cahoots)"
             )

ambiguous <- c("Transport Made",
               "Unhoused Related Issue")

# ============ mapping ============ #
# cross validation of outcomes with nature and agency

data_mapped <- data %>%
  mutate(
    outcome_category = case_when(
      # categorical lists
      outcome %in% arrest ~ "arrest",
      outcome %in% medical ~ "medical",
      outcome %in% refused ~ "refused",
      
      # ambiguous outcomes
      outcome == "Transport Made" & CAHOOTS == 1 ~ "support",
      outcome == "Transport Made" & CAHOOTS == 0 ~ "other",
      
      outcome == "Unhoused Related Issue" & CAHOOTS == 1 ~ "support",
      outcome == "Unhoused Related Issue" & CAHOOTS == 0 ~ "other",
      
      # support lists
      outcome %in% support ~ "support",
      
      # default category
      TRUE ~ "other"
    )
  ) %>%
  select(-X)

data_mapped %>%
  count(outcome_category) %>%
  arrange(outcome_category)

# ============ save ============ #


write.csv(data_mapped, "data/clean/clean_mapped_call_outcome_full_data.csv", row.names = FALSE)

