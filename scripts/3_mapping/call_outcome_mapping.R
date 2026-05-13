
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



# ============ categories mapping

arrest <- c("Arrest",
            "Cited In Lieu Of Custody",
            "Juvenile Taken Into Custody",
            "Non Criminal Hold",
            "Warning"
             )

medical <- c("Emergency Department",
             "Medical Aid Epd",
             "Patient Transported"
             )

support <- c("Crisis Walk-In Center",
             "Information Only",
             "Transport Made",
             "Welfare Check Done",
             "Sobering Or Detox Facility",
             "Transport Made" ,
             "Unhoused Related Issue",
             "Respite",
             "Remained In Community"
             )

refused <- c("Refused Services" ,
             "Refused Services (Cahoots)",
             )

ambiguous <- c("Welfare Check Done",)


