
# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

# ============ DATA ============ #

data <- readRDS("data/clean/data_merged.rds")  

# ============ Outcome Frequency ============ #

# CAHOOTS
natures_c <- data %>%
  filter(CAHOOTS == 1, EPD == 0 ) %>%
  count(nature, sort = TRUE) %>%
  slice_head(n = 500)

# MCSLC
natures_m <- data %>%
  filter(MCSLC == 1 ) %>%
  count(nature, sort = TRUE) %>%
  slice_head(n = 500)


print(natures_c)
print(natures_m)

# check where Assist Police's source
data %>%
  filter(nature == "Assist Police") %>%
  count(nature, EPD, CAHOOTS, SPD) %>%
  arrange()

# top 14
top_cahoots_natures <- c(
  "Public Assist",
  "Check Welfare",
  "Transport",
  "Suicidal Subject",
  "Intoxicated Subject",
  "Disorderly Subject",
  "Found Syringe",
  "Info / Atl",
  "Disoriented Subject",
  "Dispute",
  "Subject Down",
  "Disorderly Juveniles",
  "Suspicious Conditions",
  "Police Officer Hold"
)

top_mcslc_natures <- c(
  "Agitation Or Disruptive Behavior",
  "Disorganized Behavior",
  "Difficulty Functioning",
  "Needing Social/Mental Health Services",
  "Suicidal Subject",
  "Harm/Risk Of Harm To Self/Others/Property",
  "Adult Social/Interpersonal Problems",
  "Paranoia",
  "Substance Use",
  "Other",
  "Concerns About Treatment Engagement",
  "Trauma",
  "Running Away",
  "Seeking Mental Health Services"
)

# ============ plot ============ #

glimpse(data)

df_analysis <- data %>%
  filter(
    source != "SPD",
    nature %in% c(top_cahoots_natures, top_mcslc_natures)
  ) %>%
  mutate(
    period = case_when(
      timestamp < ymd("2024-08-18") ~ "CAHOOTS",
      timestamp >= ymd("2024-08-18") & timestamp <= ymd("2025-04-07") ~ "BOTH",
      timestamp >= ymd("2025-04-08") ~ "MCSLC",
      TRUE ~ NA_character_
    ),
    responder = case_when(
      CAHOOTS == 1 & MCSLC == 0 & EPD == 0 ~ "CAHOOTS",
      CAHOOTS == 1 & MCSLC == 0 & EPD == 1 ~ "CAHOOTS & EPD",
      CAHOOTS == 0 & MCSLC == 1 & EPD == 0 ~ "MCSLC",
      CAHOOTS == 0 & MCSLC == 0 & EPD == 1 ~ "EPD",
      TRUE ~ "Mixed_Response"
    )
  ) %>%
  filter(!is.na(period), responder != "Mixed_Response")

# why does cahoots have no welfare check done?

df_analysis %>%
  filter(responder == "CAHOOTS") %>%
  count(outcome, sort = TRUE) %>%
  mutate(proportion = n / sum(n) * 100) %>%
  head(15)

# how is welfare check answered by cahoots?
df_analysis %>%
  filter(responder == "CAHOOTS", nature == "Check Welfare") %>%
  count(outcome, sort = TRUE) %>%
  mutate(proportion = n / sum(n) * 100) %>%
  head(15)
