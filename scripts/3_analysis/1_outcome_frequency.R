
# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

c15 <- c(

  "#acff59",
  "#0008ff",
  "#55c25e", 
  "#a9c3d9", 
  "#ff0000",
  "#a6a6a6", 
  "#ffee7d",
  "#6e4832",
  "#38a9d6", 
  "#c3e8be", 
  "#626ce3",
  "#ffb300", 
  "#e494f2",
  "#6b555e",
  "#a58bd6",
  "#ea00ff"
)
pie(rep(1, 25), col = c15)

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


# plot 1 - top call outcomes by responding agency

outcome_summary <- df_analysis %>%
  count(period, responder, outcome) %>%
  group_by(period, responder) %>%
  mutate(proportion = n / sum(n)) %>%
  slice_max(order_by = n, n = 5) %>%
  ungroup()

ggplot(outcome_summary, aes(x = responder, y = proportion, fill = outcome)) +
  geom_col(position = "fill", alpha = 0.9) +
  facet_wrap(~ period, scales = "free_x") +
  scale_fill_manual(values = c15) +
  theme_minimal() +
  labs(
    title = "Top Call Outcomes by Responding Agency Across Implementation Periods",
    x = "Responding Agency",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(face = "bold")
  )



