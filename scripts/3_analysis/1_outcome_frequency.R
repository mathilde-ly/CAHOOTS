
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
top_natures_c <- data %>%
  filter(CAHOOTS == 1, EPD == 0 ) %>%
  count(nature, sort = TRUE) %>%
  slice_head(n = 500)

# MCSLC
top_natures_m <- data %>%
  filter(MCSLC == 1 ) %>%
  count(nature, sort = TRUE) %>%
  slice_head(n = 500)


print(top_natures_c)
print(top_natures_m)

# check where Assist Police's source
data %>%
  filter(nature == "Assist Police") %>%
  count(nature, EPD, CAHOOTS, SPD) %>%
  arrange()

# ============ plot ============ #

