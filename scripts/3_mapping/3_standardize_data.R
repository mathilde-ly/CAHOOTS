
# Standardize and merge data

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)
library(ggplot2)

# ============ Mapped Data ============ #

mcslc <- read_csv("data/processed/category_mcslc_outcome.csv")
spd <- read_csv("data/processed/category_spd_outcome.csv")
cad <- read_csv("data/processed/category_cad_outcome.csv")

glimpse(mcslc)
glimpse(cad)
glimpse(spd)


unique(spd$call_source)
unique(cad$closed_as)
unique(cad$arrival)

unique(mcslc$dispatch_status)
unique(cad$dispatch)

mcslc_filt <- mcslc %>%
  filter(dispatch_status == "Other") %>%
  head()

unique(mcslc_filt$call_outcome)

# ============ CAD bias ============ #
# remove calls where target  unable to locate or refused



# ============ Merge CAD + MCSLC ============ #



# ============ Trend analysis =========== #
# Trend of call outcome analysis with {CAHOOTS; MCS LC+CAHOOTS; MCSLC}

