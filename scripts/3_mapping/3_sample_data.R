
# Create samples of CSV data for performance purposes while cleaning

# ============ Libraries ============ #

library(tidyverse)

# ============ Data ============ #

mcslc <- read_csv("data/processed/mcs_lc.csv")
spd <- read_csv("data/processed/spd_2015_2025.csv")
eugene_cad <- read_csv("data/processed/eugene_cad_2015_2025.csv")

# ============ Sample Data ============ #

colnames(mcslc)

n_sample <- 500

get_temporal_sample <- function(data) {
  data %>%
    slice_sample(n = n_sample) %>%
    arrange(timestamp)
}

spd_sample   <- get_temporal_sample(spd)
eugene_sample <- get_temporal_sample(eugene_cad)

get_temporal_sample <- function(data) {
  data %>%
    slice_sample(n = n_sample) %>%
    arrange("Dispatch Request Date & Time")
}

mcslc_sample <- get_temporal_sample(mcslc)


write_csv(mcslc_sample, "data/processed/samples/mcslc_sample.csv")
write_csv(spd_sample, "data/processed/samples/spd_sample.csv")
write_csv(eugene_sample, "data/processed/samples/eugene_cad_sample.csv")
