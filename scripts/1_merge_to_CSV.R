
# R Script to open and merge datasets
# for MCS LC, SPD, and Eugene CAD data

# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

library(ggplot2)

# ============ MCS LC Data ============ #

mcs_lc_path <- "data/raw/MCSLC.xlsx"
mcslc <- readxl::read_excel(mcs_lc_path, sheet = "Abbrev list")

write_csv(mcslc, "data/processed/mcs_lc.csv")

# ============ SPD Data ============ #

# --- Util ---
harmonize_types <- function(path, sheet_name) {
  read_excel(path, sheet = sheet_name) %>%
    mutate(across(everything(), as.character))
}

# --- SPD Calls for Service ---
spd_calls_path <- "data/raw/2015-2025 SPD Calls for Service.xlsx"
calls_sheets <- excel_sheets(spd_calls_path)

spd_calls_raw <- calls_sheets %>%
  set_names() %>%
  map_df(~harmonize_types(spd_calls_path, .x), .id = "Year")

saveRDS(spd_calls_raw, "data/intermediate/spd_calls_raw.rds")

# --- SPD Responding Units ---
spd_response_path <- "data/raw/2015-2025 SPD Responding Units.xlsx"
response_sheets <- excel_sheets(spd_response_path)[-1] # Exclure la première feuille

spd_response_raw <- response_sheets %>%
  set_names() %>%
  map_df(~harmonize_types(spd_response_path, .x), .id = "Year")

saveRDS(spd_response_raw, "data/intermediate/spd_response_raw.rds")

# --- SPD Close Codes ---
spd_codes_path <- "data/raw/2015-2025 SPD Calls with Close Codes.xlsx"
codes_sheets <- excel_sheets(spd_codes_path)

spd_codes_raw <- codes_sheets %>%
  set_names() %>%
  map_df(~harmonize_types(spd_codes_path, .x), .id = "Year")
saveRDS(spd_codes_raw, "data/intermediate/spd_codes_raw.rds")

spd_defs_raw <- read_excel(spd_codes_path, sheet = 1) %>%
  mutate(across(everything(), as.character))
saveRDS(spd_defs_raw, "data/intermediate/spd_defs_raw.rds")

# ============ Eugene CAD Data ============ #

eugene_cad_path <- "data/raw/Eugene_CAD_data_noloc/"
output_raw_path <- "data/intermediate/eugene_cad_raw.rds"

eugene_cad_files <- list.files(
  path = eugene_cad_path, 
  pattern = "\\.csv$", 
  full.names = TRUE
)

cad_raw <- eugene_cad_files %>%
  map_df(~read_csv(.x, col_types = cols(.default = "c")))

saveRDS(cad_raw, output_raw_path)


# ============ Read CSV ============ #

mcslc <- read_csv("data/processed/mcs_lc.csv")
spd <- read_csv("data/processed/spd_2015_2025.csv")
eugene_cad <- read_csv("data/processed/eugene_cad_2015_2025.csv")
