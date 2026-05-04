# ============ Libraries ============ #

library(tidyverse)
library(dplyr)
library(ggplot2)

# ============ Mapped Data ============ #

cad <- read_csv("data/processed/category_cad_outcome.csv")

glimpse(cad)

unique(cad$nature, sort = TRUE)

cad %>%
  filter(agency == "CAHOOTS") %>%
  count(prime_unit, sort = TRUE)

unit_agency_analysis <- cad_raw %>%
  group_by(primeunit) %>%
  summarize(
    nb_interventions = n(),                      # Nombre total d'occurrences
    nb_agencies = n_distinct(agency),            # Nombre d'agences uniques pour cette unité
    agencies_list = paste(unique(agency), collapse = ", ") # Liste des agences concernées
  ) %>%
  arrange(desc(nb_interventions))

# Affichage du résultat
print(unit_agency_analysis, n = 1822)


glimpse(cad_raw)
