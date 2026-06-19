
# ============ Libraries ============ #

library(tidyverse)
library(dplyr)

library(readxl)
library(purrr)
library(janitor)
library(lubridate)
library(hms)

library(stargazer)

# ============ DATA ============ #
data <- readRDS("data/clean/data_merged.rds")  

# =========== reg =========== #

# On isole une issue (ex: Arrestation) avec une variable 0 ou 1
df_binaire <- df_grouped %>%
  mutate(is_arrest = if_else(outcome_4cat == "Arrest", 1, 0))

# glm = Generalized Linear Model
# family = binomial indique à R que Y est binaire (0/1)
modele_binaire <- glm(
  is_arrest ~ responder + nature + priority + period, 
  data = df_binaire, 
  family = binomial(link = "logit")
)


