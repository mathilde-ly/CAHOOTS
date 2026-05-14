
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
  )

data_mapped %>%
  count(outcome_category) %>%
  arrange(outcome_category)

# ============ visu ============ #

# Distribution des catégories de résultats
data_mapped %>%
  count(outcome_category) %>%
  mutate(outcome_category = reorder(outcome_category, -n)) %>%
  ggplot(aes(x = outcome_category, y = n, fill = outcome_category)) +
  geom_col(show.legend = FALSE) +
  scale_fill_brewer(palette = "Pastel1") +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Distribution of Call Outcome Categories",
    x = "Category",
    y = "Number of Calls"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold")
  )

# aggregation des donnees par mois et categorie
data_trends <- data_mapped %>%
  filter(outcome_category != "other") %>%
  mutate(month = floor_date(as.Date(timestamp), "month")) %>%
  count(month, outcome_category)

# visualisation des tendances temporelles
ggplot(data_trends, aes(x = month, y = n, color = outcome_category)) +
  geom_line(size = 1) +
  # transition markers
  geom_vline(xintercept = as.Date("2024-08-18"), color = "red", linetype = "dashed") +
  geom_vline(xintercept = as.Date("2025-04-07"), color = "red", linetype = "dashed") +
  # annotations
  annotate("text", x = as.Date("2024-08-18"), y = max(data_trends$n), 
           label = "MCS LC launched", angle = 90, vjust = -0.5, color = "red", size = 3) +
  annotate("text", x = as.Date("2025-04-07"), y = max(data_trends$n), 
           label = "CAHOOTS discontinued", angle = 90, vjust = -0.5, color = "red", size = 3) +
  scale_color_brewer(palette = "Pastel1") +
  labs(
    title = "Evolution of Call Outcomes Over Time",
    subtitle = "Impact of MCS LC launch and CAHOOTS discontinuation",
    x = "Date",
    y = "Number of Calls",
    color = "Category"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")
