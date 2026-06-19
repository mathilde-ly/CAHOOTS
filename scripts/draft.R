
glimpse(data)

data %>%
  filter(MCSLC == 1) %>%
  count(outcome)

data_temporal %>% 
  filter(CAHOOTS == 1) %>%
  count(nature, outcome) %>%
  slice_max(n, n = 20)

data %>% 
  filter(source == "SPD" | source == "CAD") %>%  
  filter(as.integer(str_detect(str_to_lower(outcome), "arrest|custody|cited")) == 1) %>%  
  count(source, outcome)


data %>%
  filter(
    city %in% c("Eugene", "Springfield"),
    timestamp >= as.POSIXct("2023-01-01"),
    nature %in% top_14_cahoots_natures      
  ) %>%
  count(outcome) %>%
  arrange(n)


data %>%
  filter(EPD == 1) %>%
  count(outcome) %>%
  arrange()
