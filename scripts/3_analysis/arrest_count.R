
# Arrest count during overlap period

mcslc_window_start <- as.POSIXct("2024-08-18")
mcslc_window_end   <- as.POSIXct("2025-12-17")

windowed <- data %>%
  filter(timestamp >= mcslc_window_start,
         timestamp <= mcslc_window_end)

arrests_mcslc <- windowed %>%
  filter(MCSLC == 1) %>%
  summarise(
    total_calls = n(),
    arrests     = sum(outcome == "Arrest", na.rm = TRUE),
    arrest_rate = arrests / total_calls
  )

arrests_cahoots <- windowed %>%
  filter(CAHOOTS == 1, source == "CAD") %>%
  summarise(
    total_calls = n(),
    arrests     = sum(outcome == "Arrest", na.rm = TRUE),
    arrest_rate = arrests / total_calls
  )


results <- bind_rows(
  arrests_mcslc   %>% mutate(agency = "MCS LC"),
  arrests_cahoots %>% mutate(agency = "CAHOOTS (CAD)")
) %>%
  select(agency, total_calls, arrests, arrest_rate) %>%
  mutate(arrest_rate = scales::percent(arrest_rate, accuracy = 0.01))

print(results)