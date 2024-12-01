library(lubridate)
library(dplyr)

patterns <- function(basic_table) {


# Create additional columns for time-based analysis
  patterns <- basic_table %>%
  mutate(
    hour_of_day = hour(crash_ts),
    day_of_week = wday(crash_ts, label = TRUE),
    day_of_year = yday(crash_ts),
    season = case_when(
      month(crash_ts) %in% c(12, 1, 2) ~ "Winter",
      month(crash_ts) %in% c(3, 4, 5) ~ "Spring",
      month(crash_ts) %in% c(6, 7, 8) ~ "Summer",
      month(crash_ts) %in% c(9, 10, 11) ~ "Fall",
      TRUE ~ "Unknown"
    ),
    day_of_week = wday(crash_ts, label = TRUE, week_start = 1)
  )


  return(patterns)
}
