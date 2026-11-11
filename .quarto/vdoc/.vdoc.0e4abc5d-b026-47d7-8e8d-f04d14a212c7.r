#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(readxl)
#
#
#
#| cache: true
births_tibble <- read_excel("data/us_births_1994_2014.xlsx") |>
  mutate(day_of_week = factor(
    day_of_week,
    levels = c("Sun", "Mon", "Tues", "Wed", "Thurs", "Fri", "Sat"),
    ordered = TRUE
  ))
#
#
#
births_by_calendar_position <- births_tibble |>
  group_by(month, date_of_month) |>
  summarise(average_births = mean(births), .groups = "drop")
#
#
#
#
