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
ggplot(
  births_by_calendar_position,
  aes(
    x = date_of_month,
    y = factor(month.abb[month], levels = rev(month.abb)),
    fill = average_births
  )
) +
  geom_tile(color = "white", linewidth = 0.3) +
  scale_x_continuous(
    name = "Day of month",
    breaks = 1:31,
    limits = c(0.5, 31.5),
    expand = c(0, 0)
  ) +
  scale_y_discrete(name = NULL, drop = FALSE) +
  scale_fill_viridis_c(name = "Mean births") +
  labs(title = "Average births by calendar date") +
  theme_minimal()
#
#
#
christmas_by_year <- births_tibble |>
  filter(month == 12, date_of_month == 25) |>
  transmute(year, dec_25_births = births) |>
  left_join(
    births_tibble |>
      filter(month == 12, date_of_month %in% c(20:24, 27:30)) |>
      group_by(year) |>
      summarise(baseline_births = mean(births), .groups = "drop"),
    by = "year"
  ) |>
  mutate(percent_of_baseline = 100 * dec_25_births / baseline_births) |>
  arrange(year)

knitr::kable(
  christmas_by_year,
  digits = 1,
  col.names = c(
    "Year",
    "Dec 25 births",
    "Average births on Dec 20-24 and 27-30",
    "Dec 25 (% of baseline)"
  )
)
#
#
#
#
#
