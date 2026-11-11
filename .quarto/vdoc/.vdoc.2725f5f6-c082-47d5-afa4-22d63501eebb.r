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
  transmute(year, day_of_week, dec_25_births = births) |>
  left_join(
    births_tibble |>
      filter(month == 12, date_of_month %in% c(20:24, 27:30)) |>
      group_by(year) |>
      summarise(baseline_births = mean(births), .groups = "drop"),
    by = "year"
  ) |>
  mutate(pct_of_baseline = 100 * dec_25_births / baseline_births) |>
  arrange(year)

knitr::kable(
  christmas_by_year,
  digits = 1,
  col.names = c(
    "Year",
    "Weekday on Dec 25",
    "Dec 25 births",
    "Average births on Dec 20-24 and 27-30",
    "Dec 25 (% of baseline)"
  )
)
#
#
#
ggplot(christmas_by_year, aes(x = year, y = pct_of_baseline)) +
  geom_line() +
  geom_point(aes(color = day_of_week)) +
  scale_color_brewer(palette = "Dark2", name = "Weekday on Dec 25") +
  labs(
    title = "December 25 Births as a Percentage of Baseline",
    x = "Year",
    y = "December 25 births (% of baseline)"
  ) +
  theme_minimal()
#
#
#
births_model <- lm(births ~ year + month + day_of_week, data = births_tibble)
cat("R-squared:", round(summary(births_model)$r.squared, 3))
#
#
#
births_adjusted <- births_tibble |>
  mutate(pct_resid = 100 * residuals(births_model) / mean(births))

calendar_resid <- births_adjusted |>
  filter(!(month == 2 & date_of_month == 29)) |>
  group_by(month, date_of_month) |>
  summarise(mean_pct_resid = mean(pct_resid), .groups = "drop") |>
  mutate(calendar_date = as.Date(sprintf("2001-%02d-%02d", month, date_of_month))) |>
  select(calendar_date, everything()) |>
  arrange(calendar_date)

calendar_resid
#
#
#
holiday_labels <- tibble(
  calendar_date = as.Date(c(
    "2001-01-01", "2001-07-04", "2001-12-24", "2001-12-25"
  )),
  holiday = c(
    "New Year's Day", "Independence Day", "Christmas Eve", "Christmas Day"
  ),
  label_date = as.Date(c(
    "2001-01-16", "2001-07-16", "2001-12-10", "2001-12-10"
  )),
  label_y = c(-20, -17, -25, -36)
)

holiday_dips <- calendar_resid |>
  inner_join(holiday_labels, by = "calendar_date")

ggplot(calendar_resid, aes(x = calendar_date, y = mean_pct_resid)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_line(color = "#287271") +
  geom_point(color = "#287271", size = 1.2) +
  geom_segment(
    data = holiday_dips,
    aes(
      x = calendar_date,
      xend = label_date,
      y = mean_pct_resid,
      yend = label_y
    ),
    inherit.aes = FALSE,
    color = "gray45",
    linewidth = 0.4
  ) +
  geom_label(
    data = holiday_dips,
    aes(x = label_date, y = label_y, label = holiday),
    inherit.aes = FALSE,
    size = 3
  ) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  labs(
    title = "Average Daily Birth Residuals Across the Calendar",
    x = "Calendar date (2001)",
    y = "Mean residual (% of average births)"
  ) +
  theme_minimal()
#
#
#
#
#
