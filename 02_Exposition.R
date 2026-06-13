set_theme(theme_bw(base_size = 16, base_family = "Barlow"))

flights_per_day <- full_flight_df |>
  summarize(flights_per_day = n(), .by = c(year, month, day)) |>
  mutate(ymd = make_date(year = year, month = month, day = day), .before = 1) |>
  select(-c(year, month, day))

plot_over_time <- flights_per_day |>
  ggplot() +
  aes(x = ymd, y = flights_per_day) +
  geom_line(color = "#5c4cbf") +
  geom_vline(
    xintercept = ymd("2021-07-01"),
    linetype = "dashed",
    linewidth = .8,
    color = "#b3114b"
  ) +
  geom_vline(
    xintercept = ymd("2025-07-01"),
    linetype = "dashed",
    linewidth = .8,
    color = "#b3114b"
  ) +
  labs(
    title = "Effect of Covid on Flights Departing LAX",
    y = "Flights per Day",
    x = NULL
  ) +
  scale_x_date(expand = expansion(mult = c(.02, .02))) +
  canvas(width = 8, height = 6)

train_set <- flights_per_day |>
  filter(ymd >= ymd("2021-07-01") & ymd < ymd("2025-07-01"))

test_set <- flights_per_day |>
  filter(ymd >= ymd("2025-07-01"))
