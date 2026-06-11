plain_test_xgb <- test_set |>
  select(-flights_per_day) |>
  tk_augment_timeseries_signature(ymd) |>
  tk_augment_holiday_signature(
    ymd,
    .holiday_pattern = "$^",
    .locale_set = "none",
    .exchange_set = "NYSE"
  ) |>
  select(
    ymd,
    index = index.num,
    diff,
    year,
    half,
    quarter,
    month.lbl,
    wday.lbl,
    qday,
    yday,
    mweek,
    week,
    week2,
    week3,
    week4,
    mday7,
    surprise_day_off = exch_NYSE
  ) |>
  mutate(across(c(quarter:wday.lbl), as.character)) |>
  dummy_cols(
    remove_selected_columns = TRUE
  ) |>
  mutate(
    current_pos = row_number(),
    target_positions = list(which(surprise_day_off == 1)),

    days_till_surprise_day_off = map2_int(
      current_pos,
      target_positions,
      function(pos, targets) {
        future_targets <- targets[targets > pos]
        if (length(future_targets) == 0) {
          return(NA)
        }
        return(min(future_targets) - pos)
      }
    )
  ) |>
  select(-c(current_pos, target_positions)) |>
  mutate(
    year_2021 = 0L,
    year_2022 = 0L,
    year_2023 = 0L,
    year_2025 = 1L,
    half_1 = 1L,
    quarter_1 = 0L,
    quarter_2 = 0L,
    month.lbl_April = 0L,
    month.lbl_February = 0L,
    month.lbl_June = 0L,
    month.lbl_March = 0L,
    month.lbl_May = 0L,
    month.lbl_January = 0L
  ) |>
  select(-c(year, half, quarter_3, month.lbl_August, wday.lbl_Friday))

lag_test_xgb <- plain_train_xgb |>
  mutate(ymd = as_datetime(index) |> as_date(), .before = 1) |>
  bind_rows(plain_test_xgb) |>
  arrange(ymd) |>
  mutate(lag_1_day = lag_vec(flights_per_day, lag = 1)) |>
  mutate(lag_1_year = lag_vec(flights_per_day, lag = 365)) |>
  filter_out(ymd < ymd("2025-07-01")) |>
  select(-flights_per_day)
