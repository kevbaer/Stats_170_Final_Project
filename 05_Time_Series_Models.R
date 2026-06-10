arima_auto_reg <- arima_reg() |>
  set_engine("auto_arima")

arima_auto_fit <- arima_auto_reg |>
  fit(flights_per_day ~ ymd, data = train_set)

arima_auto_fit

prophet_reg <- prophet_reg() |>
  set_engine("prophet")

prophet_fit <- prophet_reg |>
  fit(flights_per_day ~ ymd, data = train_set)

prophet_fit

ets_reg <- exp_smoothing() |>
  set_engine("ets")

ets_fit <- ets_reg |>
  fit(flights_per_day ~ ymd, data = train_set)

ets_fit
