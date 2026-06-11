non_xgb_revs <- modeltime_table(
  arima_auto_fit,
  prophet_fit,
  ets_fit
) |>
  modeltime_forecast(h = 184, actual_data = train_set)

arima_preds <- non_xgb_revs |>
  filter(.model_id == 1) |>
  select(.index, arima_pred = .value)

prophet_preds <- non_xgb_revs |>
  filter(.model_id == 2) |>
  select(.index, prophet_pred = .value)

ets_preds <- non_xgb_revs |>
  filter(.model_id == 3) |>
  select(.index, ets_pred = .value)

xgb_preds_overlap_folds <- plain_test_xgb |>
  bind_cols(predict(xgb_fit, new_data = plain_test_xgb)) |>
  select(ymd, xgb_overlap_pred = .pred)

xgb_preds_independent_folds <- plain_test_xgb |>
  bind_cols(predict(xgb_fit_nu, new_data = plain_test_xgb)) |>
  select(ymd, xgb_independent_pred = .pred)

lagged_xgb_preds_overlap_folds <- rep(NA, 184)
lag_test_xgb_updating <- lag_test_xgb

for (i in 1:184) {
  lagged_xgb_preds_overlap_folds[i] <- predict(
    xgb_fit_2,
    new_data = lag_test_xgb_updating[i, ]
  ) |>
    pull()

  lag_test_xgb_updating[i + 1, "lag_1_day"] <- lagged_xgb_preds_overlap_folds[
    i
  ] |>
    as.integer()
}

lagged_xgb_preds_independent_folds <- rep(NA, 184)
lag_test_xgb_updating <- lag_test_xgb

for (i in 1:184) {
  lagged_xgb_preds_independent_folds[i] <- predict(
    xgb_fit_nu_2,
    new_data = lag_test_xgb_updating[i, ]
  ) |>
    pull()

  lag_test_xgb_updating[
    i + 1,
    "lag_1_day"
  ] <- lagged_xgb_preds_independent_folds[i] |>
    as.integer()
}

pred_df <- test_set |>
  rename(truth = flights_per_day) |>
  left_join(
    arima_preds,
    by = join_by(ymd == .index)
  ) |>
  left_join(
    prophet_preds,
    by = join_by(ymd == .index)
  ) |>
  left_join(
    ets_preds,
    by = join_by(ymd == .index)
  ) |>
  left_join(
    xgb_preds_overlap_folds,
    by = join_by(ymd)
  ) |>
  left_join(
    xgb_preds_independent_folds,
    by = join_by(ymd)
  ) |>
  bind_cols(
    lagged_xgb_overlap_pred = lagged_xgb_preds_overlap_folds,
    lagged_xgb_independent_pred = lagged_xgb_preds_independent_folds
  )
