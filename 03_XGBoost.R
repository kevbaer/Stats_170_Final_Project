set.seed(11042004)
mirai::daemons(8)

plain_train_xgb <- train_set |>
  tk_augment_timeseries_signature(ymd) |>
  tk_augment_holiday_signature(
    ymd,
    .holiday_pattern = "$^",
    .locale_set = "none",
    .exchange_set = "NYSE"
  ) |>
  select(
    ymd,
    flights_per_day,
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
  mutate(across(c(year:wday.lbl), as.character)) |>
  dummy_cols(
    remove_selected_columns = TRUE,
    remove_most_frequent_dummy = TRUE
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
  select(-c(current_pos, target_positions))

xgb_folds <- time_series_cv(
  plain_train_xgb,
  date_var = ymd,
  initial = 10,
  cumulative = TRUE,
  assess = 184,
  slice_limit = 25
)

plain_train_xgb <- plain_train_xgb |>
  select(-ymd)

rec_xgb <- recipe(flights_per_day ~ ., data = plain_train_xgb)

xgb_spec <- boost_tree(
  mode = "regression",
  engine = "xgboost",
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  trees = tune()
)

wf_xgb <- workflow() |>
  add_recipe(rec_xgb) |>
  add_model(xgb_spec)


tune_res_xgb <- tune_grid(
  wf_xgb,
  xgb_folds,
  grid = 50,
  control = control_grid(parallel_over = "everything"),
  metrics = metric_set(rmse)
)
