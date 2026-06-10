set.seed(11042004)
mirai::daemons(8)

lag_train_xgb <- plain_train_xgb |>
  mutate(ymd = as_datetime(index) |> as_date(), .before = 1) |>
  mutate(lag_1_day = lag_vec(flights_per_day, lag = 1)) |>
  mutate(lag_1_year = lag_vec(flights_per_day, lag = 365))


xgb_folds_2 <- time_series_cv(
  lag_train_xgb,
  date_var = ymd,
  initial = 10,
  cumulative = TRUE,
  assess = 184,
  slice_limit = 25
)

nu_folds_2 <- sliding_period(
  lag_train_xgb,
  ymd,
  period = "month",
  lookback = Inf,
  assess_start = 1,
  assess_stop = 6,
  skip = 32
)

lag_train_xgb <- lag_train_xgb |>
  select(-ymd)

rec_xgb_2 <- recipe(flights_per_day ~ ., data = lag_train_xgb)

xgb_spec_2 <- boost_tree(
  mode = "regression",
  engine = "xgboost",
  tree_depth = tune(),
  learn_rate = tune(),
  loss_reduction = tune(),
  min_n = tune(),
  sample_size = tune(),
  trees = tune()
)

wf_xgb_2 <- workflow() |>
  add_recipe(rec_xgb_2) |>
  add_model(xgb_spec_2)

grid_rules_2 <- control_grid(parallel_over = "everything", save_workflow = TRUE)

tune_res_xgb_2 <- tune_grid(
  wf_xgb_2,
  xgb_folds_2,
  grid = 50,
  control = grid_rules_2,
  metrics = metric_set(rmse)
)

tune_res_xgb_2 |> show_best(metric = "rmse")

tune_res_xgb_nu_2 <- tune_grid(
  wf_xgb_2,
  nu_folds_2,
  grid = 50,
  control = grid_rules_2,
  metrics = metric_set(rmse)
)

tune_res_xgb_nu_2 |> show_best(metric = "rmse")

xgb_fit_2 <- fit_best(tune_res_xgb_2, metric = "rmse", verbose = TRUE)
xgb_fit_nu_2 <- fit_best(tune_res_xgb_nu_2, metric = "rmse", verbose = TRUE)
