explainer <- explain_tidymodels(
  xgb_fit_nu_2,
  data = bind_rows(lag_train_xgb, lag_test_xgb) |>
    select(-c(flights_per_day, ymd)),
  y = as.numeric(as.character(
    bind_rows(lag_train_xgb, lag_test_xgb)$flights_per_day
  )),
  label = "xgb",
  verbose = FALSE,
) |>
  model_parts()

kb_ggplot_imp <- function(num = 100, ...) {
  {
    obj <- list(...)
    metric_name <- attr(obj[[1]], "loss_name")
    metric_lab <- "Permutation Based Variable Importance"

    full_vip <- bind_rows(obj) %>%
      filter(variable != "_baseline_") |>
      filter(variable != "_full_model_") %>%
      summarize(dropout_loss = mean(dropout_loss), .by = variable) |>
      mutate(variable = fct_reorder(variable, dropout_loss)) |>
      slice_max(dropout_loss, n = num)

    full_vip |>
      ggplot() +
      aes(dropout_loss, variable) +
      geom_col(fill = "#940034") +
      scale_x_continuous(expand = expansion(mult = (c(0.005, 0.06)))) +
      theme(legend.position = "none") +
      labs(
        x = metric_lab,
        y = NULL,
        fill = NULL,
        color = NULL
      ) +
      theme_bw(base_size = 16, base_family = "Barlow")
  }
}

y_axis_labels <- c(
  "Day of the Week: Saturday",
  "Day of the Year",
  "Day of the Week: Tuesday",
  "One Day Lag",
  "Day of the Week: Wednesday",
  "Surprise Day Off (NYSE Shutdown)",
  "One Year Lag",
  "Days till Surprise Day Off",
  "Seconds since the Epoch",
  "Day of the Week: Thursday"
)

plot_vip <- kb_ggplot_imp(explainer, num = 10) +
  ggtitle(
    "What Does the XGBoost Model Rely On?",
    subtitle = "Variable Importance Plot (Top 10 Features)"
  ) +
  labs(
    x = "RMSE with Permutation of Column"
  ) +
  theme(
    plot.title = element_text(size = 22),
    plot.title.position = "plot"
  ) +
  scale_y_discrete(
    labels = rev(y_axis_labels)
  ) +
  canvas(10, 8)
