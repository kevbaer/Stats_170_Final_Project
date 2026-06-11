long_pred_df <- pred_df |>
  pivot_longer(
    cols = -c(ymd, truth),
    values_to = "estimate",
    names_to = "model"
  )

results <- long_pred_df |>
  group_by(model) |>
  rmse(truth = truth, estimate = estimate) |>
  arrange(.estimate)

results

scores <- results$.estimate |> round(digits = 2)

labs <- rev(c(
  "ETS",
  "Auto-ARIMA",
  "Prophet",
  "Lagged XGBoost with Overlapping Folds",
  "XGBoost with Overlapping Folds",
  "XGBoost with Distinct Folds",
  "Lagged XGBoost with Distinct Folds"
))

plot_results <- results |>
  mutate(model = fct_rev(fct_reorder(model, .estimate))) |>
  ggplot() +
  aes(x = .estimate, y = model) +
  geom_col(fill = "#940034") +
  labs(
    title = "Model RMSE on Unseen Test Data (2025/07/01 - 2025/12/31)",
    y = NULL,
    x = "RMSE (Smaller is Better)"
  ) +
  scale_x_continuous(expand = expansion(mult = c(0.004, 0.03))) +
  theme(
    plot.title.position = "plot",
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  geom_richtext(
    aes(x = .3, label = labs),
    family = "Barlow",
    fontface = "bold",
    label.color = NA,
    fill = NA,
    color = "#f8f9fa",
    hjust = 0,
    size = 5
  ) +
  geom_richtext(
    aes(label = scores, x = .estimate - 2.75),
    family = "Barlow",
    fontface = "bold",
    label.color = NA,
    fill = NA,
    color = "#f8f9fa",
    hjust = 0,
    size = 5
  ) +
  canvas(width = 10, height = 6)

labs <- c(
  "truth" = "Truth",
  "lagged_xgb_independent_pred" = "Lagged XGB (Distinct)",
  "xgb_independent_pred" = "XGB (Distinct)",
  "xgb_overlap_pred" = "XGB (Overlap)",
  "lagged_xgb_overlap_pred" = "Lagged XGB (Overlap)",
  "prophet_pred" = "Prophet",
  "arima_pred" = "Auto-Arima",
  "ets_pred" = "ETS"
)

plot_prediction <- pred_df |>
  pivot_longer(
    cols = -ymd,
    values_to = "estimate",
    names_to = "model"
  ) |>
  mutate(
    model = factor(
      model,
      levels = c(
        "truth",
        "lagged_xgb_independent_pred",
        "xgb_independent_pred",
        "xgb_overlap_pred",
        "lagged_xgb_overlap_pred",
        "prophet_pred",
        "arima_pred",
        "ets_pred"
      )
    )
  ) |>
  ggplot() +
  aes(x = ymd, y = estimate) +
  geom_line(linewidth = 0.5, color = "#002c55") +
  facet_wrap2(
    ~model,
    nrow = 2,
    strip = strip_themed(
      background_x = elem_list_rect(fill = c("#8ee7af", rep("#fecdd4", 7)))
    ),
    labeller = labeller(model = labs)
  ) +
  canvas(width = 10, height = 6) +
  labs(title = "Model Prediction Analysis", x = "Date", y = "Estimate") +
  theme(strip.text = element_text(size = 16)) +
  theme(panel.spacing.x = unit(.85, "lines"))
