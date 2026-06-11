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
