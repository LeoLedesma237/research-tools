# Create a custom function to plot defective density distributions
plot_defectiveDensity <- function(data, design) {
  
  # Standardize variables needed for plotting
  plot_dat <- data %>%
    transmute(
      stimulus = !!design$stimulus,
      response = !!design$response,
      rt = !!design$rt,
      accuracy = if_else(
        rlang::eval_tidy(design$correct, data = data),
        "Correct",
        "Error"
      )
    )
  
  # Get response proportions within each stimulus
  response_props <- plot_dat %>%
    count(stimulus, response) %>%
    group_by(stimulus) %>%
    mutate(
      prop = n / sum(n)
    ) %>%
    select(-n) %>%
    ungroup()
  
  # Add response proportions to the trial-level data
  plot_dat <- plot_dat %>%
    left_join(
      response_props,
      by = c("stimulus", "response")
    )
  
  # Calculate defective densities
  density_dat <- plot_dat %>%
    group_by(stimulus, response, accuracy) %>%
    reframe(
      x = density(rt)$x,
      y = density(rt)$y * first(prop)
    )
  
  # Plot defective densities
  density_dat %>%
    ggplot(
      aes(
        x = x,
        y = y,
        fill = accuracy,
        color = accuracy
      )
    ) +
    geom_area(
      alpha = 0.4,
      position = "identity"
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    facet_wrap(~stimulus) +
    scale_fill_manual(
      values = c(
        "Correct" = "#2b5c8f",
        "Error" = "#d95f02"
      )
    ) +
    scale_color_manual(
      values = c(
        "Correct" = "#2b5c8f",
        "Error" = "#d95f02"
      )
    ) +
    labs(
      title = "Defective Reaction Time Density Plot",
      subtitle = "Area under each curve reflects overall response proportion (sums to 1.0)",
      x = "Reaction Time (s)",
      y = "Defective Density",
      fill = "Response",
      color = "Response"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}