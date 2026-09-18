# Create a custom function to plot defective densities for Go/No-Go data
plot_defectiveDensityGNG <- function(data, design) {
  
  # Standardize variables needed for plotting
  plot_dat <- data %>%
    transmute(
      stimulus = !!design$stimulus,
      response = !!design$response,
      rt = !!design$rt
    )
  
  # Calculate probability of an observed response for each stimulus
  response_props <- plot_dat %>%
    group_by(stimulus) %>%
    summarise(
      prop = mean(!is.na(rt)),
      .groups = "drop"
    )
  
  # Calculate RT density for trials with an observed response
  density_dat <- plot_dat %>%
    filter(!is.na(rt)) %>%
    group_by(stimulus) %>%
    group_modify(~ {
      
      d <- density(.x$rt)
      
      data.frame(
        x = d$x,
        y = d$y
      )
      
    }) %>%
    ungroup() %>%
    left_join(
      response_props,
      by = "stimulus"
    ) %>%
    mutate(
      y = y * prop
    )
  
  # Plot defective densities
  density_dat %>%
    ggplot(
      aes(
        x = x,
        y = y,
        fill = factor(stimulus),
        color = factor(stimulus)
      )
    ) +
    geom_area(
      alpha = 0.4,
      position = "identity"
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    scale_fill_manual(
      values = c(
        "#2b5c8f",
        "#d95f02"
      )
    ) +
    scale_color_manual(
      values = c(
        "#2b5c8f",
        "#d95f02"
      )
    ) +
    labs(
      title = "Go/No-Go Defective Reaction Time Density Plot",
      subtitle = "Area under each curve reflects the probability of an observed response",
      x = "Reaction Time (s)",
      y = "Defective Density",
      fill = "Stimulus",
      color = "Stimulus"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
}