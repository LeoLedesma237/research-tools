# Create a custom function to compare GNG defective density distributions
plot_defectiveDensityComparisonGNG <- function(data, ref_data, design, x_max = NULL) {
  
  # Standardize variables needed for plotting
  prepare_data <- function(dat, dataset) {
    
    dat %>%
      transmute(
        stimulus = !!design$stimulus,
        response = !!design$response,
        rt = !!design$rt,
        dataset = dataset
      )
  }
  
  # Prepare and combine datasets
  plot_dat <- bind_rows(
    prepare_data(data, "Data"),
    prepare_data(ref_data, "Reference")
  )
  
  # Get response proportions within each stimulus and dataset
  response_props <- plot_dat %>%
    group_by(dataset, stimulus) %>%
    summarise(
      prop = mean(response == "RESP"),
      .groups = "drop"
    )
  
  # Keep trials with an observable response
  plot_dat <- plot_dat %>%
    filter(response == "RESP") %>%
    left_join(
      response_props,
      by = c("dataset", "stimulus")
    )
  
  # Calculate defective densities
  density_dat <- plot_dat %>%
    group_by(dataset, stimulus) %>%
    reframe(
      x = density(rt, na.rm = TRUE)$x,
      y = density(rt, na.rm = TRUE)$y * first(prop)
    )
  
  # Plot defective densities
  p <- density_dat %>%
    ggplot(
      aes(
        x = x,
        y = y,
        fill = dataset,
        color = dataset
      )
    ) +
    geom_area(
      alpha = 0.25,
      position = "identity"
    ) +
    geom_line(
      linewidth = 0.8
    ) +
    facet_wrap(
      ~ stimulus,
      nrow = 1
    ) +
    labs(
      title = "Go/No-Go Defective Reaction Time Density Comparison",
      subtitle = "Area under each curve reflects response proportion within each dataset",
      x = "Reaction Time (s)",
      y = "Defective Density",
      fill = "Dataset",
      color = "Dataset"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      panel.grid.minor = element_blank()
    )
  
  # Optionally restrict the displayed x-axis range
  if (!is.null(x_max)) {
    p <- p +
      coord_cartesian(xlim = c(0, x_max))
  }
  
  return(p)
}