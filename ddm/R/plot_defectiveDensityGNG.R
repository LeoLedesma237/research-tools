# Create a custom function to plot defective densities for Go/No-Go data
plot_defectiveDensityGNG <- function(data, design, facet = NULL, xmax = NULL) {
  
  # Capture optional faceting variable(s)
  facet_quo <- rlang::enquo(facet)
  use_facet <- !rlang::quo_is_null(facet_quo)
  
  # Extract facet variable names
  if (use_facet) {
    facet_names <- tidyselect::eval_select(
      facet_quo,
      data = data
    ) %>%
      names()
  } else {
    facet_names <- character(0)
  }
  
  # Standardize variables needed for plotting
  plot_dat <- data %>%
    transmute(
      stimulus = !!design$stimulus,
      response = !!design$response,
      rt = !!design$rt,
      across(all_of(facet_names))
    )
  
  # Calculate probability of an observed response
  response_props <- plot_dat %>%
    group_by(
      across(all_of(c(facet_names, "stimulus")))
    ) %>%
    summarise(
      prop = mean(!is.na(rt)),
      .groups = "drop"
    )
  
  # Calculate RT density for trials with an observed response
  density_dat <- plot_dat %>%
    filter(!is.na(rt)) %>%
    group_by(
      across(all_of(c(facet_names, "stimulus")))
    ) %>%
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
      by = c(facet_names, "stimulus")
    ) %>%
    mutate(
      y = y * prop
    )
  
  # Create defective density plot
  p <- density_dat %>%
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
  
  # Add facets if requested
  if (use_facet) {
    p <- p +
      facet_wrap(
        vars(!!!rlang::syms(facet_names))
      )
  }
  
  # Zoom x-axis if requested
  if (!is.null(xmax)) {
    p <- p +
      coord_cartesian(
        xlim = c(0, xmax)
      )
  }
  
  return(p)
}