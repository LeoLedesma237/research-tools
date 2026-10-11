plot_defectiveDensityGNG <- function(
    data,
    design,
    facet = NULL,
    xmax = NULL,
    individual = FALSE,
    min_rt = 10,
    alpha = 0.10
) {
  
  # Capture optional faceting variables
  facet_quo <- rlang::enquo(facet)
  use_facet <- !rlang::quo_is_null(facet_quo)
  
  if (use_facet) {
    facet_names <- names(
      tidyselect::eval_select(facet_quo, data = data)
    )
  } else {
    facet_names <- character(0)
  }
  
  # Validate arguments
  if (individual) {
    
    if (is.null(design$id)) {
      stop("design$id must identify the participant column.")
    }
    
    if (
      length(min_rt) != 1 ||
      is.na(min_rt) ||
      !is.finite(min_rt) ||
      min_rt < 2 ||
      min_rt != floor(min_rt)
    ) {
      stop("min_rt must be an integer greater than or equal to 2.")
    }
    
    if (
      length(alpha) != 1 ||
      is.na(alpha) ||
      !is.finite(alpha) ||
      alpha < 0 ||
      alpha > 1
    ) {
      stop("alpha must be between 0 and 1.")
    }
  }
  
  # Standardize variables
  plot_dat <- data %>%
    transmute(
      stimulus = !!design$stimulus,
      response = !!design$response,
      rt = !!design$rt,
      across(all_of(facet_names))
    )
  
  # Add participant identifiers when requested
  if (individual) {
    plot_dat$.subject <- rlang::eval_tidy(
      design$id,
      data = data
    )
    
    if (anyNA(plot_dat$.subject)) {
      stop("Participant identifiers contain missing values.")
    }
  }
  
  # Aggregate grouping variables
  group_vars <- c(facet_names, "stimulus")
  
  # Calculate aggregate response probabilities
  response_props <- plot_dat %>%
    group_by(across(all_of(group_vars))) %>%
    summarise(
      prop = mean(!is.na(rt)),
      .groups = "drop"
    )
  
  # Calculate aggregate defective densities
  density_dat <- plot_dat %>%
    filter(!is.na(rt)) %>%
    group_by(across(all_of(group_vars))) %>%
    group_modify(~ {
      
      if (
        nrow(.x) < 2 ||
        dplyr::n_distinct(.x$rt) < 2
      ) {
        return(
          data.frame(
            x = numeric(),
            y = numeric()
          )
        )
      }
      
      d <- density(.x$rt)
      
      data.frame(
        x = d$x,
        y = d$y
      )
      
    }) %>%
    ungroup() %>%
    left_join(
      response_props,
      by = group_vars
    ) %>%
    mutate(
      y = y * prop
    )
  
  # Plot settings
  colors <- c("#2b5c8f", "#d95f02")
  
  p <- ggplot() +
    scale_fill_manual(values = colors) +
    scale_color_manual(values = colors) +
    labs(
      title = "Go/No-Go Defective Reaction Time Density Plot",
      subtitle = paste(
        "Area under each curve reflects",
        "the probability of an observed response"
      ),
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
  
  if (individual) {
    
    # Individual grouping variables
    individual_vars <- c(
      facet_names,
      ".subject",
      "stimulus"
    )
    
    # Calculate individual response probabilities
    # and determine which curves have sufficient RTs
    individual_props <- plot_dat %>%
      group_by(across(all_of(individual_vars))) %>%
      summarise(
        prop = mean(!is.na(rt)),
        n_rt = sum(!is.na(rt)),
        n_unique_rt = n_distinct(rt[!is.na(rt)]),
        .groups = "drop"
      ) %>%
      mutate(
        eligible = n_rt >= min_rt &
          n_unique_rt >= 2
      )
    
    # Calculate individual RT densities first,
    # then join response probabilities
    individual_density <- plot_dat %>%
      inner_join(
        individual_props %>%
          filter(eligible) %>%
          select(all_of(individual_vars)),
        by = individual_vars
      ) %>%
      filter(!is.na(rt)) %>%
      group_by(across(all_of(individual_vars))) %>%
      group_modify(~ {
        
        d <- density(.x$rt)
        
        data.frame(
          x = d$x,
          y = d$y
        )
        
      }) %>%
      ungroup() %>%
      left_join(
        individual_props %>%
          select(all_of(c(individual_vars, "prop"))),
        by = individual_vars
      ) %>%
      mutate(
        y = y * prop
      )
    
    # Summarize excluded individual curves
    exclusions <- individual_props %>%
      group_by(stimulus) %>%
      summarise(
        omitted = sum(!eligible),
        total = n(),
        .groups = "drop"
      )
    
    exclusion_text <- paste(
      sprintf(
        "%s: %d/%d omitted",
        as.character(exclusions$stimulus),
        exclusions$omitted,
        exclusions$total
      ),
      collapse = "; "
    )
    
    # Plot individual curves and aggregate curves
    p <- p +
      
      geom_line(
        data = individual_density,
        aes(
          x = x,
          y = y,
          color = factor(stimulus),
          group = interaction(.subject, stimulus)
        ),
        alpha = alpha,
        linewidth = 0.35,
        show.legend = FALSE
      ) +
      
      geom_line(
        data = density_dat,
        aes(
          x = x,
          y = y,
          color = factor(stimulus),
          group = stimulus
        ),
        linewidth = 1.3
      ) +
      
      labs(
        caption = paste0(
          "Individual curves require at least ",
          min_rt,
          " observed RTs and 2 distinct RT values. ",
          "Omitted: ",
          exclusion_text,
          ". Counts are per subject and facet."
        )
      ) +
      
      theme(
        plot.caption = element_text(
          size = 9,
          color = "grey40",
          hjust = 0
        )
      )
    
  } else {
    
    # Original shaded aggregate densities
    p <- p +
      
      geom_area(
        data = density_dat,
        aes(
          x = x,
          y = y,
          fill = factor(stimulus),
          group = stimulus
        ),
        alpha = 0.4,
        position = "identity"
      ) +
      
      geom_line(
        data = density_dat,
        aes(
          x = x,
          y = y,
          color = factor(stimulus),
          group = stimulus
        ),
        linewidth = 0.8
      )
  }
  
  # Optional faceting
  if (use_facet) {
    p <- p +
      facet_wrap(
        vars(!!!rlang::syms(facet_names))
      )
  }
  
  # Optional x-axis zoom
  if (!is.null(xmax)) {
    p <- p +
      coord_cartesian(
        xlim = c(0, xmax)
      )
  }
  
  return(p)
}