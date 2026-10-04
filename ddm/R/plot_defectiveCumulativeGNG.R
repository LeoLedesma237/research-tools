plot_defectiveCumulativeGNG <- function(data, design, cfg) {
  
  # Standardize variables
  data <- data %>%
    mutate(
      ID = !!design$id,
      S = !!design$stimulus,
      rt = !!design$rt,
      trial = row_number()
    )
  
  # Define grouping variables
  facet_vars <- all.vars(cfg$facet)
  group_vars <- c("S", facet_vars)
  id_group_vars <- c("ID", group_vars)
  
  # Total number of trials
  n_trials <- aggregate(
    reformulate(group_vars, response = "trial"),
    data,
    length
  )
  names(n_trials)[ncol(n_trials)] <- "n_trials"
  
  # Create overall defective CDF
  cdf_dat <- data %>%
    drop_na(rt) %>%
    arrange(across(all_of(group_vars)), rt) %>%
    group_by(across(all_of(group_vars))) %>%
    mutate(cumulative_n = row_number()) %>%
    ungroup() %>%
    left_join(n_trials, by = group_vars) %>%
    mutate(cdf = cumulative_n / n_trials)
  
  # Get Q25, Q50, Q75
  cdf_quantiles <- cdf_dat %>%
    group_by(across(all_of(group_vars))) %>%
    slice(round(c(.25, .50, .75) * n())) %>%
    ungroup()
  
  # Total number of trials per subject
  n_trials_id <- aggregate(
    reformulate(id_group_vars, response = "trial"),
    data,
    length
  )
  names(n_trials_id)[ncol(n_trials_id)] <- "n_trials"
  
  # Create individual defective CDFs
  cdf_id <- data %>%
    drop_na(rt) %>%
    arrange(across(all_of(id_group_vars)), rt) %>%
    group_by(across(all_of(id_group_vars))) %>%
    mutate(cumulative_n = row_number()) %>%
    ungroup() %>%
    left_join(n_trials_id, by = id_group_vars) %>%
    mutate(cdf = cumulative_n / n_trials)
  
  # Internal plotting function
  make_cdf_plot <- function(stim, xmax, ymax) {
    
    p <- ggplot()
    
    # Individual CDFs
    if (cfg$individual) {
      p <- p +
        geom_step(
          data = filter(cdf_id, S == stim),
          aes(x = rt, y = cdf, group = ID),
          color = "grey",
          alpha = .8,
          linewidth = .8
        )
    }
    
    # Reference lines
    p <- p +
      geom_vline(
        xintercept = .5,
        color = "red",
        linetype = "dashed"
      ) +
      geom_hline(
        yintercept = .5,
        color = "red",
        linetype = "dashed"
      )
    
    # Overall CDF
    p <- p +
      geom_step(
        data = filter(cdf_dat, S == stim),
        aes(x = rt, y = cdf),
        color = "black",
        linewidth = 1
      ) +
      geom_point(
        data = filter(cdf_quantiles, S == stim),
        aes(x = rt, y = cdf),
        size = 3
      ) +
      facet_wrap(cfg$facet) +
      coord_cartesian(
        xlim = c(0, xmax),
        ylim = c(0, ymax)
      ) +
      labs(
        title = ifelse(stim == "go", "Go Trials", "No-Go Trials"),
        x = "Reaction Time (s)",
        y = "Defective CDF"
      ) +
      theme_minimal()
    
    return(p)
  }
  
  # Return Go and No-Go plots
  plots <- list(
    go = make_cdf_plot("go", cfg$go$xmax, cfg$go$ymax),
    ng = make_cdf_plot("ng", cfg$ng$xmax, cfg$ng$ymax)
  )
  
  return(plots)
}