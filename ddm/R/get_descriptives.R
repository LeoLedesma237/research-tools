# Create a custom function that returns the descriptives of the data
get_descriptives <- function(data, design) {
  
  # Add a correct variable to the data based on the rule given
  data <- data %>%
    mutate(
      correct = rlang::eval_tidy(design$correct, data = data)
    )
  
  # Get accuracy descriptives for each stimulus
  accuracy <- data %>%
    group_by(stimulus = !!design$stimulus) %>%
    summarise(
      n_subjects = n_distinct(!!design$id),
      n_trials = n(),
      n_correct = sum(correct, na.rm = TRUE),
      prop_correct = mean(correct, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Get RT quantiles for each stimulus and response accuracy
  reaction_time <- data %>%
    group_by(
      stimulus = !!design$stimulus,
      correct
    ) %>%
    summarise(
      q10 = quantile(!!design$rt, .10, na.rm = TRUE),
      q30 = quantile(!!design$rt, .30, na.rm = TRUE),
      q50 = quantile(!!design$rt, .50, na.rm = TRUE),
      q70 = quantile(!!design$rt, .70, na.rm = TRUE),
      q90 = quantile(!!design$rt, .90, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Return descriptives
  list(
    accuracy = accuracy,
    reaction_time = reaction_time
  )
}