# Create a custom function that returns the descriptives of the data
get_descriptives <- function(data, design, formula = ~ stimulus) {
  
  # Create standardized variables
  data <- data %>%
    mutate(
      id = !!design$id,
      stimulus = !!design$stimulus,
      rt = !!design$rt,
      correct = rlang::eval_tidy(design$correct, data = data)
    )
  
  
  # Get grouping variables from formula
  grouping_vars <- all.vars(formula)
  
  
  # Create formulas for accuracy
  id_formula <- reformulate(grouping_vars, response = "id")
  correct_formula <- reformulate(grouping_vars, response = "correct")
  
  
  # Accuracy descriptives
  
  # Number of subjects
  n_subjects <- aggregate(
    id_formula,
    data = data,
    FUN = function(x) length(unique(x))
  )
  
  # Number of trials
  n_trials <- aggregate(
    id_formula,
    data = data,
    FUN = length
  )
  
  # Number correct
  n_correct <- aggregate(
    correct_formula,
    data = data,
    FUN = sum,
    na.rm = TRUE
  )
  
  # Proportion correct
  prop_correct <- aggregate(
    correct_formula,
    data = data,
    FUN = mean,
    na.rm = TRUE
  )
  
  
  # Combine accuracy descriptives
  accuracy <- n_subjects
  
  names(accuracy)[ncol(accuracy)] <- "n_subjects"
  
  accuracy$n_trials <- n_trials$id
  accuracy$n_correct <- n_correct$correct
  accuracy$prop_correct <- prop_correct$correct
  
  
  # Add correct to grouping variables for RT
  rt_grouping_vars <- c(grouping_vars, "correct")
  
  # Create RT formula
  rt_formula <- reformulate(
    rt_grouping_vars,
    response = "rt"
  )
  
  
  # RT quantiles
  reaction_time <- aggregate(
    rt_formula,
    data = data,
    FUN = quantile,
    probs = c(0, .05, .25, .50, .75, .95, 1),
    na.rm = TRUE
  )
  
  
  # Separate RT quantiles into columns
  rt_quantiles <- reaction_time$rt
  
  reaction_time$rt <- NULL
  
  reaction_time$min <- rt_quantiles[, 1]
  reaction_time$q05 <- rt_quantiles[, 2]
  reaction_time$q25 <- rt_quantiles[, 3]
  reaction_time$q50 <- rt_quantiles[, 4]
  reaction_time$m75 <- rt_quantiles[, 5]
  reaction_time$q95 <- rt_quantiles[, 6]
  reaction_time$max <- rt_quantiles[, 7]
  
  
  # Round the data frame information
  accuracy <- mutate(accuracy, across(where(is.numeric), ~ round(.x, 3)))
  reaction_time <- mutate(reaction_time, across(where(is.numeric), ~ round(.x, 3)))
  
  
  # Return descriptives
  list(
    accuracy = accuracy,
    reaction_time = reaction_time
  )
}