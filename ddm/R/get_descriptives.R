# Create a custom function that returns the descriptives of the data

get_descriptives <- function(
    data,
    design,
    formula = ~ stimulus,
    individual = FALSE,
    min_rt = 10
) {
  
  # Standardize variables
  data <- data %>%
    mutate(
      id = !!design$id,
      stimulus = !!design$stimulus,
      rt = !!design$rt,
      correct = rlang::eval_tidy(design$correct, data = data)
    )
  
  # Get grouping variables
  grouping_vars <- all.vars(formula)
  rt_grouping_vars <- unique(c(grouping_vars, "correct"))
  
  # Aggregate accuracy descriptives
  accuracy <- data %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_subjects = n_distinct(id),
      n_trials = n(),
      n_correct = sum(correct, na.rm = TRUE),
      prop_correct = mean(correct, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Aggregate RT quantiles
  reaction_time <- data %>%
    filter(!is.na(rt)) %>%
    group_by(across(all_of(rt_grouping_vars))) %>%
    summarise(
      min = min(rt),
      q05 = quantile(rt, .05),
      q25 = quantile(rt, .25),
      q50 = median(rt),
      m75 = quantile(rt, .75),
      q95 = quantile(rt, .95),
      max = max(rt),
      .groups = "drop"
    )
  
  # Round summary statistics without rounding counts
  round_summary <- function(x) {
    count_cols <- c(
      "n_subjects", "n_trials", "n_correct",
      "n_rt", "n_included", "n_excluded"
    )
    
    numeric_cols <- names(x)[vapply(x, is.numeric, logical(1))]
    numeric_cols <- setdiff(numeric_cols, count_cols)
    
    x %>%
      mutate(
        across(
          all_of(numeric_cols),
          ~ round(.x, 3)
        )
      )
  }
  
  # Preserve original behavior
  if (!individual) {
    return(list(
      accuracy = round_summary(accuracy),
      reaction_time = round_summary(reaction_time)
    ))
  }
  
  # Validate minimum RT requirement
  if (
    length(min_rt) != 1 ||
    is.na(min_rt) ||
    !is.numeric(min_rt) ||
    !is.finite(min_rt) ||
    min_rt < 1 ||
    min_rt != floor(min_rt)
  ) {
    stop("min_rt must be a positive integer.")
  }
  
  # Subject-level accuracy
  subject_accuracy <- data %>%
    group_by(across(all_of(c("id", grouping_vars)))) %>%
    summarise(
      n_trials = n(),
      n_correct = sum(correct, na.rm = TRUE),
      prop_correct = mean(correct, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Between-subject accuracy descriptives
  individual_accuracy <- subject_accuracy %>%
    group_by(across(all_of(grouping_vars))) %>%
    summarise(
      n_subjects = n(),
      mean = mean(prop_correct, na.rm = TRUE),
      sd = sd(prop_correct, na.rm = TRUE),
      min = min(prop_correct, na.rm = TRUE),
      q05 = quantile(prop_correct, .05, na.rm = TRUE),
      q25 = quantile(prop_correct, .25, na.rm = TRUE),
      q50 = median(prop_correct, na.rm = TRUE),
      q75 = quantile(prop_correct, .75, na.rm = TRUE),
      q95 = quantile(prop_correct, .95, na.rm = TRUE),
      max = max(prop_correct, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify conditions with at least one observed RT
  observed_conditions <- data %>%
    filter(!is.na(rt)) %>%
    distinct(across(all_of(rt_grouping_vars)))
  
  # Calculate subject-level RT descriptives
  # Include zero-response subjects in conditions where
  # other subjects have observed RTs
  subject_rt <- data %>%
    semi_join(
      observed_conditions,
      by = rt_grouping_vars
    ) %>%
    group_by(
      across(all_of(c("id", rt_grouping_vars)))
    ) %>%
    summarise(
      n_rt = sum(!is.na(rt)),
      mean_rt = if (sum(!is.na(rt)) > 0) {
        mean(rt, na.rm = TRUE)
      } else {
        NA_real_
      },
      median_rt = if (sum(!is.na(rt)) > 0) {
        median(rt, na.rm = TRUE)
      } else {
        NA_real_
      },
      sd_rt = if (sum(!is.na(rt)) > 1) {
        sd(rt, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    ) %>%
    mutate(
      included = n_rt >= min_rt
    )
  
  # Count participants excluded from individual RT summaries
  rt_exclusions <- subject_rt %>%
    group_by(across(all_of(rt_grouping_vars))) %>%
    summarise(
      n_subjects = n(),
      n_included = sum(included),
      n_excluded = sum(!included),
      .groups = "drop"
    )
  
  # Between-subject RT descriptives
  # Use temporary names to avoid overwriting mean_rt
  # inside summarise()
  individual_reaction_time <- subject_rt %>%
    filter(included) %>%
    group_by(across(all_of(rt_grouping_vars))) %>%
    summarise(
      mean_of_means = mean(mean_rt),
      sd_of_means = sd(mean_rt),
      min_mean = min(mean_rt),
      q05_mean = quantile(mean_rt, .05),
      q25_mean = quantile(mean_rt, .25),
      q50_mean = median(mean_rt),
      q75_mean = quantile(mean_rt, .75),
      q95_mean = quantile(mean_rt, .95),
      max_mean = max(mean_rt),
      mean_of_medians = mean(median_rt),
      .groups = "drop"
    ) %>%
    rename(
      mean_rt = mean_of_means,
      sd_mean_rt = sd_of_means,
      min = min_mean,
      q05 = q05_mean,
      q25 = q25_mean,
      q50 = q50_mean,
      q75 = q75_mean,
      q95 = q95_mean,
      max = max_mean,
      mean_median_rt = mean_of_medians
    )
  
  # Retain conditions even when no subjects meet min_rt
  individual_reaction_time <- rt_exclusions %>%
    select(all_of(c(rt_grouping_vars, "n_included"))) %>%
    left_join(
      individual_reaction_time,
      by = rt_grouping_vars
    ) %>%
    rename(n_subjects = n_included) %>%
    relocate(n_subjects, .after = last_col())
  
  # Move n_subjects next to grouping columns
  individual_reaction_time <- individual_reaction_time %>%
    relocate(
      n_subjects,
      .after = all_of(tail(rt_grouping_vars, 1))
    )
  
  # Generate exclusion notes
  note_parts <- apply(rt_exclusions, 1, function(x) {
    
    condition <- paste(
      paste(
        rt_grouping_vars,
        x[rt_grouping_vars],
        sep = "="
      ),
      collapse = ", "
    )
    
    paste0(
      condition,
      ": ",
      x[["n_excluded"]],
      " of ",
      x[["n_subjects"]],
      " subjects excluded"
    )
  })
  
  rt_note <- paste0(
    "Individual RT summaries require at least ",
    min_rt,
    " observed RTs per subject and condition. ",
    paste(note_parts, collapse = "; "),
    "."
  )
  
  # Return descriptives
  list(
    accuracy = round_summary(accuracy),
    reaction_time = round_summary(reaction_time),
    individual_accuracy = round_summary(individual_accuracy),
    individual_reaction_time = round_summary(individual_reaction_time),
    subject_accuracy = round_summary(subject_accuracy),
    subject_reaction_time = round_summary(subject_rt),
    rt_exclusions = rt_exclusions,
    rt_note = rt_note
  )
}
