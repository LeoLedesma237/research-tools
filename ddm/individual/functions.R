# Create a custom function to create a design model and data object
create_design <- function(formula) {
  
  design <- design(
    factors = list(subjects = 1, S = c("go", "nogo")),
    Rlevels = c("go", "nogo"),
    matchfun = function(d) as.numeric(d$S) == as.numeric(d$lR),
    functions = list(match = function(d) ifelse(d$S == "go", -1, 1)),
    model = DDM,
    formula = formula,
    constants = c(s = log(1))
  )
  
  return(design)
}

# Make priors function
make_priors <- function(mod, prior_pvec, prior_vvec) {
  
  # Generate the prior object
  prior(design = mod,
        theta_mu_mean = prior_pvec, 
        theta_mu_var = prior_vvec, 
        type="single")
}


# Create a custom function to simulate data and report their descriptives
simulate_DDM <- function(param_vector, param_name, mod,
                         param_vector2 = NULL, param_name2 = NULL, n_trials = 10000) {
  output <- list()
  
  if(is.null(param_vector2)) param_vector2 <- NA_real_
  
  for(ii in seq_along(param_vector)) {
    for(jj in seq_along(param_vector2)) {
      
      p <- p_vector
      p[param_name] <- param_vector[ii]
      
      if(!is.null(param_name2)) {
        p[param_name2] <- param_vector2[jj]
      }
      
      dat <- make_data(p, mod, n_trials=n_trials)
      
      key <- paste(ii, jj, sep="_")
      output[[key]] <- list()
      output[[key]]$accuracy <- aggregate(S==R~S, dat, mean)
      output[[key]]$reaction_time <- aggregate(
        rt~(S==R)+S, dat, quantile, probs = c(.025, .5, .975)
      )
    }
  }
  
  # Combine accuracy
  accuracy <- bind_rows(lapply(output, `[[`, "accuracy"), .id = "index") %>%
    mutate(
      param = param_vector[as.integer(sub("_.*", "", index))],
      param2 = if(is.null(param_name2)) NA_real_ else
        param_vector2[as.integer(sub(".*_", "", index))]
    ) %>%
    select(-index)
  
  # Combine reaction time
  reaction_time <- bind_rows(lapply(output, `[[`, "reaction_time"), .id = "index") %>%
    mutate(
      param = param_vector[as.integer(sub("_.*", "", index))],
      param2 = if(is.null(param_name2)) NA_real_ else
        param_vector2[as.integer(sub(".*_", "", index))]
    ) %>%
    (\(x) cbind(
      x[, c("param", "S == R", "S", "param2")],
      as.data.frame(x$rt)
    ))() %>%
    setNames(c("param", "S==R", "S", "param2", "rt.025", "rt.5", "rt.975"))
  
  return(list(accuracy = accuracy, reaction_time = reaction_time))
}


# Create a custom function to plot the results
plot_results <- function(sim_results, transform = "NULL", title ="") {
  
  # Save the data into objects
  accuracy <- sim_results$accuracy
  reaction_time <- sim_results$reaction_time
  
  # Check the transformation
  if(transform == "exp") {
    accuracy$param <- exp(accuracy$param)
    reaction_time$param <- exp(reaction_time$param)
    
  } else if (transform == "pnorm") {
    accuracy$param <- pnorm(accuracy$param)
    reaction_time$param <- pnorm(reaction_time$param)
  }
  
  # Generate the plots
  p1 <- accuracy %>%
    ggplot(aes(x = param, y = `S == R`, color = S)) +
    facet_wrap(~S, nrow = 2) +
    geom_point() + 
    labs(y = "Accuracy", x = NULL) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  p2 <- reaction_time %>%
    ggplot(aes(x = param, y = rt.5, color = S, fill = S)) +
    geom_ribbon(aes(ymin = rt.025, ymax = rt.975),
                alpha = .2, color = NA) +
    geom_line(linewidth = 1) +
    facet_wrap(~S, nrow = 2) +
    labs(y = "Reaction Time", x= NULL) +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Prduce plots side by side
  (p1 + p2) +
    plot_annotation(title = title,
                    theme = theme(plot.title = element_text(hjust = 0.5)))
}


# Create a function to describe results
describe_results <- function(sim_results, transform = "NULL",
                             parameter2 = FALSE, transform2 = "NULL") {
  
  x <- sim_results$accuracy
  
  # Identify parameter quantiles
  param_lvls <- quantile(x$param, probs = c(.025, .5, .975))
  closest <- sapply(param_lvls, function(p) {
    x$param[which.min(abs(x$param - p))]
  })
  
  x_filt <- filter(x, param %in% closest)
  
  # Identify second parameter quantiles
  if(parameter2) {
    param2_lvls <- quantile(x$param2, probs = c(.025, .5, .975))
    closest2 <- sapply(param2_lvls, function(p) {
      x$param2[which.min(abs(x$param2 - p))]
    })
    
    x_filt <- filter(x_filt, param2 %in% closest2)
  }
  
  # Transform parameters
  if(transform == "exp") {
    x_filt <- mutate(x_filt, param = exp(param))
  } else if(transform == "pnorm") {
    x_filt <- mutate(x_filt, param = pnorm(param))
  }
  
  if(parameter2) {
    if(transform2 == "exp") {
      x_filt <- mutate(x_filt, param2 = exp(param2))
    } else if(transform2 == "pnorm") {
      x_filt <- mutate(x_filt, param2 = pnorm(param2))
    }
  }
  
  # Describe accuracy
  if(parameter2) {
    accuracy <- aggregate(`S == R` ~ param + param2 + S, data = x_filt, mean)
  } else {
    accuracy <- aggregate(`S == R` ~ param + S, data = x_filt, mean)
  }
  
  accuracy <- accuracy %>%
    pivot_wider(names_from = S, values_from = `S == R`) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))
  
  # Describe results for reaction time
  z <- sim_results$reaction_time
  
  param_lvls <- quantile(z$param, probs = c(.025, .5, .975))
  closest <- sapply(param_lvls, function(p) {
    z$param[which.min(abs(z$param - p))]
  })
  
  z_filt <- filter(z, param %in% closest)
  
  if(parameter2) {
    param2_lvls <- quantile(z$param2, probs = c(.025, .5, .975))
    closest2 <- sapply(param2_lvls, function(p) {
      z$param2[which.min(abs(z$param2 - p))]
    })
    
    z_filt <- filter(z_filt, param2 %in% closest2)
  }
  
  # Transform parameters
  if(transform == "exp") {
    z_filt <- mutate(z_filt, param = exp(param))
  } else if(transform == "pnorm") {
    z_filt <- mutate(z_filt, param = pnorm(param))
  }
  
  if(parameter2) {
    if(transform2 == "exp") {
      z_filt <- mutate(z_filt, param2 = exp(param2))
    } else if(transform2 == "pnorm") {
      z_filt <- mutate(z_filt, param2 = pnorm(param2))
    }
  }
  
  # Describe reaction times
  rt <- z_filt %>%
    select(-`S==R`) %>%
    {if(!parameter2) select(., -param2) else .} %>%
    pivot_wider(
      names_from = S,
      values_from = starts_with("rt."),
      names_glue = "{S}_{.value}",
      names_vary = "slowest"
    ) %>%
    mutate(across(where(is.numeric), ~round(.x, 3)))
  
  # Return both
  list(accuracy = accuracy,
       reaction_time = rt)
}



# Create a custom function to plot two-parameter simulation results
plot_results2 <- function(sim_results, transform = "NULL",
                          transform2 = "NULL", title = "") {
  
  # Save the data into objects
  accuracy <- sim_results$accuracy
  reaction_time <- sim_results$reaction_time
  
  # Identify the three quantiles of parameter 2
  param_lvls <- quantile(accuracy$param2, probs = c(.025, .5, .975))
  closest <- sapply(param_lvls, function(p) {
    accuracy$param2[which.min(abs(accuracy$param2 - p))]
  })
  
  # Filter data to the three selected values of parameter 2
  accuracy <- filter(accuracy, param2 %in% closest)
  reaction_time <- filter(reaction_time, param2 %in% closest)
  
  # Check the transformation for parameter 1
  if(transform == "exp") {
    accuracy$param <- exp(accuracy$param)
    reaction_time$param <- exp(reaction_time$param)
    
  } else if(transform == "pnorm") {
    accuracy$param <- pnorm(accuracy$param)
    reaction_time$param <- pnorm(reaction_time$param)
  }
  
  # Check the transformation for parameter 2
  if(transform2 == "exp") {
    accuracy$param2 <- exp(accuracy$param2)
    reaction_time$param2 <- exp(reaction_time$param2)
    
  } else if(transform2 == "pnorm") {
    accuracy$param2 <- pnorm(accuracy$param2)
    reaction_time$param2 <- pnorm(reaction_time$param2)
  }
  
  # Label the three levels of parameter 2
  accuracy$param2 <- factor(
    accuracy$param2,
    levels = sort(unique(accuracy$param2)),
    labels = c("2.5%", "50%", "97.5%")
  )
  
  reaction_time$param2 <- factor(
    reaction_time$param2,
    levels = sort(unique(reaction_time$param2)),
    labels = c("2.5%", "50%", "97.5%")
  )
  
  # Generate the accuracy plot
  p1 <- accuracy %>%
    ggplot(aes(x = param, y = `S == R`,
               color = param2, group = param2)) +
    geom_line(linewidth = 1) +
    facet_wrap(~S, nrow = 2) +
    labs(y = "Accuracy", x = NULL, color = "Parameter 2") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Generate the reaction time plot
  p2 <- reaction_time %>%
    ggplot(aes(x = param, y = rt.5,
               color = param2, group = param2)) +
    geom_line(linewidth = 1) +
    facet_wrap(~S, nrow = 2) +
    labs(y = "Reaction Time", x = NULL, color = "Parameter 2") +
    theme_classic() +
    theme(legend.position = "bottom")
  
  # Produce plots side by side
  (p1 + p2) +
    plot_annotation(
      title = title,
      theme = theme(plot.title = element_text(hjust = 0.5))
    )
}
