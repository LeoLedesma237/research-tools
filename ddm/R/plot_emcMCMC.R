# Set up libraries of interest
library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)


plot_emcMCMC <- function(emc, include_variance = FALSE,
                         linewidth = 0.3, alpha = 0.65) {
  
  # Check structure
  required <- c("theta_mu", "theta_var", "stage", "subj_ll")
  
  if (!all(vapply(emc, function(ch) {
    all(required %in% names(ch$samples))
  }, logical(1)))) {
    stop("The supplied EMC2 object must be a hierarchical fit with retained sampling stages.")
  }
  
  # Extract trajectories across all chains
  dat <- dplyr::bind_rows(lapply(seq_along(emc), function(ch) {
    
    s <- emc[[ch]]$samples
    n <- length(s$stage)
    
    mu <- as.data.frame(t(s$theta_mu), check.names = FALSE)
    names(mu) <- rownames(s$theta_mu)
    
    out <- data.frame(
      Chain = factor(ch),
      Iteration = seq_len(n) - 1L,
      Stage = as.character(s$stage),
      mu,
      check.names = FALSE
    )
    
    if (include_variance) {
      variances <- t(vapply(seq_len(n), function(i) {
        diag(s$theta_var[, , i])
      }, numeric(nrow(s$theta_mu))))
      
      colnames(variances) <- paste0(
        rownames(s$theta_mu), "_var"
      )
      
      out <- cbind(out, variances)
    }
    
    out$LogLikelihood <- colSums(s$subj_ll)
    out
  }))
  
  # Population parameters in long format
  param_dat <- dat %>%
    tidyr::pivot_longer(
      cols = -c(Chain, Iteration, Stage, LogLikelihood),
      names_to = "Parameter",
      values_to = "Estimate"
    ) %>%
    dplyr::filter(Stage %in% c("preburn", "burn", "adapt", "sample")) %>%
    dplyr::mutate(
      Phase = ifelse(Stage == "sample", "Sampling", "Early stages")
    ) %>%
    dplyr::group_by(Chain, Phase) %>%
    dplyr::arrange(Iteration, .by_group = TRUE) %>%
    dplyr::mutate(PhaseIteration = dense_rank(Iteration)) %>%
    dplyr::ungroup()
  
  # Calculate stage boundaries from retained stages
  first_chain <- dat %>%
    dplyr::filter(Chain == levels(Chain)[1])
  
  early_stages <- first_chain %>%
    dplyr::filter(Stage %in% c("preburn", "burn", "adapt"))
  
  early_counts <- table(factor(
    early_stages$Stage,
    levels = c("preburn", "burn", "adapt")
  ))
  
  early_boundaries <- head(cumsum(as.numeric(early_counts)), -1) + 0.5
  
  full_counts <- table(factor(
    first_chain$Stage,
    levels = c("init", "preburn", "burn", "adapt", "sample")
  ))
  
  full_boundaries <- head(cumsum(as.numeric(full_counts)), -1) - 1.5
  
  # Common styling
  base_theme <- ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      legend.position = "none",
      strip.text = element_text(face = "bold"),
      panel.grid.minor = element_blank()
    )
  
  # Column 1: Early stages
  p_early <- param_dat %>%
    dplyr::filter(Phase == "Early stages") %>%
    ggplot2::ggplot(aes(PhaseIteration, Estimate, color = Chain)) +
    ggplot2::geom_line(alpha = alpha, linewidth = linewidth) +
    ggplot2::geom_vline(
      xintercept = early_boundaries,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::facet_grid(Parameter ~ ., scales = "free_y") +
    ggplot2::labs(
      title = "Preburn / Burn / Adapt",
      x = "Iteration",
      y = "Population Parameter Estimate"
    ) +
    base_theme
  
  # Column 2: Posterior sampling
  p_sample <- param_dat %>%
    dplyr::filter(Phase == "Sampling") %>%
    ggplot2::ggplot(aes(PhaseIteration, Estimate, color = Chain)) +
    ggplot2::geom_line(alpha = alpha, linewidth = linewidth) +
    ggplot2::facet_grid(Parameter ~ ., scales = "free_y") +
    ggplot2::labs(
      title = "Posterior Sampling",
      x = "Iteration",
      y = NULL
    ) +
    base_theme
  
  # Column 3: Full log-likelihood trajectory
  p_ll <- dat %>%
    ggplot2::ggplot(aes(Iteration, LogLikelihood, color = Chain)) +
    ggplot2::geom_line(alpha = alpha, linewidth = linewidth) +
    ggplot2::geom_vline(
      xintercept = full_boundaries,
      linetype = "dashed",
      color = "gray50"
    ) +
    ggplot2::labs(
      title = "Log-Likelihood",
      x = "Iteration",
      y = "Total Log-Likelihood"
    ) +
    base_theme
  
  # Assemble
  combined <- p_early + p_sample + p_ll +
    patchwork::plot_layout(widths = c(1, 1, 1.2)) +
    patchwork::plot_annotation(
      title = "Hierarchical DDM: MCMC Estimation Across Fitting Stages",
      subtitle = "Population parameter trajectories and total log-likelihood"
    )
  
  return(combined)
}
