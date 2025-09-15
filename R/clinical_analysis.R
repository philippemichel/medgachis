#' Clinical Trial Survival Analysis
#'
#' Performs survival analysis for clinical trial data
#'
#' @param time Numeric vector of time to event
#' @param event Numeric vector indicating event occurrence (1 = event, 0 = censored)
#' @param treatment Factor indicating treatment groups
#' @param covariates Optional data frame of additional covariates
#'
#' @return List containing survival analysis results
#' @export
#'
#' @examples
#' time <- c(12, 24, 36, 48, 60)
#' event <- c(1, 0, 1, 1, 0)
#' treatment <- factor(c("A", "A", "B", "B", "A"))
#' clinical_survival_analysis(time, event, treatment)
clinical_survival_analysis <- function(time, event, treatment, covariates = NULL) {
  
  # Input validation
  if (length(time) != length(event) || length(time) != length(treatment)) {
    stop("time, event, and treatment must have the same length")
  }
  
  # Create survival object
  surv_obj <- survival::Surv(time, event)
  
  # Kaplan-Meier estimator
  km_fit <- survival::survfit(surv_obj ~ treatment)
  
  # Log-rank test
  logrank_test <- survival::survdiff(surv_obj ~ treatment)
  
  # Cox proportional hazards model
  if (is.null(covariates)) {
    cox_formula <- surv_obj ~ treatment
  } else {
    covariate_names <- names(covariates)
    cox_formula <- as.formula(paste("surv_obj ~", paste(c("treatment", covariate_names), collapse = " + ")))
    covariates$treatment <- treatment
  }
  
  cox_fit <- survival::coxph(cox_formula, data = if (is.null(covariates)) data.frame(treatment = treatment) else covariates)
  
  # Median survival times
  median_survival <- summary(km_fit)$table[, "median"]
  
  # Hazard ratio and confidence interval
  hr_summary <- summary(cox_fit)
  hazard_ratio <- hr_summary$coefficients[1, c("exp(coef)", "lower .95", "upper .95")]
  
  # Return results
  list(
    km_fit = km_fit,
    logrank_test = logrank_test,
    cox_fit = cox_fit,
    median_survival = median_survival,
    hazard_ratio = hazard_ratio,
    data = data.frame(time = time, event = event, treatment = treatment)
  )
}

#' Plot Kaplan-Meier Curves
#'
#' Creates Kaplan-Meier survival curves
#'
#' @param clinical_results Results from clinical_survival_analysis function
#'
#' @return ggplot object
#' @export
plot_km_curves <- function(clinical_results) {
  
  # Extract survival data for plotting
  km_data <- survival::survfit(clinical_results$km_fit)
  
  # Create data frame for ggplot
  plot_data <- data.frame(
    time = km_data$time,
    surv = km_data$surv,
    treatment = rep(names(km_data$strata), km_data$strata)
  )
  
  # Handle case when treatment names are not available
  if (is.null(plot_data$treatment)) {
    plot_data$treatment <- "All"
  }
  
  ggplot2::ggplot(plot_data, ggplot2::aes(x = time, y = surv, color = treatment)) +
    ggplot2::geom_step(size = 1) +
    ggplot2::labs(
      title = "Kaplan-Meier Survival Curves",
      x = "Time",
      y = "Survival Probability",
      color = "Treatment"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
}

#' Clinical Trial Summary Statistics
#'
#' Calculates summary statistics for clinical trial endpoints
#'
#' @param data Data frame containing trial data
#' @param outcome Primary outcome variable name
#' @param treatment Treatment variable name
#' @param baseline_vars Vector of baseline variable names
#'
#' @return List containing summary statistics
#' @export
clinical_summary_stats <- function(data, outcome, treatment, baseline_vars = NULL) {
  
  # Overall summary by treatment
  treatment_summary <- data %>%
    dplyr::group_by(!!rlang::sym(treatment)) %>%
    dplyr::summarise(
      n = dplyr::n(),
      mean_outcome = mean(!!rlang::sym(outcome), na.rm = TRUE),
      sd_outcome = sd(!!rlang::sym(outcome), na.rm = TRUE),
      median_outcome = median(!!rlang::sym(outcome), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Statistical test
  if (length(unique(data[[treatment]])) == 2) {
    # Two-sample t-test
    t_test_result <- t.test(data[[outcome]] ~ data[[treatment]])
    statistical_test <- list(
      method = "Two-sample t-test",
      p_value = t_test_result$p.value,
      confidence_interval = t_test_result$conf.int,
      estimate = t_test_result$estimate
    )
  } else {
    # ANOVA for multiple groups
    anova_result <- aov(data[[outcome]] ~ data[[treatment]])
    statistical_test <- list(
      method = "ANOVA",
      p_value = summary(anova_result)[[1]][["Pr(>F)"]][1],
      f_statistic = summary(anova_result)[[1]][["F value"]][1]
    )
  }
  
  # Baseline characteristics table
  baseline_table <- NULL
  if (!is.null(baseline_vars)) {
    baseline_table <- data %>%
      dplyr::group_by(!!rlang::sym(treatment)) %>%
      dplyr::summarise(
        dplyr::across(dplyr::all_of(baseline_vars), list(
          mean = ~ mean(.x, na.rm = TRUE),
          sd = ~ sd(.x, na.rm = TRUE)
        )),
        .groups = "drop"
      )
  }
  
  list(
    treatment_summary = treatment_summary,
    statistical_test = statistical_test,
    baseline_table = baseline_table
  )
}