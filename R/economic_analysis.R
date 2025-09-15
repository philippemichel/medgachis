#' Economic Analysis for Medical Interventions
#'
#' Performs cost-effectiveness analysis for medical interventions
#'
#' @param costs Numeric vector of costs
#' @param effects Numeric vector of effects (e.g., QALYs)
#' @param treatment Factor indicating treatment groups
#' @param willingness_to_pay Willingness to pay threshold (default: 50000)
#'
#' @return List containing analysis results
#' @export
#'
#' @examples
#' costs <- c(1000, 1500, 2000, 2500)
#' effects <- c(0.8, 0.85, 0.9, 0.95)
#' treatment <- factor(c("A", "A", "B", "B"))
#' economic_analysis(costs, effects, treatment)
economic_analysis <- function(costs, effects, treatment, willingness_to_pay = 50000) {
  
  # Input validation
  if (length(costs) != length(effects) || length(costs) != length(treatment)) {
    stop("All inputs must have the same length")
  }
  
  # Create data frame
  data <- data.frame(
    cost = costs,
    effect = effects,
    treatment = treatment
  )
  
  # Calculate means by treatment
  summary_stats <- data %>%
    dplyr::group_by(treatment) %>%
    dplyr::summarise(
      mean_cost = mean(cost, na.rm = TRUE),
      mean_effect = mean(effect, na.rm = TRUE),
      n = dplyr::n(),
      .groups = "drop"
    )
  
  # Calculate incremental cost-effectiveness ratio (ICER)
  if (nrow(summary_stats) == 2) {
    delta_cost <- diff(summary_stats$mean_cost)
    delta_effect <- diff(summary_stats$mean_effect)
    
    icer <- if (delta_effect != 0) delta_cost / delta_effect else Inf
    
    # Net monetary benefit
    nmb <- delta_effect * willingness_to_pay - delta_cost
    
    # Cost-effectiveness acceptability
    cost_effective <- nmb > 0
  } else {
    icer <- NA
    nmb <- NA
    cost_effective <- NA
  }
  
  # Bootstrap confidence intervals
  set.seed(123)
  bootstrap_results <- boot::boot(
    data = data,
    statistic = function(data, indices) {
      boot_data <- data[indices, ]
      boot_summary <- boot_data %>%
        dplyr::group_by(treatment) %>%
        dplyr::summarise(
          mean_cost = mean(cost, na.rm = TRUE),
          mean_effect = mean(effect, na.rm = TRUE),
          .groups = "drop"
        )
      
      if (nrow(boot_summary) == 2) {
        delta_c <- diff(boot_summary$mean_cost)
        delta_e <- diff(boot_summary$mean_effect)
        return(c(delta_c, delta_e, if (delta_e != 0) delta_c / delta_e else Inf))
      } else {
        return(c(NA, NA, NA))
      }
    },
    R = 1000
  )
  
  # Return results
  list(
    summary_stats = summary_stats,
    icer = icer,
    net_monetary_benefit = nmb,
    cost_effective = cost_effective,
    willingness_to_pay = willingness_to_pay,
    bootstrap_results = bootstrap_results,
    data = data
  )
}

#' Plot Cost-Effectiveness Plane
#'
#' Creates a cost-effectiveness plane plot
#'
#' @param economic_results Results from economic_analysis function
#'
#' @return ggplot object
#' @export
plot_ce_plane <- function(economic_results) {
  
  if (is.null(economic_results$bootstrap_results)) {
    stop("Bootstrap results not available")
  }
  
  boot_data <- data.frame(
    delta_cost = economic_results$bootstrap_results$t[, 1],
    delta_effect = economic_results$bootstrap_results$t[, 2]
  )
  
  # Remove infinite or missing values
  boot_data <- boot_data[is.finite(boot_data$delta_cost) & is.finite(boot_data$delta_effect), ]
  
  wtp <- economic_results$willingness_to_pay
  
  ggplot2::ggplot(boot_data, ggplot2::aes(x = delta_effect, y = delta_cost)) +
    ggplot2::geom_point(alpha = 0.3, color = "blue") +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed") +
    ggplot2::geom_abline(slope = wtp, intercept = 0, color = "red", linetype = "solid") +
    ggplot2::labs(
      title = "Cost-Effectiveness Plane",
      subtitle = paste("Willingness to pay threshold:", scales::dollar(wtp)),
      x = "Incremental Effect",
      y = "Incremental Cost"
    ) +
    ggplot2::theme_minimal()
}