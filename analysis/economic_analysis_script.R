# Economic Analysis Script
# This script demonstrates how to perform economic analysis using the medgachis package

# Load required libraries
library(dplyr)
library(ggplot2)
library(boot)

# Source functions if package is not installed
if (!require(medgachis, quietly = TRUE)) {
  source("R/economic_analysis.R")
}

# Load the anesthesia cost data
load("data/anesthesia_costs.rda")

# 1. Exploratory Data Analysis ----
cat("=== Economic Analysis for Anesthesia Protocols ===\n\n")

# Basic descriptive statistics
cat("Dataset Overview:\n")
cat("Total patients:", nrow(anesthesia_costs), "\n")
cat("Treatment groups:", levels(anesthesia_costs$treatment), "\n\n")

# Summary by treatment group
summary_stats <- anesthesia_costs %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean_cost = round(mean(cost), 2),
    sd_cost = round(sd(cost), 2),
    mean_qaly = round(mean(qaly), 3),
    sd_qaly = round(sd(qaly), 3),
    mean_los = round(mean(length_of_stay), 1),
    complication_rate = round(100 * mean(complications), 1),
    .groups = "drop"
  )

print(summary_stats)

# 2. Cost-Effectiveness Analysis ----
cat("\n=== Cost-Effectiveness Analysis ===\n")

# Perform the economic analysis
cea_results <- economic_analysis(
  costs = anesthesia_costs$cost,
  effects = anesthesia_costs$qaly,
  treatment = anesthesia_costs$treatment,
  willingness_to_pay = 50000
)

# Display key results
cat("Summary Statistics by Treatment:\n")
print(cea_results$summary_stats)

cat("\nKey Economic Outcomes:\n")
cat("ICER: $", format(round(cea_results$icer, 0), big.mark = ","), " per QALY\n")
cat("Net Monetary Benefit: $", format(round(cea_results$net_monetary_benefit, 0), big.mark = ","), "\n")
cat("Cost-effective at $50,000 WTP: ", cea_results$cost_effective, "\n")

# 3. Visualization ----
cat("\n=== Creating Visualizations ===\n")

# Cost-effectiveness plane
ce_plane <- plot_ce_plane(cea_results)
print(ce_plane)

# Save the plot
ggsave("analysis/ce_plane.png", ce_plane, width = 10, height = 8, dpi = 300)

# Cost and QALY comparison plot
comparison_plot <- anesthesia_costs %>%
  select(treatment, cost, qaly) %>%
  tidyr::pivot_longer(cols = c(cost, qaly), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = treatment, y = value, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    title = "Cost and QALY Comparison by Treatment",
    x = "Treatment",
    y = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(comparison_plot)
ggsave("analysis/cost_qaly_comparison.png", comparison_plot, width = 10, height = 6, dpi = 300)

# 4. Sensitivity Analysis ----
cat("\n=== Sensitivity Analysis ===\n")

# Cost-effectiveness acceptability curve
wtp_thresholds <- seq(0, 100000, by = 5000)
prob_cost_effective <- sapply(wtp_thresholds, function(wtp) {
  # Re-run analysis with different WTP threshold
  temp_results <- economic_analysis(
    costs = anesthesia_costs$cost,
    effects = anesthesia_costs$qaly,
    treatment = anesthesia_costs$treatment,
    willingness_to_pay = wtp
  )
  
  # Extract bootstrap results
  bootstrap_data <- temp_results$bootstrap_results$t
  delta_cost <- bootstrap_data[, 1]
  delta_effect <- bootstrap_data[, 2]
  
  # Remove infinite values
  valid_indices <- is.finite(delta_cost) & is.finite(delta_effect)
  if (sum(valid_indices) == 0) return(0)
  
  delta_cost <- delta_cost[valid_indices]
  delta_effect <- delta_effect[valid_indices]
  
  # Calculate NMB
  nmb <- delta_effect * wtp - delta_cost
  mean(nmb > 0, na.rm = TRUE)
})

# Create CEAC data
ceac_data <- data.frame(
  wtp = wtp_thresholds,
  probability = prob_cost_effective
)

# Plot CEAC
ceac_plot <- ggplot(ceac_data, aes(x = wtp, y = probability)) +
  geom_line(color = "blue", size = 1) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red") +
  geom_vline(xintercept = 50000, linetype = "dotted", color = "green") +
  labs(
    title = "Cost-Effectiveness Acceptability Curve",
    x = "Willingness to Pay per QALY ($)",
    y = "Probability of Cost-Effectiveness"
  ) +
  scale_x_continuous(labels = scales::dollar_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()

print(ceac_plot)
ggsave("analysis/ceac.png", ceac_plot, width = 10, height = 6, dpi = 300)

# 5. Budget Impact Analysis ----
cat("\n=== Budget Impact Analysis ===\n")

# Assumptions
annual_patients <- 1000
years <- 1:5
cost_difference <- cea_results$summary_stats$mean_cost[2] - cea_results$summary_stats$mean_cost[1]

budget_impact <- data.frame(
  Year = years,
  Additional_Cost_Per_Year = cost_difference * annual_patients,
  Cumulative_Cost = cumsum(cost_difference * annual_patients)
)

print(budget_impact)

# Budget impact plot
budget_plot <- ggplot(budget_impact, aes(x = Year, y = Cumulative_Cost)) +
  geom_line(size = 1, color = "red") +
  geom_point(size = 3, color = "red") +
  labs(
    title = "Cumulative Budget Impact Over 5 Years",
    subtitle = paste("Assumes", annual_patients, "patients per year"),
    x = "Year",
    y = "Cumulative Additional Cost ($)"
  ) +
  scale_y_continuous(labels = scales::dollar_format()) +
  theme_minimal()

print(budget_plot)
ggsave("analysis/budget_impact.png", budget_plot, width = 10, height = 6, dpi = 300)

# 6. Export Results ----
cat("\n=== Exporting Results ===\n")

# Save detailed results
saveRDS(cea_results, "analysis/cea_results.rds")

# Create summary report
summary_report <- list(
  date = Sys.Date(),
  analysis_type = "Cost-Effectiveness Analysis",
  dataset = "anesthesia_costs",
  sample_size = nrow(anesthesia_costs),
  treatments = levels(anesthesia_costs$treatment),
  icer = cea_results$icer,
  nmb = cea_results$net_monetary_benefit,
  cost_effective = cea_results$cost_effective,
  wtp_threshold = cea_results$willingness_to_pay
)

# Save as JSON for easy parsing
jsonlite::write_json(summary_report, "analysis/economic_summary.json", pretty = TRUE, auto_unbox = TRUE)

cat("Analysis complete! Results saved in analysis/ directory.\n")
cat("Key files created:\n")
cat("- cea_results.rds: Detailed analysis results\n")
cat("- economic_summary.json: Summary report\n")
cat("- *.png: Visualization plots\n")