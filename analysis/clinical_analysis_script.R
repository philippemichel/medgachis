# Clinical Trial Analysis Script
# This script demonstrates how to perform clinical trial analysis using the medgachis package

# Load required libraries
library(dplyr)
library(ggplot2)
library(survival)

# Source functions if package is not installed
if (!require(medgachis, quietly = TRUE)) {
  source("R/clinical_analysis.R")
}

# Load the clinical trial data
load("data/trial_data.rda")

# 1. Exploratory Data Analysis ----
cat("=== Clinical Trial Analysis for Anesthesia Protocols ===\n\n")

# Basic descriptive statistics
cat("Dataset Overview:\n")
cat("Total patients:", nrow(trial_data), "\n")
cat("Treatment groups:", levels(trial_data$treatment), "\n\n")

# Baseline characteristics
baseline_summary <- trial_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    age_mean = round(mean(age), 1),
    age_sd = round(sd(age), 1),
    male_pct = round(100 * sum(gender == "Male") / n(), 1),
    bmi_mean = round(mean(bmi), 1),
    asa_high_pct = round(100 * sum(asa_score >= 3) / n(), 1),
    .groups = "drop"
  )

cat("Baseline Characteristics by Treatment:\n")
print(baseline_summary)

# 2. Primary Endpoint Analysis ----
cat("\n=== Primary Endpoint Analysis (Pain Score) ===\n")

# Perform primary endpoint analysis
primary_results <- clinical_summary_stats(
  data = trial_data,
  outcome = "pain_score",
  treatment = "treatment",
  baseline_vars = c("age", "bmi", "asa_score")
)

cat("Pain Score Summary by Treatment:\n")
print(primary_results$treatment_summary)

cat("\nStatistical Test Results:\n")
cat("Method:", primary_results$statistical_test$method, "\n")
cat("P-value:", format.pval(primary_results$statistical_test$p_value, digits = 3), "\n")
cat("Treatment difference (95% CI):", 
    round(diff(primary_results$statistical_test$estimate), 2), 
    " (", round(primary_results$statistical_test$confidence_interval[1], 2),
    ", ", round(primary_results$statistical_test$confidence_interval[2], 2), ")\n")

# 3. Survival Analysis ----
cat("\n=== Survival Analysis (Time to Complication) ===\n")

# Perform survival analysis
survival_results <- clinical_survival_analysis(
  time = trial_data$time_to_event,
  event = trial_data$event,
  treatment = trial_data$treatment
)

cat("Median Survival Times:\n")
print(survival_results$median_survival)

cat("\nHazard Ratio (Experimental vs Standard):\n")
hr <- survival_results$hazard_ratio
cat("HR:", round(hr[1], 3), " (95% CI:", round(hr[2], 3), "-", round(hr[3], 3), ")\n")

cat("\nLog-rank Test:\n")
cat("Chi-square:", round(survival_results$logrank_test$chisq, 3), "\n")
cat("P-value:", format.pval(survival_results$logrank_test$pvalue, digits = 3), "\n")

# 4. Visualization ----
cat("\n=== Creating Visualizations ===\n")

# Pain score distribution
pain_plot <- ggplot(trial_data, aes(x = treatment, y = pain_score, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.5, size = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white") +
  labs(
    title = "Pain Score Distribution by Treatment Group",
    x = "Treatment",
    y = "Pain Score (0-10)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(pain_plot)
ggsave("analysis/pain_score_distribution.png", pain_plot, width = 10, height = 6, dpi = 300)

# Kaplan-Meier curves
km_fit <- survival_results$km_fit
km_data <- data.frame(
  time = c(0, km_fit$time),
  surv = c(1, km_fit$surv),
  treatment = rep(names(km_fit$strata), c(1, km_fit$strata))
)

# Clean treatment names for plotting
km_data$treatment <- gsub("treatment=", "", km_data$treatment)

km_plot <- ggplot(km_data, aes(x = time, y = surv, color = treatment)) +
  geom_step(size = 1) +
  labs(
    title = "Kaplan-Meier Survival Curves",
    subtitle = "Time to First Complication",
    x = "Time (days)",
    y = "Event-free Survival Probability",
    color = "Treatment"
  ) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent_format()) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(km_plot)
ggsave("analysis/kaplan_meier_curves.png", km_plot, width = 10, height = 6, dpi = 300)

# 5. Secondary Endpoint Analysis ----
cat("\n=== Secondary Endpoint Analysis ===\n")

# Length of stay analysis
cat("Length of Stay Analysis:\n")
los_summary <- trial_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    mean_los = round(mean(length_of_stay), 1),
    sd_los = round(sd(length_of_stay), 1),
    median_los = median(length_of_stay),
    q25_los = quantile(length_of_stay, 0.25),
    q75_los = quantile(length_of_stay, 0.75),
    .groups = "drop"
  )

print(los_summary)

# Statistical test for length of stay
los_test <- wilcox.test(length_of_stay ~ treatment, data = trial_data)
cat("Wilcoxon rank-sum test p-value:", format.pval(los_test$p.value, digits = 3), "\n")

# Length of stay plot
los_plot <- ggplot(trial_data, aes(x = treatment, y = length_of_stay, fill = treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  labs(
    title = "Length of Stay by Treatment Group",
    x = "Treatment",
    y = "Length of Stay (days)",
    fill = "Treatment"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(los_plot)
ggsave("analysis/length_of_stay.png", los_plot, width = 10, height = 6, dpi = 300)

# 6. Subgroup Analysis ----
cat("\n=== Subgroup Analysis ===\n")

# Define subgroups
subgroups <- list(
  "Age ≤65" = trial_data$age <= 65,
  "Age >65" = trial_data$age > 65,
  "Male" = trial_data$gender == "Male",
  "Female" = trial_data$gender == "Female",
  "ASA 1-2" = trial_data$asa_score <= 2,
  "ASA 3-4" = trial_data$asa_score > 2
)

# Calculate treatment effects for each subgroup
subgroup_results <- data.frame(
  Subgroup = names(subgroups),
  N = sapply(subgroups, sum),
  Effect = NA,
  Lower_CI = NA,
  Upper_CI = NA,
  P_value = NA
)

for (i in seq_along(subgroups)) {
  subset_data <- trial_data[subgroups[[i]], ]
  if (nrow(subset_data) > 10 && length(unique(subset_data$treatment)) == 2) {
    test_result <- t.test(pain_score ~ treatment, data = subset_data)
    subgroup_results$Effect[i] <- diff(test_result$estimate)
    subgroup_results$Lower_CI[i] <- test_result$conf.int[1]
    subgroup_results$Upper_CI[i] <- test_result$conf.int[2]
    subgroup_results$P_value[i] <- test_result$p.value
  }
}

# Remove rows with missing results
subgroup_results <- subgroup_results[!is.na(subgroup_results$Effect), ]

cat("Subgroup Analysis Results (Pain Score Difference):\n")
print(subgroup_results)

# Forest plot
if (nrow(subgroup_results) > 0) {
  forest_plot <- ggplot(subgroup_results, aes(x = Effect, y = reorder(Subgroup, Effect))) +
    geom_point(size = 3) +
    geom_errorbarh(aes(xmin = Lower_CI, xmax = Upper_CI), height = 0.2) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
    labs(
      title = "Treatment Effect by Subgroups",
      subtitle = "Pain Score Difference (Experimental - Standard)",
      x = "Treatment Effect (95% CI)",
      y = "Subgroup"
    ) +
    theme_minimal()
  
  print(forest_plot)
  ggsave("analysis/subgroup_forest_plot.png", forest_plot, width = 10, height = 6, dpi = 300)
}

# 7. Safety Analysis ----
cat("\n=== Safety Analysis ===\n")

safety_summary <- trial_data %>%
  group_by(treatment) %>%
  summarise(
    n = n(),
    events = sum(event),
    event_rate = round(100 * sum(event) / n(), 1),
    total_follow_up = sum(time_to_event),
    incidence_rate = round(1000 * sum(event) / sum(time_to_event), 2),
    .groups = "drop"
  )

cat("Safety Summary:\n")
print(safety_summary)

# Fisher's exact test for event rates
safety_table <- table(trial_data$treatment, trial_data$event)
fisher_test <- fisher.test(safety_table)
cat("Fisher's exact test p-value:", format.pval(fisher_test$p.value, digits = 3), "\n")

# 8. Export Results ----
cat("\n=== Exporting Results ===\n")

# Save detailed results
saveRDS(list(
  primary_results = primary_results,
  survival_results = survival_results,
  subgroup_results = subgroup_results,
  safety_summary = safety_summary
), "analysis/clinical_results.rds")

# Create summary report
clinical_summary <- list(
  date = Sys.Date(),
  analysis_type = "Clinical Trial Analysis",
  dataset = "trial_data",
  sample_size = nrow(trial_data),
  treatments = levels(trial_data$treatment),
  primary_endpoint = list(
    endpoint = "pain_score",
    p_value = primary_results$statistical_test$p_value,
    treatment_difference = diff(primary_results$statistical_test$estimate),
    confidence_interval = primary_results$statistical_test$confidence_interval
  ),
  survival_analysis = list(
    hazard_ratio = survival_results$hazard_ratio[1],
    hr_ci_lower = survival_results$hazard_ratio[2],
    hr_ci_upper = survival_results$hazard_ratio[3],
    logrank_p = survival_results$logrank_test$pvalue
  )
)

# Save as JSON
jsonlite::write_json(clinical_summary, "analysis/clinical_summary.json", pretty = TRUE, auto_unbox = TRUE)

cat("Analysis complete! Results saved in analysis/ directory.\n")
cat("Key files created:\n")
cat("- clinical_results.rds: Detailed analysis results\n")
cat("- clinical_summary.json: Summary report\n")
cat("- *.png: Visualization plots\n")