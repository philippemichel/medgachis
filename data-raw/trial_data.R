# Generate sample clinical trial data
set.seed(456)

# Number of patients per treatment group
n_per_group <- 75

# Treatment A (Standard care)
treatment_a_trial <- data.frame(
  patient_id = 1:n_per_group,
  treatment = "Standard",
  age = round(rnorm(n_per_group, mean = 68, sd = 10)),
  gender = sample(c("Male", "Female"), n_per_group, replace = TRUE, prob = c(0.6, 0.4)),
  bmi = rnorm(n_per_group, mean = 27, sd = 5),
  asa_score = sample(1:4, n_per_group, replace = TRUE, prob = c(0.2, 0.4, 0.3, 0.1))
)

# Treatment B (Experimental protocol)
treatment_b_trial <- data.frame(
  patient_id = (n_per_group + 1):(2 * n_per_group),
  treatment = "Experimental",
  age = round(rnorm(n_per_group, mean = 66, sd = 12)),
  gender = sample(c("Male", "Female"), n_per_group, replace = TRUE, prob = c(0.55, 0.45)),
  bmi = rnorm(n_per_group, mean = 26.5, sd = 4.5),
  asa_score = sample(1:4, n_per_group, replace = TRUE, prob = c(0.25, 0.45, 0.25, 0.05))
)

# Combine baseline data
trial_data <- rbind(treatment_a_trial, treatment_b_trial)
trial_data$treatment <- factor(trial_data$treatment)

# Generate time-to-event data (e.g., time to complication)
# Hazard ratio for experimental vs standard = 0.7
lambda_standard <- 0.05  # baseline hazard
lambda_experimental <- lambda_standard * 0.7

# Generate survival times using exponential distribution
trial_data$time_to_event <- ifelse(
  trial_data$treatment == "Standard",
  rexp(sum(trial_data$treatment == "Standard"), rate = lambda_standard),
  rexp(sum(trial_data$treatment == "Experimental"), rate = lambda_experimental)
)

# Add censoring (administrative censoring at 365 days)
max_follow_up <- 365
trial_data$event <- ifelse(trial_data$time_to_event <= max_follow_up, 1, 0)
trial_data$time_to_event <- pmin(trial_data$time_to_event, max_follow_up)

# Add continuous outcome (e.g., pain score)
trial_data$pain_score <- ifelse(
  trial_data$treatment == "Standard",
  rnorm(sum(trial_data$treatment == "Standard"), mean = 4.2, sd = 1.5),
  rnorm(sum(trial_data$treatment == "Experimental"), mean = 3.6, sd = 1.3)
)

# Ensure pain scores are within 0-10 range
trial_data$pain_score <- pmax(pmin(trial_data$pain_score, 10), 0)

# Add length of stay
trial_data$length_of_stay <- ifelse(
  trial_data$treatment == "Standard",
  rpois(sum(trial_data$treatment == "Standard"), lambda = 4),
  rpois(sum(trial_data$treatment == "Experimental"), lambda = 3.2)
)

# Save the dataset
save(trial_data, file = "data/trial_data.rda")