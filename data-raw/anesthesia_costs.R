# Generate sample anesthesia cost data
set.seed(123)

# Number of patients per treatment group
n_per_group <- 50

# Treatment A (Standard anesthesia)
treatment_a <- data.frame(
  patient_id = 1:n_per_group,
  treatment = "Standard",
  cost = rnorm(n_per_group, mean = 1200, sd = 300),
  qaly = rnorm(n_per_group, mean = 0.85, sd = 0.10),
  length_of_stay = rpois(n_per_group, lambda = 3),
  complications = rbinom(n_per_group, 1, 0.15)
)

# Treatment B (New anesthesia protocol)
treatment_b <- data.frame(
  patient_id = (n_per_group + 1):(2 * n_per_group),
  treatment = "New_Protocol",
  cost = rnorm(n_per_group, mean = 1400, sd = 350),
  qaly = rnorm(n_per_group, mean = 0.90, sd = 0.08),
  length_of_stay = rpois(n_per_group, lambda = 2.5),
  complications = rbinom(n_per_group, 1, 0.08)
)

# Combine datasets
anesthesia_costs <- rbind(treatment_a, treatment_b)
anesthesia_costs$treatment <- factor(anesthesia_costs$treatment)

# Ensure positive costs and QALYs within reasonable bounds
anesthesia_costs$cost <- pmax(anesthesia_costs$cost, 500)
anesthesia_costs$qaly <- pmax(pmin(anesthesia_costs$qaly, 1), 0)

# Add additional variables
anesthesia_costs$age <- round(rnorm(nrow(anesthesia_costs), mean = 65, sd = 12))
anesthesia_costs$bmi <- rnorm(nrow(anesthesia_costs), mean = 26, sd = 4)
anesthesia_costs$asa_score <- sample(1:3, nrow(anesthesia_costs), replace = TRUE, prob = c(0.3, 0.5, 0.2))

# Save the dataset
save(anesthesia_costs, file = "data/anesthesia_costs.rda")