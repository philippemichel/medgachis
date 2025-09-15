# medgachis: Economic and Clinical Trial Analysis in Anesthesia

## Installation

You can install the development version of medgachis from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("philippemichel/medgachis")
```

## Basic Usage

### Economic Analysis

``` r
library(medgachis)

# Load example data
data("anesthesia_costs")

# Perform cost-effectiveness analysis
cea_results <- economic_analysis(
  costs = anesthesia_costs$cost,
  effects = anesthesia_costs$qaly,
  treatment = anesthesia_costs$treatment
)

# View results
cea_results$icer
cea_results$net_monetary_benefit

# Create cost-effectiveness plane
plot_ce_plane(cea_results)
```

### Clinical Trial Analysis

``` r
# Load clinical trial data
data("trial_data")

# Survival analysis
survival_results <- clinical_survival_analysis(
  time = trial_data$time_to_event,
  event = trial_data$event,
  treatment = trial_data$treatment
)

# View hazard ratio
survival_results$hazard_ratio

# Summary statistics for outcomes
clinical_summary_stats(
  data = trial_data,
  outcome = "pain_score",
  treatment = "treatment"
)
```

### Generate Reports

The package includes Quarto templates for generating comprehensive reports:

- `qmd/economic_analysis.qmd`: Economic evaluation template
- `qmd/clinical_trial.qmd`: Clinical trial analysis template  
- `qmd/integrated_report.qmd`: Combined analysis report

### Analysis Scripts

Pre-built analysis scripts are available in the `analysis/` folder:

- `economic_analysis_script.R`: Complete economic analysis workflow
- `clinical_analysis_script.R`: Complete clinical trial analysis workflow