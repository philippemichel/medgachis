# medgachis: Economic and Clinical Trial Analysis in Anesthesia

## Overview

This package provides a comprehensive framework for conducting medical-economic studies in anesthesia, combining clinical trial analysis with economic evaluation tools using R and Quarto.

## Features

- **Clinical Trial Analysis**: Statistical analysis tools for clinical trials in anesthesia
- **Economic Evaluation**: Cost-effectiveness analysis, budget impact modeling
- **Interactive Reports**: Quarto-based dynamic documents and dashboards
- **Data Visualization**: Publication-ready plots and tables
- **Reproducible Research**: Integrated workflow from data to publication

## Installation

```r
# Install from source
devtools::install_github("philippemichel/medgachis")
```

## Quick Start

### Economic Analysis

```r
library(medgachis)

# Load example data
data("anesthesia_costs")

# Perform cost-effectiveness analysis
cea_results <- economic_analysis(
  costs = anesthesia_costs$cost,
  effects = anesthesia_costs$qaly,
  treatment = anesthesia_costs$treatment
)

# Generate report
render_economic_report(cea_results, output_dir = "reports/")
```

### Clinical Trial Analysis

```r
# Load clinical trial data
data("trial_data")

# Survival analysis
survival_results <- clinical_survival_analysis(
  time = trial_data$time_to_event,
  event = trial_data$event,
  treatment = trial_data$treatment
)

# Generate clinical report
render_clinical_report(survival_results, output_dir = "reports/")
```

## Project Structure

```
medgachis/
├── R/                      # R functions
├── data/                   # Example datasets
├── analysis/              # Analysis scripts
├── qmd/                   # Quarto documents
├── docs/                  # Documentation
└── inst/extdata/         # External data files
```

## Quarto Documents

The package includes several pre-configured Quarto documents:

- `economic_analysis.qmd`: Economic evaluation template
- `clinical_trial.qmd`: Clinical trial analysis template
- `dashboard.qmd`: Interactive dashboard
- `final_report.qmd`: Combined analysis report

## Getting Started

1. Clone or download this repository
2. Install required R packages: `install.packages(c("dplyr", "ggplot2", "survival", "meta"))`
3. Install Quarto: https://quarto.org/docs/get-started/
4. Open analysis scripts in the `analysis/` folder
5. Render Quarto documents in the `qmd/` folder

## Contributing

Please read our contributing guidelines and submit pull requests to our GitHub repository.

## License

This project is licensed under the MIT License - see the LICENSE file for details.

## Contact

Philippe Michel - philippe.michel@example.com

Project Link: https://github.com/philippemichel/medgachis
