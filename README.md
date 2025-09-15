# medgachis

Étude médico-économique en anesthésie / Medical Economic Study in Anesthesia

## Description

`medgachis` is an R package designed for conducting medical economic studies in anesthesia. It provides tools for:

- Cost-effectiveness analysis (ICER calculations)
- Total anesthesia cost estimation
- Patient outcome analysis and summarization
- Data validation for economic studies

## Installation

```r
# Install from source
devtools::install_github("philippemichel/medgachis")
```

## Usage

```r
library(medgachis)

# Calculate incremental cost-effectiveness ratio
icer <- calculate_icer(1500, 1200, 0.95, 0.85)

# Calculate total anesthesia costs
total_cost <- calculate_anesthesia_cost(150, 2, 45, 25)

# Summarize patient outcomes by group
outcomes <- c(95, 92, 88, 96, 89, 94, 91, 93)
groups <- c(rep("intervention", 4), rep("control", 4))
summary_stats <- summarize_outcomes(outcomes, groups)
```

## License

MIT License - see LICENSE file for details.
