\name{calculate_icer}
\alias{calculate_icer}
\title{Calculate Cost-Effectiveness Ratio}
\usage{
calculate_icer(cost_intervention, cost_control, effect_intervention, effect_control)
}
\arguments{
\item{cost_intervention}{Numeric. Cost of the intervention.}

\item{cost_control}{Numeric. Cost of the control/standard treatment.}

\item{effect_intervention}{Numeric. Effectiveness measure for intervention.}

\item{effect_control}{Numeric. Effectiveness measure for control.}
}
\value{
Numeric. The incremental cost-effectiveness ratio.
}
\description{
This function calculates the incremental cost-effectiveness ratio (ICER)
for comparing medical interventions in anesthesia.
}
\examples{
# Calculate ICER for a new anesthetic technique
calculate_icer(1500, 1200, 0.95, 0.85)
}