\name{summarize_outcomes}
\alias{summarize_outcomes}
\title{Summarize Patient Outcomes}
\usage{
summarize_outcomes(outcomes, group)
}
\arguments{
\item{outcomes}{Numeric vector. Patient outcome measures.}

\item{group}{Character vector. Group assignment for each patient.}
}
\value{
Data frame with summary statistics by group.
}
\description{
This function provides summary statistics for patient outcomes
in anesthesia studies.
}
\examples{
outcomes <- c(95, 92, 88, 96, 89, 94, 91, 93)
groups <- c(rep("intervention", 4), rep("control", 4))
summarize_outcomes(outcomes, groups)
}