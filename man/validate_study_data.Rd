\name{validate_study_data}
\alias{validate_study_data}
\title{Validate Study Data}
\usage{
validate_study_data(data, required_cols)
}
\arguments{
\item{data}{Data frame containing study data.}

\item{required_cols}{Character vector of required column names.}
}
\value{
Logical. TRUE if data passes validation, FALSE otherwise.
}
\description{
This function performs basic validation checks on study data
to ensure data quality for economic analysis.
}
\examples{
study_data <- data.frame(
  patient_id = 1:10,
  cost = runif(10, 1000, 2000),
  outcome = runif(10, 0.8, 1.0)
)
validate_study_data(study_data, c("patient_id", "cost", "outcome"))
}