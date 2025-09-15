\name{calculate_anesthesia_cost}
\alias{calculate_anesthesia_cost}
\title{Calculate Total Anesthesia Costs}
\usage{
calculate_anesthesia_cost(personnel_cost, duration_hours, medication_cost, equipment_cost)
}
\arguments{
\item{personnel_cost}{Numeric. Cost of anesthesia personnel per hour.}

\item{duration_hours}{Numeric. Duration of procedure in hours.}

\item{medication_cost}{Numeric. Cost of anesthetic medications.}

\item{equipment_cost}{Numeric. Cost of equipment usage.}
}
\value{
Numeric. Total anesthesia cost.
}
\description{
This function calculates total costs for anesthesia procedures including
personnel, equipment, and medication costs.
}
\examples{
# Calculate cost for a 2-hour surgery
calculate_anesthesia_cost(150, 2, 45, 25)
}