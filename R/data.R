#' Sample Anesthesia Cost Data
#'
#' A dataset containing cost and outcome data for a comparison of anesthesia protocols
#'
#' @format A data frame with 100 rows and 8 variables:
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{treatment}{Treatment group: Standard or New_Protocol}
#'   \item{cost}{Total cost in USD}
#'   \item{qaly}{Quality-adjusted life years}
#'   \item{length_of_stay}{Hospital length of stay in days}
#'   \item{complications}{Binary indicator for complications (0/1)}
#'   \item{age}{Patient age in years}
#'   \item{bmi}{Body mass index}
#'   \item{asa_score}{ASA physical status classification (1-3)}
#' }
#' @source Simulated data for demonstration purposes
"anesthesia_costs"

#' Sample Clinical Trial Data
#'
#' A dataset containing clinical trial data comparing anesthesia protocols
#'
#' @format A data frame with 150 rows and 9 variables:
#' \describe{
#'   \item{patient_id}{Patient identifier}
#'   \item{treatment}{Treatment group: Standard or Experimental}
#'   \item{age}{Patient age in years}
#'   \item{gender}{Patient gender: Male or Female}
#'   \item{bmi}{Body mass index}
#'   \item{asa_score}{ASA physical status classification (1-4)}
#'   \item{time_to_event}{Time to complication or censoring in days}
#'   \item{event}{Event indicator (1 = complication, 0 = censored)}
#'   \item{pain_score}{Pain score on 0-10 scale}
#'   \item{length_of_stay}{Hospital length of stay in days}
#' }
#' @source Simulated data for demonstration purposes
"trial_data"