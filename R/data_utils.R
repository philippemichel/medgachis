#' Summarize Patient Outcomes
#'
#' This function provides summary statistics for patient outcomes
#' in anesthesia studies.
#'
#' @param outcomes Numeric vector. Patient outcome measures.
#' @param group Character vector. Group assignment for each patient.
#'
#' @return Data frame with summary statistics by group.
#' @export
#'
#' @examples
#' outcomes <- c(95, 92, 88, 96, 89, 94, 91, 93)
#' groups <- c(rep("intervention", 4), rep("control", 4))
#' summarize_outcomes(outcomes, groups)
summarize_outcomes <- function(outcomes, group) {
  
  if (!is.numeric(outcomes)) {
    stop("Outcomes must be numeric")
  }
  
  if (length(outcomes) != length(group)) {
    stop("Outcomes and group vectors must have the same length")
  }
  
  df <- data.frame(
    outcome = outcomes,
    group = group,
    stringsAsFactors = FALSE
  )
  
  result <- aggregate(outcome ~ group, data = df, FUN = function(x) {
    c(
      n = length(x),
      mean = mean(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      min = min(x, na.rm = TRUE),
      max = max(x, na.rm = TRUE)
    )
  })
  
  # Convert matrix columns to separate columns
  outcome_stats <- data.frame(result$outcome)
  names(outcome_stats) <- c("n", "mean", "sd", "median", "min", "max")
  
  final_result <- data.frame(
    group = result$group,
    outcome_stats,
    stringsAsFactors = FALSE
  )
  
  return(final_result)
}

#' Validate Study Data
#'
#' This function performs basic validation checks on study data
#' to ensure data quality for economic analysis.
#'
#' @param data Data frame containing study data.
#' @param required_cols Character vector of required column names.
#'
#' @return Logical. TRUE if data passes validation, FALSE otherwise.
#' @export
#'
#' @examples
#' study_data <- data.frame(
#'   patient_id = 1:10,
#'   cost = runif(10, 1000, 2000),
#'   outcome = runif(10, 0.8, 1.0)
#' )
#' validate_study_data(study_data, c("patient_id", "cost", "outcome"))
validate_study_data <- function(data, required_cols) {
  
  if (!is.data.frame(data)) {
    warning("Input is not a data frame")
    return(FALSE)
  }
  
  if (nrow(data) == 0) {
    warning("Data frame is empty")
    return(FALSE)
  }
  
  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    warning(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    return(FALSE)
  }
  
  # Check for any completely empty columns
  empty_cols <- names(data)[sapply(data, function(x) all(is.na(x)))]
  if (length(empty_cols) > 0) {
    warning(paste("Empty columns detected:", paste(empty_cols, collapse = ", ")))
    return(FALSE)
  }
  
  return(TRUE)
}