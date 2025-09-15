#' Calculate Cost-Effectiveness Ratio
#'
#' This function calculates the incremental cost-effectiveness ratio (ICER)
#' for comparing medical interventions in anesthesia.
#'
#' @param cost_intervention Numeric. Cost of the intervention.
#' @param cost_control Numeric. Cost of the control/standard treatment.
#' @param effect_intervention Numeric. Effectiveness measure for intervention.
#' @param effect_control Numeric. Effectiveness measure for control.
#'
#' @return Numeric. The incremental cost-effectiveness ratio.
#' @export
#'
#' @examples
#' # Calculate ICER for a new anesthetic technique
#' calculate_icer(1500, 1200, 0.95, 0.85)
calculate_icer <- function(cost_intervention, cost_control, 
                          effect_intervention, effect_control) {
  
  if (!is.numeric(cost_intervention) || !is.numeric(cost_control) ||
      !is.numeric(effect_intervention) || !is.numeric(effect_control)) {
    stop("All inputs must be numeric")
  }
  
  delta_cost <- cost_intervention - cost_control
  delta_effect <- effect_intervention - effect_control
  
  if (delta_effect == 0) {
    warning("Zero effectiveness difference detected")
    return(Inf)
  }
  
  icer <- delta_cost / delta_effect
  return(icer)
}

#' Calculate Total Anesthesia Costs
#'
#' This function calculates total costs for anesthesia procedures including
#' personnel, equipment, and medication costs.
#'
#' @param personnel_cost Numeric. Cost of anesthesia personnel per hour.
#' @param duration_hours Numeric. Duration of procedure in hours.
#' @param medication_cost Numeric. Cost of anesthetic medications.
#' @param equipment_cost Numeric. Cost of equipment usage.
#'
#' @return Numeric. Total anesthesia cost.
#' @export
#'
#' @examples
#' # Calculate cost for a 2-hour surgery
#' calculate_anesthesia_cost(150, 2, 45, 25)
calculate_anesthesia_cost <- function(personnel_cost, duration_hours, 
                                    medication_cost, equipment_cost) {
  
  if (!is.numeric(personnel_cost) || !is.numeric(duration_hours) ||
      !is.numeric(medication_cost) || !is.numeric(equipment_cost)) {
    stop("All inputs must be numeric")
  }
  
  if (duration_hours < 0) {
    stop("Duration must be non-negative")
  }
  
  total_cost <- (personnel_cost * duration_hours) + medication_cost + equipment_cost
  return(total_cost)
}