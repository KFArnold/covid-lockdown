#' Check if permissible values of simulation parameters are specified.
#'
#' @param simulation_type Type of simulation (one of 
#' c("shift_intervention_sequence", "time_between_interventions"))
#' @param n_days_first_restriction Number of days to bring forward first restriction
#' @param n_days_lockdown Number of days to bring forward lockdown
#' @param days_between_interventions Number of days between first restriction
#' and lockdown
#' @param date_lockdown Date of lockdown
#'
#' @return Error message if any parameters are mis-specified.
#'
#' @examples Check_Simulation_Parameters(simulation_type = "shift_intervention_sequence",
#' n_days_first_restriction = 1, n_days_lockdown = 1, 
#' days_between_interventions = rlang::missing_arg(), 
#' date_lockdown = as.Date("2020-03-23"))
#' Check_Simulation_Parameters(simulation_type = "time_between_interventions",
#' n_days_first_restriction = 7, n_days_lockdown = rlang::missing_arg(),
#' days_between_interventions = "min", date_lockdown = as.Date("2020-03-23"))
Check_Simulation_Parameters <- function(simulation_type,
                                        n_days_first_restriction,
                                        n_days_lockdown,
                                        days_between_interventions,
                                        date_lockdown) {
  
  # Check that permissible value of simulation_type specified, and check that
  # correct counterfactual parameters are specified for the correct type of simulation  
  if (!simulation_type %in% c("shift_intervention_sequence", "time_between_interventions")) {
    error_message <- paste("Only the following values of the parameter 'simulation_type' are allowed:", 
                             "c('shift_intervention_sequence', 'time_between_interventions')",
                             sep = "\n")
  } else if (simulation_type == "shift_intervention_sequence") {
    # Print messages for the following reasons:
    # (1) if days_between_interventions is specified
    # (2) if either n_days_first_restriction or n_days_lockdown are missing
    # (3) if either n_days_first_restriction or n_days_lockdown are less than zero
    if (!is_missing(maybe_missing(days_between_interventions))) {
      error_message <- paste("For simulation_type = 'shift_intervention_sequence', the following argument",
                     "is not relevant and should not be specified:",
                     "days_between_interventions",
                     sep = "\n")
    } else if (is_missing(maybe_missing(n_days_first_restriction)) || 
               is_missing(maybe_missing(n_days_lockdown))) {
      error_message <- paste("For simulation_type = 'shift_intervention_sequence', the following arguments must be specified:",
                     "n_days_first_restriction, n_days_lockdown",
                     sep = "\n")
    } else if (n_days_first_restriction < 0 || n_days_lockdown < 0) {
      error_message <- paste("For simulation type = 'shift_intervention_sequence', the following arguments must be >= 0:",
                     "n_days_first_restriction, n_days_lockdown",
                     sep = "\n")
    }
  } else if (simulation_type == "time_between_interventions") {
    # Print messages or the following reasons:
    # (1) if days_between_interventions is not specified
    # (2) if both n_days_first_restriction and n_days_lockdown are specified
    # (3) if both n_days_first_restriction and n_days_lockdown are missing
    # (4)-(5) if either n_days_first_restriction or n_days_lockdown are less than zero
    # (6) if days_between_interventions is less than or equal to zero
    # (7) if the country did not enter lockdown
    if (is_missing(maybe_missing(days_between_interventions))) {
      error_message <- paste("For simulation type = 'time_between_interventions', the following argument must be specified:",
                     "days_between_interventions",
                     sep = "\n")
    } else if (!is_missing(maybe_missing(n_days_first_restriction)) && 
               !is_missing(maybe_missing(n_days_lockdown))) {
      error_message <- paste("For simulation type = 'time_between_interventions', only one of the following arguments can be specified:",
                     "n_days_first_restriction, n_days_lockdown",
                     sep = "\n")
    } else if (is_missing(maybe_missing(n_days_first_restriction)) && 
               is_missing(maybe_missing(n_days_lockdown))) {
      error_message <- paste("For simulation type = 'time_between_interventions', one of the following arguments must be specified:",
                     "n_days_first_restriction, n_days_lockdown",
                     sep = "\n")
    } else if (!is_missing(maybe_missing(n_days_first_restriction)) && n_days_first_restriction < 0) {
      error_message <- paste("For simulation type = 'time_between_interventions', the following argument must be >= 0:",
                     "n_days_first_restriction",
                     sep = "\n")
    } else if (!is_missing(maybe_missing(n_days_lockdown)) && n_days_lockdown < 0) {
      error_message <- paste("For simulation type = 'time_between_interventions', the following argument must be >= 0:",
                     "n_days_lockdown",
                     sep = "\n")
    } else if (!days_between_interventions %in% c("min", seq(1, 100, 1))) {
      error_message <- paste("For simulation type = 'time_between_interventions', the following argument must be",
                     "either 'min' or an integer > 0:",
                     "days_between_interventions",
                     sep = "\n")
    } else if (is.na(date_lockdown)) {
      error_message <- paste("Country did not enter lockdown. Simulation type = 'time_between_interventions'",
                    "may only be executed for countries with two distinct interventions.",
                    sep = "\n")
    }
  }
  
  # Return error message 
  if (exists("error_message")) { return(error_message) } 
  
}
