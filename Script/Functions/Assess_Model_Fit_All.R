#' Assess model fit for a designated country.
#' 
#' This function calls the following functions to calculate model fit statistics:
#' 'Calculate_Diff_Time_To_Thresholds', 'Calculate_Diff_Total_Cases', and
#' 'Calculate_Pois_Dev_Natural_History'.
#'
#' @param country Country
#'
#' @return Dataframe containing values of all model fit criteria.
#'
#' @examples
#' Assess_Model_Fit_All(country = "United Kingdom")
Assess_Model_Fit_All <- function(country) {
  
  # Calculate the difference in time taken for cases to go below important
  #' case thresholds in the observed data vs the simuated natural history
  diff_time_to_thresholds <- Assess_Model_Fit_Diff_Time_To_Thresholds(country = country) 
  
  # Calculate the difference in total cases at the end of the simulation period
  # in the observed data vs the simuated natural history
  diff_total_cases <- Assess_Model_Fit_Diff_Total_Cases(country = country)
  
  # Calculate the Poisson deviance between the observed data and simulated
  # natural history (both incident and cumulative cases)
  pois_dev_natural_history <- Assess_Model_Fit_Pois_Dev_Natural_History(country = country)
  
  # Combine all model fit statistics into single dataframe
  model_fit <- diff_time_to_thresholds %>% 
    bind_rows(., diff_total_cases) %>%
    bind_rows(., pois_dev_natural_history) %>%
    relocate(Threshold, .after = last_col())
  
  # Return dataframe containing all model fit criteria
  return(model_fit)
  
}
