#' Analyse within-country effects for a designated country.
#' 
#' The effects considered are: 
#' (1) the percentage change in time taken to reach population-based threshold 
#' values in the simulated counterfactual histories compared to the simulated 
#' natural history.
#' (2) the percentage change in length of lockdown required in the simulated 
#' counterfactual histories compared to the simulated natural history.
#' (3) the percentage change in total first wave cases in the simulated 
#' counterfactual histories compared to the simulated natural history.
#' 
#' This function calls the following functions to calculate effects:
#' 'Analyse_Pct_Change_Time_To_Threshold' and 'Analyse_Pct_Change_Total_Cases'.
#'
#' @param country Country
#'
#' @return Dataframe containing values of percentage change (Pct_change) for all
#' outcomes across all simulations, for the designated \code{country}.
#'
#' @examples
#' Analyse_Effects_Within_Country(country = "United Kingdom")
Analyse_Effects_Within_Country <- function(country) {
  
  # Get lockdown date in specified country
  important_dates <- Get_Dates(country = country,
                               dates = "Date_lockdown")
  list2env(important_dates, envir = environment())
  
  # Estimate percentage change in time to reach population-based thresholds
  # in simulated counterfactual histories compared to simulated natural history
  if (!is.na(date_lockdown)) {
    pct_change_time_to_threshold <- 
      Analyse_Pct_Change_Time_To_Threshold(country = country,
                                           thresholds = c("1 case per 100,000", 
                                                          "1 case per 20,000", 
                                                          "1 case per 10,000")) %>%
      mutate(Outcome = "Time_to_threshold")
  } else {
    pct_change_time_to_threshold <- tibble()
  }
  
  # Estimate percentage change in length of lockdown in simulated counterfactual
  # histories compared to simulated natural history
  if (!is.na(date_lockdown)) {
    pct_change_length_lockdown <- 
      Analyse_Pct_Change_Time_To_Threshold(country = country,
                                           thresholds = "Lockdown eased") %>%
      mutate(Outcome = "Length_lockdown",
             Threshold = NA)
  } else {
    pct_change_length_lockdown <- tibble()
  }
  
  # Estimate percentage change in total cases in simulated counterfactual 
  # histories compared to simulated natural history
  pct_change_total_cases <-
    Analyse_Pct_Change_Total_Cases(country = country) %>%
    mutate(Outcome = "Total_cases")
  
  # Bind all effects into single dataframe
  effects_within_country <- pct_change_time_to_threshold %>%
    bind_rows(., pct_change_length_lockdown) %>%
    bind_rows(., pct_change_total_cases) %>%
    relocate(Outcome, .after = Simulation) %>%
    arrange(Simulation)
  
  # Return dataframe
  return(effects_within_country)
  
}
