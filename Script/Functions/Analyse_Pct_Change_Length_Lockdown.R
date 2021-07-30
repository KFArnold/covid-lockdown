#' Calculate the percentage change in length of lockdown for each simulated 
#' counterfactual history compared to the simulated natural history, 
#' for a designated country.
#' 
#' Note that the change is expressed as a percentage of length of lockdown
#' in the simulated natural history.
#'
#' @param country Country
#'
#' @return Dataframe containing 4 columns: Country, History, Simulation, 
#' Value (of length of lockdown) and Pct_change
#'
#' @examples Analyse_Pct_Change_Length_Lockdown(country = "United Kingdom")
Analyse_Pct_Change_Length_Lockdown <- function(country) {
  
  # Calculate length of lockdown for all simulations
  length_lockdown <- Calculate_Length_Lockdown_Sim(country = country)
  
  # Calculate length of lockdown in simulated natural history
  length_lockdown_nh <- length_lockdown %>%
    filter(Simulation == "Natural history") %>%
    pull(Length_lockdown)
  
  # Calculate percent change in length of lockdown compared to natural history
  pct_change_length_lockdown <- length_lockdown %>%
    mutate(Pct_change = 
             ((Length_lockdown - length_lockdown_nh) / length_lockdown_nh)) %>%
    rename(Value = Length_lockdown)
  
  # Return dataframe
  return(pct_change_length_lockdown)
  
}
