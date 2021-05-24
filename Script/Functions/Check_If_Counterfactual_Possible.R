#' Check if counterfactual condition is able to be simulated for a designated country.
#' 
#' Function checks whether specified combination of \code{n_days_first_restriction} and
#' \code{n_days_first_restriction} exists for \code{country} in the 
#' 'possible_days_counterfactual.csv' file, which contains all possible combinations
#' of shifts for all European countries.
#'
#' @param country Country
#' @param n_days_first_restriction Number of days to bring forward first restriction
#' @param n_days_lockdown Number of days to bring forward lockdown 
#' (NA if country did not enter lockdown)
#'
#' @return TRUE if counterfactual can be simulated, FALSE if not.
#'
#' @examples
#' Check_If_Counterfactual_Possible(country = "United Kingdom",
#' n_days_first_restriction = 0, n_days_lockdown = 3)
#' Check_If_Counterfactual_Possible(country = "Sweden",
#' n_days_first_restriction = 0, n_days_lockdown = NA)
Check_If_Counterfactual_Possible <- function(country,
                                             n_days_first_restriction, 
                                             n_days_lockdown) {
  
  # Load full dataframe containing possible counterfactuals (if not already loaded)
  if(!exists("possible_days_counterfactual")) {
    possible_days_counterfactual <- read_csv("./Output/possible_days_counterfactual.csv", 
                                             col_types = cols())
  }
  
  # Filter dataframe by specified country
  possible_days_counterfactual_country <- possible_days_counterfactual %>% 
    filter(Country == country) 
  
  # Combine specified n_days_first_restriction and n_days_lockdown parameters into dataframe
  n_days_counterfactual <- tibble(N_days_first_restriction = n_days_first_restriction,
                                  N_days_lockdown = n_days_lockdown)
  
  # Check whether combination of parameter values exists in specified
  # dataframe of possible counterfactuals
  match <- plyr::match_df(possible_days_counterfactual_country, n_days_counterfactual,
                          on = c("N_days_first_restriction", "N_days_lockdown"))
  counterfactual_possible <- ifelse(nrow(match) == 0, FALSE, TRUE)
  
  # Return whether counterfactual is possible (T/F)
  return(counterfactual_possible)
  
}
