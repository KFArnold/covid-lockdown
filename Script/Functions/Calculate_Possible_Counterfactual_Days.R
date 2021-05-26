#' Calculate all possible counterfactal conditions for a given list of knot dates
#' 
#' The first knot date cannot be moved earlier than the start date of the 
#' simulation, else we cannot know the effect of the first intervention. 
#' Similarly, the second knot date (if it exists) cannot be moved before the 
#' first knot date.
#'
#' @param country Country
#' @param knots Dataframe containg knot date pairs
#'
#' @return Named list ("possible_days_counterfactual") containing 1 dataframe 
#' with all possible combinations of N_days_first_restriction' and 'N_days_lockdown', 
#' i.e. number of days earlier first restrictions and lockdown can be moved forward, 
#' respectively. Note that a simple S3 error is returned if there are no knot
#' dates for the specified country.
#'
#' @examples
#' Calculate_Possible_Counterfactual_Days(country = "United Kingdom",
#' knots = knots_best)
Calculate_Possible_Counterfactual_Days <- function(country, knots) {
  
  # Filter knots datasets by specified country
  knots_country <- knots %>% filter(Country == country) 
  
  # If no best knots for specified country, stop
  if (nrow(knots_country) == 0) {
    stop(paste0("No counterfactuals are possible for ", country))
  }
  
  # Get important dates in designated country and save to environment
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_start", 
                                         "Date_first_restriction",
                                         "Date_lockdown")) %>%
    setNames(., tolower(names(.)))
  list2env(important_dates, envir = environment())
  
  # Record total number of knots (i.e. number of interventions)
  n_knots <- knots_country %>% pull(N_knots) %>% unique
  
  # Calculate maximum number of days earlier we can shift first restriction
  # (minimum value of knot_date_1 greater than or equal to date_start)
  max_days_counterfactual_first_restriction <- knots_country %>% 
    mutate(Diff = Knot_date_1 - date_start) %>% pull(Diff) %>% min %>% as.numeric 
  
  # Calculate minimum date we can estimate first restriction
  min_date_first_restriction <- date_first_restriction - max_days_counterfactual_first_restriction
  
  # Calculate possible date combinations for which counterfactual can be estimated
  if (n_knots == 2) {  # (2 interventions)
    
    if (date_first_restriction == date_lockdown) {  # (2 interventions implemented same day)
      
      # Determine all possible dates for first restriction 
      # (lockdown date is equal to date of first restriction)
      possible_dates_counterfactual <- 
        tibble(Date_first_restriction = seq.Date(min_date_first_restriction, 
                                                 date_first_restriction, 1),
               Date_lockdown = Date_first_restriction)
      
    } else {  # (2 interventions implemented distinct days)
      
      # Calculate minimum date we can estimate lockdown (must be after date of first restriction)
      min_date_lockdown <- min_date_first_restriction + 1
      
      # Determine all possible combinations of dates for first restriction and lockdown
      # (first restriction must be before lockdown)
      possible_dates_counterfactual <- 
        expand_grid(Date_first_restriction = seq.Date(min_date_first_restriction, 
                                                      date_first_restriction, 1),
                    Date_lockdown = seq.Date(min_date_lockdown, date_lockdown, 1)) %>%
        filter(Date_first_restriction < Date_lockdown)
      
    }
    
  } else {  # (1 intervention)
    
    # Determine all possible dates for first restriction 
    # (lockdown date is NA)
    possible_dates_counterfactual <- 
      tibble(Date_first_restriction = seq.Date(min_date_first_restriction, 
                                               date_first_restriction, 1),
             Date_lockdown = as.Date(NA))
    
  }
  
  # Determine all possible counterfactual shifts for first restriction
  possible_days_counterfactual <- possible_dates_counterfactual %>%
    mutate(N_days_first_restriction = as.numeric(date_first_restriction - Date_first_restriction),
           N_days_lockdown = as.numeric(date_lockdown - Date_lockdown)) 
  
  # Label dataframe with country, n_knots
  possible_days_counterfactual <- possible_days_counterfactual %>% 
    mutate(Country = country, N_knots = n_knots) %>%
    arrange(N_days_first_restriction, N_days_lockdown) %>%
    relocate(Country, N_knots, N_days_first_restriction, N_days_lockdown)
  
  # Return dataframe containing possible counterfactual days
  return(list(possible_days_counterfactual = possible_days_counterfactual))
  
}
