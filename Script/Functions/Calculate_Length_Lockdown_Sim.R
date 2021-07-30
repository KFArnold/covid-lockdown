#' Calculate length of lockdown for a designated country across all 
#' simulated histories.
#'
#' @param country Country
#'
#' @return Dataframe containing 4 columns: Country, History, Simulation, and
#' Length_lockdown
#'
#' @examples Calculate_Length_Lockdown_Sim(country = "United Kingdom")
Calculate_Length_Lockdown_Sim <- function(country) {
  
  # Import files containing simulated case data
  Import_All_Simulated_Data(filenames = "summary_daily_cases_sim",
                            silent = TRUE)
  
  # Get observed lockdown and lockdown easing dates, and length of lockdown
  # in specified country
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_lockdown",
                                         "Date_lockdown_eased",
                                         "Length_lockdown"))
  list2env(important_dates, envir = environment())
  
  # Filter simulated data by specified country 
  data_country <- summary_daily_cases_sim_all %>%
    filter(Country == country)
  
  # Get value of daily cases on date of lockdown easing under natural history
  lockdown_easing_threshold <- data_country %>% 
    filter(History == "Natural history",
           Date == date_lockdown_eased) %>%
    pull(Mean)
  
  # Get simulations performed for specified country
  simulations <- data_country %>% 
    pull(Simulation) %>% as.character %>% unique
  
  # Create empty list
  length_lockdown_sim <- list()
  
  # Loop through all simulations to calculate number of days since lockdown until
  # mean value of simulated daily cases went below thresholds
  for (i in simulations) {
    
    # If simulation is natural history, set length of lockdown to observed length of lockdown
    # (since this is by definition the first date when the lockdown easing threshold was reached)
    # Else, calculate length of lockdown as first date for which simulated 
    # cases weny below lockdown easing threshold
    if (i == "Natural history") {
      
      # Record length of lockdown
      length_lockdown_sim_i <- tibble(Simulation = "Natural history",
                                      Length_lockdown = length_lockdown)
      
    } else {
      
      # Filter dataframe by specified simulation
      data_country_i <- data_country %>%
        filter(Simulation == i)
      
      # Record (counterfactual) date of lockdown in specified simulation
      date_lockdown_i <- data_country_i %>% pull(Date_lockdown) %>% unique
      
      # Calculate number of days since lockdown until mean value of simulated 
      # daily cases went below lockdown easing threshold
      length_lockdown_sim_i <- Calculate_First_Date_Below_Threshold(data = data_country_i,
                                                                    cases = "Mean",
                                                                    threshold_value = lockdown_easing_threshold,
                                                                    date_lockdown = date_lockdown_i) %>%
        bind_rows %>%
        select(Length_lockdown = Days_since_lockdown)
      
    }
    
    # Add to full list
    length_lockdown_sim[[i]] <- length_lockdown_sim_i
    
  }
  
  # Combine results from all simulations
  length_lockdown_sim <- length_lockdown_sim %>% 
    bind_rows(.id = "Simulation") %>%
    mutate(Country = country,
           History = ifelse(Simulation == "Natural history", 
                            "Natural history",
                            "Counterfactual history")) %>%
    relocate(Country, History, Simulation)
  
  # Return dataframe
  return(length_lockdown_sim)
  
}
