#' Calculate the percentage change in time taken for cases to go below 
#' important case thresholds for each simulated counterfactual history compared
#' to the simulated natural history, for a designated country.
#' 
#' Note that the change is expressed as a percentage of the time taken to go below
#' thresholds in the simulated natural history.
#'
#' @param country Country
#' @param thresholds Vector of important case thresholds (possible values =
#' c("1 case per 100,000", "1 case per 20,000", "1 case per 10,000", "Length_lockdown"))
#'
#' @return Dataframe containing 6 columns: Country, History, Simulation, 
#' Value (of days to threshold), Pct_change, and Threshold
#'
#' @examples
#' Analyse_Pct_Change_Time_To_Threshold(country = "United Kingdom",
#' thresholds = c("1 case per 100,000", "1 case per 20,000", "1 case per 10,000"))
Analyse_Pct_Change_Time_To_Threshold <- function(country, thresholds) {
  
  # Import files containing important case thresholds and simulated case data
  Import_Unloaded_CSV_Files(filenames = "thresholds_eur",
                            silent = TRUE)
  Import_All_Simulated_Data(filenames = "summary_daily_cases_sim",
                            silent = TRUE)
  
  # Filter dataframes containing important threshold values and simulated data by 
  # specified country and thresholds
  thresholds_country <- thresholds_eur %>%
    filter(Country == country, Threshold %in% thresholds)
  data_country <- summary_daily_cases_sim_all %>%
    filter(Country == country)
  
  # Get values of specified thresholds thresholds
  threshold_values <- thresholds_country %>% pull(Threshold_value)
  
  # Get simulations performed for specified country
  simulations <- data_country %>% 
    pull(Simulation) %>% as.character %>% unique
  
  # Create empty list
  dates_below_thresholds <- list()
  
  # Loop through all simulations to calculate number of days since lockdown until
  # mean value of simulated daily cases went below thresholds
  for (i in simulations) {
    
    # Filter dataframe by specified simulation
    data_country_i <- data_country %>%
      filter(Simulation == i)
    
    # Record (counterfactual) date of lockdown in specified simulation
    date_lockdown_i <- data_country_i %>% pull(Date_lockdown) %>% unique
    
    # Calculate number of days since lockdown until mean value of simulated 
    # daily cases went below thresholds
    dates_below_thresholds_i <- foreach(k = threshold_values,
                                      .errorhandling = "pass") %do%
      Calculate_First_Date_Below_Threshold(data = data_country_i,
                                           cases = "Mean",
                                           threshold_value = k,
                                           date_lockdown = date_lockdown_i) %>%
      bind_rows %>%
      select(Threshold_value, Days_since_lockdown)

    # Add to full list
    dates_below_thresholds[[i]] <- dates_below_thresholds_i
    
  }
  
  # Combind results from all simulations
  dates_below_thresholds <- dates_below_thresholds %>% 
    bind_rows(.id = "Simulation") 
  
  # Calculate number of days taken to go below thresholds in simulated natural history
  dates_below_thresholds_nh <- dates_below_thresholds %>%
    filter(Simulation == "Natural history") %>%
    select(Threshold_value, Days_since_lockdown_nh = Days_since_lockdown)
  
  # Calculate percent change in days to reach thresholds compared to natural history
  pct_change_time_to_threshold <- dates_below_thresholds %>%
    full_join(., dates_below_thresholds_nh,
              by = "Threshold_value") %>%
    mutate(Pct_change = 
             ((Days_since_lockdown - Days_since_lockdown_nh) / Days_since_lockdown_nh)) %>%
    full_join(thresholds_country, ., by = "Threshold_value") %>%
    select(-c(Days_since_lockdown_nh, Threshold_value)) %>%
    rename(Value = Days_since_lockdown)
  
  # Label dataframe with history and arrange by simulation
  pct_change_time_to_threshold <- pct_change_time_to_threshold %>%
    mutate(History = ifelse(Simulation == "Natural history", 
                            "Natural history",
                            "Counterfactual history")) %>%
    relocate(Country, History, Simulation, Value, Pct_change) %>%
    arrange(Simulation)
  
  # Return dataframe
  return(pct_change_time_to_threshold)
  
}
