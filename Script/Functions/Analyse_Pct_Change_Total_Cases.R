#' Calculate the percentage change in total cases for each simulated
#' counterfactual history comared to in the simulated natural history, 
#' for a designated country.
#' 
#' Note that the change is expressed as a percentage of total cases in the 
#' simulated natural history.
#'
#' @param country Country
#'
#' @return Dataframe containing 4 columns: Country, Simulation (e.g. "0,1"), 
#' History (e.g. "Counterfactual history"), and Pct_change
#'
#' @examples
#' Analyse_Pct_Change_Total_Cases(country = "United Kingdom")
Analyse_Pct_Change_Total_Cases <- function(country) {
  
  # Import files containing simulated case data (cumulative)
  Import_All_Simulated_Data(filenames = "summary_cumulative_cases_end_sim",
                            silent = TRUE)
  
  # Calculate maximum date of observed data included in modelling 
  important_dates <- Get_Dates(country = country,
                               dates = "Date_T")
  list2env(important_dates, envir = environment())
  
  # Filter dataframe specified country 
  data_country <- summary_cumulative_cases_end_sim_all %>%
    filter(Country == country)
  
  # Calculate total cases on date_T for all simulations
  total_cases_all <- data_country %>% 
    filter(Date == date_T) %>%
    select(Country, Simulation, History, Mean)
  
  # Calculate total cases on date_T for natural history simulation only
  total_cases_nh <- total_cases_all %>%
    filter(Simulation == "0,0") %>%
    pull(Mean)
  
  # Calculate percent change in total cases compared to natural history
  pct_change_total_cases <- total_cases_all %>%
    mutate(Pct_change = (Mean - total_cases_nh) / total_cases_nh) %>%
    select(-Mean) %>%
    arrange(Simulation)
  
  # Return dataframe
  return(pct_change_total_cases)
  
}
