#' Calculate the difference in total cases at the end of the simulation period 
#' (i.e. date_T) in the observed data vs the simulated natural history,
#' for a given country.
#' 
#' Note that the difference is calculated as observed - simulated, so that negative 
#' values indicate the model overestimates total cases, and positive values 
#' indicate the model underestimates total cases. Percent differences are calculated
#' as a proportion of observed total cases.
#'
#' @param country Country
#'
#' @return Dataframe with \code{country}, description, type (i.e. number 
#' vs percentage), and value
#'
#' @examples
#' Assess_Model_Fit_Diff_Total_Cases(country = "United Kingdom")
Assess_Model_Fit_Diff_Total_Cases <- function(country) {
  
  # Import files containing observed and simulated case data 
  Import_Unloaded_CSV_Files(filenames = "Cases_deaths_data_europe",
                            silent = TRUE)
  Import_All_Simulated_Data(filenames = "summary_cumulative_cases_end_sim",
                            silent = TRUE)
  
  # Calculate maximum date of observed data included in modelling 
  important_dates <- Get_Dates(country = country,
                               dates = "Date_T")
  list2env(important_dates, envir = environment())
  
  # Calculate total observed and simulated cases on date_T
  cases_T_obs <- Cases_deaths_data_europe %>% 
    filter(Country == country, Date == date_T) %>%
    pull(Cumulative_cases_end)
  cases_T_sim <- summary_cumulative_cases_end_sim_all %>% 
    filter(Country == country, History == "Natural history", Date == date_T) %>%
    pull(Mean)
  
  # Calculate difference between observed and simulated cases on date_T
  # (raw and percentage differences)
  diff_cases_T <- cases_T_obs - cases_T_sim
  pct_diff_cases_T <- diff_cases_T / cases_T_obs
  
  # Return difference
  return(tibble(Country = country,
                Measure = "Diff_total_cases",
                Type = c("Number", "Pct"), 
                Value = c(diff_cases_T, pct_diff_cases_T)))
}
