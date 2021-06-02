#' Calculate the Poisson deviance between the observed data and the simulated 
#' natural history (both incident and cumulative cases), for a given country.
#'
#' @param country Country
#'
#' @return Dataframe with \code{country}, description, type (i.e. number), and value
#'
#' @examples 
#' Assess_Model_Fit_Pois_Dev_Natural_History(country = "United Kingdom")
Assess_Model_Fit_Pois_Dev_Natural_History <- function(country) {
  
  # Import files containing observed and simulated case data 
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe"),
                            silent = TRUE)
  Import_All_Simulated_Data(silent = TRUE)
  
  # Calculate maximum date of observed data included in modelling and date of lockdown
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_start", "Date_T"))
  list2env(important_dates, envir = environment())
  
  # Get observed and simulated case data
  cases_obs <- Cases_deaths_data_europe %>% 
    filter(Country == country, Date >= date_start, Date <= date_T)
  cases_daily_sim <- summary_daily_cases_sim_all %>%
    filter(Country == country, History == "Natural history",
           Date >= date_start, Date <= date_T) 
  cases_cum_sim <- summary_cumulative_cases_end_sim_all %>% 
    filter(Country == country, History == "Natural history",
           Date >= date_start, Date <= date_T) 
  
  # Calculate and record Poisson deviances for observed (7-day moving aberage) 
  # vs simulated incident and cumulative cases
  pois_dev_inc <- Calculate_Poisson_Deviance(obs = cases_obs$Daily_cases_MA7,
                             pred = cases_daily_sim$Mean)
  pois_dev_cum <- Calculate_Poisson_Deviance(obs = cases_obs$Cumulative_cases_end_MA7,
                                             pred = cases_cum_sim$Mean)
  
  # Return dataframe of Poisson deviance for both incident and cumulative cases
  return(tibble(Country = country,
                Measure = c("Pois_dev_inc", "Pois_dev_cum"),
                Type = "Number",
                Value = c(pois_dev_inc, pois_dev_cum)))
  
}
