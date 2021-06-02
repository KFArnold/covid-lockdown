#' Calculate the difference in time taken for cases to go below important
#' case thresholds in the observed data vs the simuated natural history, for a 
#' designated country.
#' 
#' Note that the difference is calculated as observed - simulated, so that negative 
#' values indicate the model takes longer to reach the case thresholds (i.e. it
#' overestimates incidence), and positive values indicate the model takes less time
#' to reach the case thresholds (i.e. it underestimates incidence). Percent 
#' differences are calculated as a proportion of observed time taken to reach thresholds.
#'
#' @param country Country
#'
#' @return Dataframe with \code{country}, description, threshold descriptions,
#' threshold values, type (i.e. number vs percentage), and value
#'
#' @examples
#' Calculate(Diff_Time_To_Threshold(country = "United Kingdom"))
Assess_Model_Fit_Diff_Time_To_Thresholds <- function(country) {
  
  # Import files containing observed case data and important thresholds,
  # and simulated case data
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "thresholds_eur"),
                            silent = TRUE)
  Import_All_Simulated_Data(filenames = "summary_daily_cases_sim",
                            silent = TRUE)
  
  # Calculate maximum date of observed data included in modelling and date of lockdown
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_lockdown", "Date_T"))
  list2env(important_dates, envir = environment())
  
  # Filter dataframe containing observed cases by country and maximum date,
  # and filter dataframe containing simulated cases by country and natural history
  data_country_obs <- Cases_deaths_data_europe %>% 
    filter(Country == country, Date <= date_T) 
  data_country_sim <- summary_daily_cases_sim_all %>%
    filter(Country == country, History == "Natural history")
  
  # Filter dataframe containing important threshold values by country
  thresholds_country <- thresholds_eur %>%
    filter(Country == country)
  
  # Get values of important thresholds
  threshold_values <- thresholds_country %>% pull(Threshold_value)
  
  # Calculate number of days since lockdown until 7-day MA of observed daily cases 
  # went below thresholds
  # (remove rows where incidence never went as high as given threshold -
  # coded as Days_since_lockdown = 0)
  dates_below_thresholds_obs <- foreach(k = threshold_values,
                                             .errorhandling = "pass") %do%
    Calculate_First_Date_Below_Threshold(data = data_country_obs,
                                         cases = "Daily_cases_MA7",
                                         threshold_value = k,
                                         date_lockdown = date_lockdown) %>%
    reduce(bind_rows) %>%
    select(Threshold_value, Days_since_lockdown) %>%
    filter(Days_since_lockdown != 0)

  # Calculate number of days since lockdown until mean simulated daily cases 
  # under the natural history went below thresholds
  # (remove rows where incidence never went as high as given threshold -
  # coded as Days_since_lockdown = 0)
  dates_below_thresholds_sim <- foreach(k = threshold_values,
                                       .errorhandling = "pass") %do%
    Calculate_First_Date_Below_Threshold(data = data_country_sim,
                                         cases = "Mean",
                                         threshold_value = k,
                                         date_lockdown = date_lockdown) %>%
    reduce(bind_rows) %>%
    select(Threshold_value, Days_since_lockdown) %>%
    filter(Days_since_lockdown != 0)

  # Join summary tables and calculate difference in time to reach thresholds
  # between observed and simulated
  diff_time_to_thresholds <- full_join(dates_below_thresholds_obs, dates_below_thresholds_sim,
            by = c("Threshold_value"),
            suffix = c("_obs", "_sim")) %>%
    full_join(thresholds_country, .,
              by = "Threshold_value") %>%
    mutate(Diff = as.numeric(Days_since_lockdown_obs - Days_since_lockdown_sim),
           Pct_diff = Diff / Days_since_lockdown_obs) %>%
    select(-contains("Days"), -"Threshold_value") %>%
    rename(Number = Diff, Pct = Pct_diff) %>%
    pivot_longer(cols = c(Number, Pct), names_to = "Type", values_to = "Value") %>%
    mutate(Measure = "Diff_time_to_threshold") %>%
    relocate(Measure, .after = Country) 
  
  # Return dataframe 
  return(diff_time_to_thresholds)
  
}
