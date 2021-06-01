#' Calculate important threshold values for a specified country.
#' 
#' Thresholds may be population-based (i.e. representing a proportion of the
#' total population) or case-based (i.e. representing the number of specified
#' cases on the date lockdown was eased).
#'
#' @param country Country
#' @param pop_thresholds Numeric vector of thresholds, each representing a proportion
#' of the total population (e.g. 0.01 = 1%)
#' @param cases_lockdown_easing Character vector containing the type of cases 
#' on the date of lockdown easing to use for calculating lockdown easing threshold
#' (default is 7-day moving average of daily cases, i.e. "Daily_cases_MA7")
#'
#' @return Summary dataframe containing 3 columns: (1) \code{country}; 
#' (2) text description of each of \code{thresholds} (e.g. 1 case per 100); and
#' (3) value of each of \code{thresholds}, expressed as a proportion of the
#' total population.
#'
#' @examples
#' Calculate_Pop_Threshold_Values(country = "United Kingdom", 
#' pop_thresholds = c(0.001, 0.005, 0.01))
Calculate_Threshold_Values <- function(country, 
                                       pop_thresholds, 
                                       cases_lockdown_easing = "Daily_cases_MA7") {
  
  # Import files containing observed data and summary statistics for all countries
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "summary_eur"),
                            silent = TRUE)
  
  # Get date of lockdown easing
  important_dates <- Get_Dates(country = country, 
                               dates = "Date_lockdown_eased") 
  list2env(important_dates, envir = environment())
  
  # Filter summary dataframe by specified country 
  summary_country <- summary_eur %>% 
    filter(Country == country) %>%
    select(Country, Population)
  
  # Calculate value of thresholds based on population
  pop_threshold_values <- summary_country %>%
    expand_grid(Threshold = pop_thresholds) %>%
    mutate(Threshold_value = Population * Threshold,
           Threshold = paste("1 case per", 
                             formatC(1/Threshold, format = "f", big.mark = ",", digits = 0))) %>%
    select(-Population)
  
  # Calculate value of specified cases on the date lockdown was eased
  if (!is.na(date_lockdown_eased)) {
    easing_threshold_values <- Cases_deaths_data_europe %>%
      filter(Country == country, Date == date_lockdown_eased) %>%
      select(Country, Threshold_value = all_of(cases_lockdown_easing)) %>%
      mutate(Threshold = "Lockdown eased")
  } else {
    easing_threshold_values <- tibble(Country = country,
                                      Threshold = "Lockdown_eased",
                                      Threshold_value = NA)
  }
  
  # Combine all thresholds and values into single dataframe
  thresholds <- bind_rows(pop_threshold_values,
                          easing_threshold_values)
  
  # Return dataframe of thresholds and values
  return(thresholds)
  
}
