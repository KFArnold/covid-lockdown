#' Calculate range of dates to include model estimation and analysis.
#'
#' @param country Country
#' @param policy_dates Dataframe containing important policy dates 
#' (i.e. Date_lockdown, Date_lockdown_eased, Date_restrictions_eased)
#' @param population_pct_threshold Number expressing a proportion of the total
#' population, which represents the minimum total number of cumulative cases
#' that must have accrued before date can be included in analysis 
#' (default = 0.000001, or 0.0001% of total population / 1 case per million population)
#' @param min_case_threshold Minimum total number of cumulative cases that
#' must have accrued before date can be included in analysis (default = 5)
#'
#' @return Named list containing 3 values: 
#' (1) 'Country' = \code{country};
#' (2) 'Date_start' = first date for which total cumulative cases in the 
#' specified \code{country} go above the value of either 
#' \code{population_pct_threshold} or \code{min_case_threshold}, whichever
#' is greater; and
#' (3) 'Date_T' = 28 days after the date of lockdown easing (or after the date
#' of restriction easing, if the specified \code{country} did not enter lockdown).
#'
#' @examples
#' Calculate_Date_Range_For_Analysis(country = "United Kingdom",
#' policy_dates = tibble(Country = "United Kingdom", 
#' Date_lockdown = "2020-03-23", Date_lockdown_eased = "2020-05-11"))
Calculate_Date_Range_For_Analysis <- function(country, policy_dates, 
                                              population_pct_threshold = 0.000001,
                                              min_case_threshold = 5) {
  
  # Import datafrimes containing cases and demographic data into
  # global environment, if not already loaded
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "Worldbank_data_europe"),
                            silent = TRUE)
  
  # Calculate minimum date for analysis ----------------------------------------
  
  # Get most recent measure of population of specified country
  population <- Worldbank_data_europe %>%
    filter(Country == country) %>% 
    arrange(desc(Year)) %>%
    summarise(across(Population, ~first(na.omit(.))),
              .groups = "drop") %>%
    pull
  
  # Calculate value of defined population-based threshold
  population_threshold <- population * population_pct_threshold
  
  # Calculate which of defined thresholds is larger
  threshold <- max(population_threshold, min_case_threshold)
  
  # Calculate first date for which total cases goes above threshold
  date_start <- Cases_deaths_data_europe %>%
    filter(Country == country) %>%
    summarise(Date_start = Date[which(Cumulative_cases_beg >= threshold)[1]]) %>%
    pull(Date_start)
  
  # Calculate maximum date for analysis ----------------------------------------
  
  # Calculate maximum date for analysis as 28 days after date of 
  # restriction/lockdown easing
  date_T <- policy_dates %>%
    filter(Country == country) %>%
    summarise(Date_eased = if_else(is.na(Date_lockdown), 
                                Date_restrictions_eased, Date_lockdown_eased)) %>%
    pull(Date_eased) + 28
  
  # Return list of start and end of date range
  return(list(Country = country,
              Date_start = date_start,
              Date_T = date_T))
  
}
