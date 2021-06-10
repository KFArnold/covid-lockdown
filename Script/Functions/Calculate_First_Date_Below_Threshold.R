#' Calculate the first date for which cases went below a designated threshold value
#' after they peaked.
#'
#' @param data Dataframe 
#' @param cases Variable in \code{data} corresponding to cases
#' @param threshold_value Value of threshold
#' @param date_lockdown Date of lockdown
#'
#' @return List containing 4 elements: 
#' (1) "Threshold_value" = \code{threshold_value};
#' (2) "Threshold_exceeded" = T/F indicator for whether the specified \code{cases} 
#' ever exceeded the specified \code{threshold_value}; 
#' (3) "Date_cases_below_threshold" = date indicating the first day after their
#' peak that the specified \code{cases} went below the specified \code{threshold_value}; and
#' (4) "Days_since_lockdown" = number of days since \code{date_lockdown} until 
#' the the specified \code{cases} went below the specified \code{threshold_value}
#' (note that the value 0 is returned if cases never went as high as the threshold).
#'
#' @examples
#' Calculate_First_Date_Below_Threshold(data = filter(Cases_deaths_data_europe, Country == country, Date <= date_T),
#' cases = "Daily_cases_MA7", threshold_value = 50, date_lockdown = .as.Date("2020-03-23"))
Calculate_First_Date_Below_Threshold <- function(data, cases, threshold_value, date_lockdown) {
  
  # Calculate peak value of designated cases
  peak_value <- data %>% 
    select(all_of(cases)) %>% 
    max
  
  # Calculate date when peak value reached
  peak_date <- data %>% 
    filter(eval(parse(text = cases)) == peak_value) %>%
    pull(Date) %>% head(1)
  
  # Create T/F indicator for whether threshold value was ever exceeded
  threshold_exceeded <- peak_value > threshold_value
  
  # If threshold value was exceeded, find first date specified cases went below threshold
  if (threshold_exceeded == TRUE) {
    
    # Filter dataset by dates >= date of peak
    data_i <- data %>% filter(Date >= peak_date)
    
    # Calculate and record first date cases fall below threshold
    date_cases_below_threshold <- data_i %>% 
      filter(eval(parse(text = cases)) <= threshold_value) %>% 
      slice(1) %>% pull(Date)
    
    # If cases never go below threshold, record date as NA
    if (length(date_cases_below_threshold) == 0) {
      date_cases_below_threshold <- as.Date(NA)
    }
    
    # Calculate number of days since lockdown until cases fall below threshold
    days_since_lockdown <- as.numeric(date_cases_below_threshold - date_lockdown)
    
  } else {
    
    # If threshold value was never exceeded, record date as NA
    # and record number of days since lockdown until cases to fall below threshold as 0
    date_cases_below_threshold <- NA
    days_since_lockdown <- 0
    
  }
  
  # Return list describing threshold value, whether it was ever exceeded,
  # and (if so) the first date that the specified cases went below the threshold
  return(list(Threshold_value = threshold_value,
              Threshold_exceeded = threshold_exceeded,
              Date_cases_below_threshold = date_cases_below_threshold,
              Days_since_lockdown = days_since_lockdown))
  
}
