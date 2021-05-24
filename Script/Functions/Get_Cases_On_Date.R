#' Get number(s) of cases on a given date for a designated country.
#' 
#' Data from 'Cases_deaths_data_europe.csv' file, which contains observed values
#' of cases and deaths (including 7-day moving averages) for all European countries.
#'
#' @param country Country
#' @param date Character vector (length 1) of date variable to get
#' @param casetypes Character vector of type(s) of case variable(s) to get
#'
#' @return Named list containing values of all \code{casetypes} on \code{date} for \code{country}
#'
#' @examples
#' Get_Cases_On_Date(country = "United Kingdom", date = as.Date("2020-03-02"),
#' casetypes = c("Daily_cases_MA7", "Cumulative_cases_end_MA7"))
Get_Cases_On_Date <- function(country, date, casetypes) {
  
  # Print warning if more than one country or date variable are specified
  if(length(country) != 1 | length(date) != 1) {
    stop("Only one country and one date can be specified.")
  }
  
  # Load full formatted dataframe containing observed cases (if not already loaded)
  if(!exists("Cases_deaths_data_europe")) {
    Cases_deaths_data_europe <- read_csv("./Data/Formatted/Cases_deaths_data_europe.csv",
                                         col_types = cols())
  }
  
  # Filter dataframe by specified country, date, and casetypes
  cases_df <- Cases_deaths_data_europe %>%
    filter(Country == country, Date == date) %>%
    select(all_of(casetypes))
  
  # Create list containing values of cases on date
  cases_list <- cases_df %>% as.list
  
  # Return list containing dates
  return(cases_list)
  
}
