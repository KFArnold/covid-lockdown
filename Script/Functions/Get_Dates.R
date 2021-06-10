#' Get values of important dates for a designated country.
#' 
#' Data comes from the 'summary_eur.csv' file, which contains a number of important
#' policy and case threshold dates for all European countries.
#'
#' @param country Country
#' @param dates Character vector of date variable(s) to get; 
#' default is NULL, which returns all variables containing "Date"
#'
#' @return Named list containing values of all \code{dates} for \code{country};
#' note that the first letter of each of \code{dates} is returned as a lowercase,
#' to indicate that it represents the specific value of the input variable.
#'
#' @examples
#' Get_Dates(country = "United Kingdom", dates = c("Date_start", "Date_T"))
#' Get_Dates(country = "Denmark", dates = "Date_lockdown")
#' Get_Dates(country = "Germany")
Get_Dates <- function(country, dates = NULL) {
  
  # Load full dataframe containing important dates (if not already loaded)
  if(!exists("summary_eur")) {
    summary_eur <- read_csv("./Output/summary_eur.csv", 
                            col_types = cols())
  }
  
  # Filter dataframe by specified country
  summary_country <- summary_eur %>% 
    filter(Country == country) 
  
  # If dates parameter is null, select all variables containing string "Date";
  # else select specified variables
  if (is.null(dates)) {
    dates_df <- summary_country %>% select(contains("Date"))
  } else {
    dates_df <- summary_country %>% select(all_of(dates))
  }
  
  # Create list containing dates
  dates_list <- dates_df %>% as.list %>% 
    setNames(., First_Character_To_Lower(names(.)))
  
  # Return list containing dates
  return(dates_list)
  
}

