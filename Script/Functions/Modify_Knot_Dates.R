#' Modify knot dates for a designated country.
#' 
#' Function modifies the 'knots_best.csv' file, which contains best knot date pairs
#' associated with the observed dates of first restriction and lockdown 
#' for all European countries.
#'
#' @param country Country
#' @param n_days_first_restriction Number of days to bring forward first restriction
#' @param n_days_lockdown Number of days to bring forward lockdown
#' (NA if country did not enter lockdown)
#'
#' @return Modified version of 'knots_best' dataframe, in which:
#' 'Country' = \code{country}; Date_first_restriction' and 'Knot_date_1' variables 
#' have been shifted forward by \code{n_days_first_restriction};
#' and 'Date lockdown' and 'Knot_date_2' variables have been shifted forward
#' by \code{n_days_lockdown}. All other variables remain unmodified
#'
#' @examples
#' Modify_Knot_Dates(country = "United Kingdom", n_days_first_restriction = 0,
#' n_days_lockdown = 3)
#' Modify_Knot_Dates(country = "Sweden", n_days_first_restriction = 0,
#' n_days_lockdown = NA)
Modify_Knot_Dates <- function(country, n_days_first_restriction, n_days_lockdown) {
  
  # Load full dataframe containing best knot date pairs (if not already loaded)
  if(!exists("knots_best")) {
    knots_best <- read_csv("./Output/knots_best.csv", 
                            col_types = cols())
  }
  
  # Filter dataframe by specified country
  knots_best_country <- knots_best %>% 
    filter(Country == country) 
  
  # Mutate knot dates for specified scenario
  knots_best_country_modified <- knots_best_country %>%
    mutate(Date_first_restriction = Date_first_restriction - n_days_first_restriction,
           Date_lockdown = Date_lockdown - n_days_lockdown,
           Knot_date_1 = Knot_date_1 - n_days_first_restriction,
           Knot_date_2 = Knot_date_2 - n_days_lockdown)
  
  # Return dataframe
  return(knots_best_country_modified)
  
}
