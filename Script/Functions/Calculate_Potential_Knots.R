#' Calculate all potential unique combinations of knot dates for a given country.
#'
#' @param country Country
#' @param date_first_restriction Date of first restriction
#' @param date_lockdown Date of lockdown
#' @param date_start Start date of modelling period
#' @param window Vector of 2 values representing the minimum and maximum number
#' of days after a restriction is implemented that its results might potentially
#' be realised
#'
#' @return Tibble with 2 columns ('Knot_date_1' and 'Knot_date_2') containing all
#' possible unique combinations of knot dates
#'
#' @examples
#' Calculate_Potential_Knots(country = "United Kingdom",
#' date_first_restriction = as.Date("2020-03-13"), date_lockdown = as.Date("2020-03-23"),
#' date_start = "2020-03-02", window = c(2, 28))
Calculate_Potential_Knots <- function(country, date_first_restriction, date_lockdown,
                                      date_start, window) {
  
  # 3 possible scenarios:
  # (1) Country did not implement lockdown, so there is only one knot date which
  # corresponds to the effect of the first restriction (second not date is NA); 
  # (2) Country implemented lockdown immediately, so there are two EQUAL knot dates which 
  # correspond to the effect of lockdown
  # (3) Country implemented both first restriction and lockdown on different days,
  # so there are two corresponding knot dates
  if (is.na(date_lockdown)) {
    
    # Define potential values of first knot date
    # (second knot date is NA, because country did not implement lockdown)
    possible_knot_dates_1 <- seq.Date(from = date_first_restriction + window[1], 
                                      to = date_first_restriction + window[2], by = 1)
    possible_knot_dates_2 <- as.Date(NA)
    
    # Create grid of potential knot date pairs
    grid <- expand_grid("Knot_date_1" = possible_knot_dates_1,
                        "Knot_date_2" = possible_knot_dates_2)
    
  } else if (date_first_restriction == date_lockdown) {
    
    # Define potential values of second knot date
    possible_knot_dates_2 <- seq(from = date_lockdown + window[1], 
                                 to = date_lockdown + window[2], by = 1)
    
    # Set first knot date equal to second knot date, because country implemented lockdown immediately
    # and skipped over period of growth under first restriction
    possible_knot_dates_1 <- possible_knot_dates_2
    
    # Create grid of potential knot date pairs
    grid <- tibble(Knot_date_1 = possible_knot_dates_1,
                   Knot_date_2 = possible_knot_dates_2)
    
  } else {
    
    # Define potential values of first and second knot dates
    possible_knot_dates_1 <- seq(from = date_first_restriction + window[1], 
                                 to = date_first_restriction + window[2], by = 1)
    possible_knot_dates_2 <- seq(from = date_lockdown + window[1], 
                                 to = date_lockdown + window[2], by = 1)
    
    # Create grid of potential knot date pairs
    grid <- expand_grid(Knot_date_1 = possible_knot_dates_1,
                        Knot_date_2 = possible_knot_dates_2)
    
  }
  
  # Remove knot date pairs for which second knot date is less than first knot date
  remove <- grid %>% filter(Knot_date_2 < Knot_date_1)
  grid <- anti_join(grid, remove, by = names(grid))
  
  # Replace all knot dates that are before or equal to date_start
  # with date_start and keep only unique pairs, because all knot dates that occur
  # at or before we start modelling cannot be distinguished from one another
  # (if both dates are before or equal to date_start, all models have 0 knots
  # which fall within the modelling period; 
  # if only first knot date is before or equal to date_start, all models have 1 knot
  # which falls within the modelling period)
  grid <- grid %>% mutate(across(.cols = everything(), 
                                 ~if_else(. <= date_start, date_start, .))) %>% 
    unique
  
  # Return grid of potential knot dates
  return(grid)
  
}
