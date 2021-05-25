#' Specify spline model for a given country
#'
#' @param country Country
#' @param n_knots Number of knots for specified country (equal to number of unique interventions)
#' @param date_min Beginning of date range to include splibe model
#' @param date_max End of date range to include in spline model
#' @param knot_date_1 Date of first knot
#' @param knot_date_2 Date of second knot
#'
#' @return Named list containing spline model specification:
#' (1) 'n_knots_in_range' = number of \code{n_knots} between \code{date_min} and \code{date_max};
#' (2) 'knot_1' = value of cumulative cases on \code{knot_date_1};
#' (3) 'knot_2' = value of cumulative cases on \code{knot_date_2};
#' (4) 'intercept' = whether model should include intercept term (T/F);
#' (5) 'covariates' = vector of all covariate names in spline model; and
#' (6) 'data_spline' = data to feed into spline model.
#'
#' @examples
#' Specify_Spline_Model(country = "United Kingdom", n_knots = 2,
#' date_min = as.Date("2020-03-02"), date_max = as.Date("2020-06-08"),
#' knot_date_1 = as.Date("2020-03-15"), knot_date_2 = as.Date("2020-03-25"))
Specify_Spline_Model <- function(country, n_knots, date_min, date_max,
                                 knot_date_1, knot_date_2) {
  
  # Load full formatted dataframe containing observed cases (if not already loaded)
  if(!exists("Cases_deaths_data_europe")) {
    Cases_deaths_data_europe <- read_csv("./Data/Formatted/Cases_deaths_data_europe.csv",
                                         col_types = cols())
  }
  
  # Create copy of cases/deaths dataframe for specified country where 
  # cumulative cases >= starting threshold and up to date_T
  data_in_range <- Cases_deaths_data_europe %>% 
    filter(Country == country,
           Date >= date_min & Date <= date_max)
  
  # (1) If the country has one knot (i.e. two periods of growth), 
  # either none or one of them may fall within range 
  # (i.e. one or two periods of growth observed).
  # (2) If the country has two knots (i.e. three periods of growth)
  # either none, one, or two of them may fall within range
  # (i.e. one, two, or three periods of growth observed).
  if (n_knots == 1) {  
    
    if (knot_date_1 <= date_min) {
      
      # If first knot date is before/at the time we begin modelling,
      # there are NO knot points within modelling range
      
      # Set number of knots in modelling range to zero (i.e. one segment)
      n_knots_in_range <- 0
      
      # Set knot points
      knot_1 <- NA
      knot_2 <- NA
      
      # Create dataframe for fitting manual splines
      # (second spline segment is observed, i.e. growth following first restriction)
      data_spline <- data_in_range %>% select(Daily_cases, Cumulative_cases_beg) %>%
        rename("Cumulative_cases_beg_2" = Cumulative_cases_beg)
      covariates <- data_in_range %>% select(contains("Cumulative")) %>% names
      
      # Specify that spline model should estimate an intercept
      # (because spline segment in range is not the first)
      intercept <- TRUE
      
    } else {
      
      # If first knot date is after the time we begin modelling,
      # there is ONE knot point within modelling range (i.e. at knot_date_1)
      
      # Set number of knots in modelling range to one (i.e. two segments)
      n_knots_in_range <- 1
      
      # Set knot points
      knot_1 <- Get_Cases_On_Date(country = country, 
                                  date = knot_date_1, 
                                  casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
      knot_2 <- NA
      
      # Create dataframe for fitting manual splines
      # (first and second segments are observed)
      data_spline <- data.frame(lspline(data_in_range$Cumulative_cases_beg, knots = c(knot_1)))
      names(data_spline) <- covariates <- paste0("Cumulative_cases_beg_", 1:2)
      data_spline <- bind_cols(Daily_cases = data_in_range$Daily_cases, data_spline)
      
      # Specify that spline model should not estimate an intercept
      # (because first spline segment in range is first so must go through zero)
      intercept <- FALSE
      
    }
    
  } else {  # (n_knots = 2)
    
    if (knot_date_2 <= date_min) {
      
      # If the second knot date is before/at the time we begin modelling,
      # there are NO knot points within modelling range
      
      # Set number of knots in modelling range to zero (i.e. one segment)
      n_knots_in_range <- 0
      
      # Set knot points
      knot_1 <- NA
      knot_2 <- NA
      
      # Create dataframe for fitting manual splines
      # (third spline segment is observed, i.e. growth under lockdown)
      data_spline <- data_in_range %>% select(Daily_cases, Cumulative_cases_beg) %>%
        rename("Cumulative_cases_beg_3" = Cumulative_cases_beg)
      covariates <- data_spline %>% select(contains("Cumulative")) %>% names
      
      # Specify that spline model should estimate an intercept
      # (because spline segment in range is not the first)
      intercept <- TRUE
      
    } else if (knot_date_1 <= date_min) {
      
      # If only the first knot date is before/at the time we begin modelling,
      # there is ONE knot point within modelling range (i.e. at knot_date_2)
      
      # Set number of knots in modelling range to one (i.e. two segments)
      n_knots_in_range <- 1
      
      # Set knot points
      knot_1 <- NA
      knot_2 <- Get_Cases_On_Date(country = country,
                                  date = knot_date_2,
                                  casetypes = "Cumulative_cases_beg") %>% unlist %>% unname

      # Create dataframe for fitting manual splines
      # (second and third spline segments are observed)
      data_spline <- data.frame(lspline(data_in_range$Cumulative_cases_beg, knots = c(knot_2)))
      names(data_spline) <- covariates <- paste0("Cumulative_cases_beg_", 2:3)
      data_spline <- bind_cols(Daily_cases = data_in_range$Daily_cases, data_spline)
      
      # Specify that spline model should estimate an intercept
      # (because spline segments in range are not the first)
      intercept <- TRUE
      
    } else {
      
      # BOTH knots (i.e. knot_date_1 and knot_date_2) fall within modelling range 
      
      # Set number of knots in modelling range to two 
      n_knots_in_range <- 2
      
      # If knot dates are equal, there is only one DISTINCT knot point within modelling range
      if (knot_date_1 == knot_date_2) {
        
        # Set knot points
        # (knot_1 is NA because it is not unique value, i.e. it is same as knot_2)
        knot_1 <- NA
        knot_2 <- Get_Cases_On_Date(country = country,
                                    date = knot_date_2,
                                    casetypes = "Cumulative_cases_beg") %>% unlist %>% unname

        # Create dataframe for fitting manual splines
        # (first and third spline segments observed)
        data_spline <- data.frame(lspline(data_in_range$Cumulative_cases_beg, knots = c(knot_2)))
        names(data_spline) <- covariates <- paste0("Cumulative_cases_beg_", c(1, 3))
        data_spline <- bind_cols(Daily_cases = data_in_range$Daily_cases, data_spline)
        
      } else {
        
        # Set knot points
        knot_1 <- Get_Cases_On_Date(country = country,
                                    date = knot_date_1,
                                    casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
        knot_2 <- Get_Cases_On_Date(country = country,
                                    date = knot_date_2,
                                    casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
        
        # Create dataframe for fitting manual splines
        # (all three spline segments observed)
        data_spline <- data.frame(lspline(data_in_range$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
        names(data_spline) <- covariates <- paste0("Cumulative_cases_beg_", 1:3)
        data_spline <- bind_cols(Daily_cases = data_in_range$Daily_cases, data_spline)
        
      }
      
      # Specify that spline model should not estimate an intercept
      # (because first spline segment in range is first so must go through zero)
      intercept <- FALSE
      
    }
    
  }
  
  # Return list of spline model specification
  return(list(n_knots_in_range = n_knots_in_range,
              knot_1 = knot_1,
              knot_2 = knot_2,
              intercept = intercept,
              covariates = covariates,
              data_spline = data_spline))
  
}
