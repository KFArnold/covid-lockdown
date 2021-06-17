#' Simulate the growth of daily cases over a designated period.
#' 
#' Each simulation consists of a maximum of 3 periods of growth, which are
#' separated by \code{knot_date_1} and \code{knot_date_2}. The simulation begins 
#' on \code{date_start} with \code{start_value} daily cases. On each day
#' subsequently (up to \code{date_end}), a random growth factor is applied to the
#' previous value of daily cases to estimate the current value of daily cases.
#' Growth factors are drawn from log-normal distributions by default with 
#' user-specified parameters (on the normal scale), according to the period of 
#' growth in which each day falls.
#'
#' @param date_start Start date of simulation
#' @param date_end End date of simulation
#' @param start_value Value of daily cases with which to start simulation
#' @param n_runs Number of simulation runs
#' @param n_knots Total number of knot dates (i.e. number of interventions implemented)
#' @param knot_date_1 Date of first knot
#' @param knot_date_2 Date of second knot
#' @param parameters List containing growth parameters on normal scale
#' (i.e. "growth_factor_1", "growth_factor_1_sd", "growth_factor_2", "growth_factor_2_sd",
#' growth_factor_3", "growth_factor_3_sd"). Note that parameters may be NA.
#' @param variation Whether to simulate variation in the growth factor (T/F);
#' default is TRUE; if FALSE, all growth factors are equal to the mean of their
#' respective distributions
#' @param log Whether to draw growth factors from a lognormal distribution (T/F);
#' default is TRUE; if FALSE, normal distribution is used
#'
#' @return Matrix of simulated incident cases, with 1 row per simulation run
#' and 1 column per date (i.e. \code{n_runs} rows and \code{date_end - date_start} columns)
#'
#' @examples
#' Simulate_Growth(date_start = as.Date("2020-03-02"),
#' date_end = as.Date("2021-09-01"),
#' start_value = 35,
#' n_runs = 100,
#' n_knots = 2,
#' knot_date_1 = as.Date("2020-03-20"),
#' knot_date_2 = as.Date("2020-04-07"),
#' parameters = list(growth_factor_1 = 1.25, growth_factor_1_sd = 0.96,
#' growth_factor_2 = 1.06, growth_factor_2_sd = 0.01,
#' growth_factor_3 = 0.98, growth_factor_3_sd = 0.005),
#' variation = TRUE,
#' log = TRUE)
Simulate_Daily_Cases <- function(date_start, date_end, 
                                 start_value,
                                 n_runs,
                                 n_knots,
                                 knot_date_1, knot_date_2,
                                 parameters,
                                 variation = TRUE,
                                 log = TRUE) {
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_start + 1, to = date_end, by = 1)
  
  # Create matrices for simulated incidence data for given knot dates
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- 
    matrix(nrow = n_runs, ncol = length(dates) + 1,
           dimnames = list(NULL, as.character(seq.Date(from = date_start, to = date_end, by = 1))))
  
  # Initialise matrix with data at date_start
  daily_cases_sim[, 1] <- start_value
  
  # Iterate through dates
  for (t in as.list(dates)) {
    
    # Get daily cases at time t-1
    inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
    
    # Define growth parameters
    if (n_knots == 1) {  # one knot (i.e. first restriction only); 2 periods of growth
      
      # Define growth factor
      if (t <= knot_date_1) {
        growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                             mean = parameters$growth_factor_1,
                                             sd = parameters$growth_factor_1_sd, 
                                             variation = variation,
                                             log = log)
      } else {
        growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                             mean = parameters$growth_factor_2,
                                             sd = parameters$growth_factor_2_sd, 
                                             variation = variation,
                                             log = log)
      }
      
    } else {  # two knots (i.e. both first restriction and lockdown); 2/3 periods of growth
      
      # If the first knot date occurs before second knot date, there are
      # 3 distinct periods of growth (i.e. initial uncontrolled growth, 
      # growth under first restrictions, and growth under lockdown measures).
      # Else, if the second knot date occurs before first knot date, there are
      # only 2 distinct periods of growth (i.e. initial uncontrolled growth and
      # growth under lockdown measures), since the effects of the first 
      # restrictions haven't been realised by the time the effects of the lockdown are.
      if (knot_date_1 <= knot_date_2) {  # (3 periods of growth)
        
        # Define growth factor
        if (t <= knot_date_1) {
          growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                               mean = parameters$growth_factor_1,
                                               sd = parameters$growth_factor_1_sd, 
                                               variation = variation,
                                               log = log)
        } else if (t <= knot_date_2) {
          growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                               mean = parameters$growth_factor_2,
                                               sd = parameters$growth_factor_2_sd, 
                                               variation = variation,
                                               log = log)
        } else {
          growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                               mean = parameters$growth_factor_3,
                                               sd = parameters$growth_factor_3_sd, 
                                               variation = variation,
                                               log = log)
        }
        
      } else {  # (2 periods of growth)
        
        # Define growth factor
        if (t <= knot_date_2) {
          growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                               mean = parameters$growth_factor_1,
                                               sd = parameters$growth_factor_1_sd, 
                                               variation = variation,
                                               log = log)
        } else {
          growth <- Draw_Random_Growth_Factors(n = n_runs, 
                                               mean = parameters$growth_factor_3,
                                               sd = parameters$growth_factor_3_sd, 
                                               variation = variation,
                                               log = log)
        }
        
      }
      
    }
    
    # Calculate daily cases at time t and record
    inc_t <- growth*inc_tminus1
    daily_cases_sim[, as.character(t)] <- inc_t
    
  }  
  
  # Return list of incident cases
  return(daily_cases_sim)
  
}
