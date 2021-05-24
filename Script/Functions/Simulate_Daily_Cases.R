#' Simulate the growth of daily cases over a designated period.
#' 
#' Each simulation consists of a maximum of 3 periods of growth, which are
#' separated by \code{knot_date_1} and \code{knot_date_2}. The simulation begins 
#' on \code{date_start} with \code{start_value} daily cases. On each day
#' subsequently (up to \code{date_end}), a random growth factor is applied to the
#' previous value of daily cases to estimate the current value of daily cases.
#' Growth factors are drawn from log-normal distributions with user-specified
#' parameters, according to the period of growth in which each day falls.
#'
#' @param date_start Start date of simulation
#' @param date_end End date of simulation
#' @param start_value Value of daily cases with which to start simulation
#' @param n_runs Number of simulation runs
#' @param n_knots Total number of knot dates (i.e. number of interventions implemented)
#' @param knot_date_1 Date of first knot
#' @param knot_date_2 Date of second knot
#' @param parameters List containing growth parameters
#' (i.e. "growth_factor_1", "growth_factor_1_sd", "growth_factor_2", "growth_factor_2_sd",
#' growth_factor_3", "growth_factor_3_sd"). Note that parameters may be NA.
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
#' growth_factor_3 = 0.98, growth_factor_3_sd = 0.005))
Simulate_Daily_Cases <- function(date_start, date_end, 
                                 start_value,
                                 n_runs,
                                 n_knots = c(1, 2),
                                 knot_date_1, knot_date_2,
                                 parameters) {
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_start + 1, to = date_end, by = 1)
  
  # Create matrices for simulated incidence data for given knot dates
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- 
    matrix(nrow = n_runs, ncol = length(dates) + 1,
           dimnames = list(NULL, as.character(seq.Date(from = date_start, to = date_end, by = 1))))
  
  # Initialise matrix with data at date_start
  daily_cases_sim[, 1] <- start_value
  
  # Calculate parameter distributions on log scale
  # (growth periods 1, 2, and 3)
  log_parameters_1 <- Calculate_Parameters_Log(mean = parameters$growth_factor_1,
                                               sd = parameters$growth_factor_1_sd) 
  log_parameters_2 <- Calculate_Parameters_Log(mean = parameters$growth_factor_2,
                                               sd = parameters$growth_factor_2_sd) 
  log_parameters_3 <- Calculate_Parameters_Log(mean = parameters$growth_factor_3,
                                               sd = parameters$growth_factor_3_sd) 
  
  # Iterate through dates
  for (t in as.list(dates)) {
    
    # Get daily cases at time t-1
    inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
    
    # Define growth parameters
    if (n_knots == 1) {  # one knot (i.e. first restriction only); 2 periods of growth
      
      # Define growth factor
      if (t <= knot_date_1) {
        growth <- rlnorm(n = n_runs,
                         meanlog = log_parameters_1$mean,
                         sdlog = log_parameters_1$sd)
      } else {
        growth <- rlnorm(n = n_runs,
                         meanlog = log_parameters_2$mean,
                         sdlog = log_parameters_2$sd)
      }
      
    } else {  # two knots (i.e. both first restriction and lockdown); 3 periods of growth
      
      # Define growth factor
      if (t <= knot_date_1) {
        growth <- rlnorm(n = n_runs,
                         meanlog = log_parameters_1$mean,
                         sdlog = log_parameters_1$sd)
      } else if (t <= knot_date_2) {
        growth <- rlnorm(n = n_runs,
                         meanlog = log_parameters_2$mean,
                         sdlog = log_parameters_2$sd)
      } else {
        growth <- rlnorm(n = n_runs,
                         meanlog = log_parameters_3$mean,
                         sdlog = log_parameters_3$sd)
      }
      
    }
    
    # Calculate daily cases at time t and record
    inc_t <- growth*inc_tminus1
    daily_cases_sim[, as.character(t)] <- inc_t
    
  }  
  
  # Return list of incident cases
  return(daily_cases_sim)
  
}
