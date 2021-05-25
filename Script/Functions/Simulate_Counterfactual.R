#' Simulate a specified counterfactual history for a given country.
#' 
#' This code requires the following files to run: 
#' (1) 'summary_eur.csv', which contains a number of important policy and case 
#' threshold dates for all European countries.
#' (2) 'knots_best.csv', which contains best knot date pairs associated with the 
#' observed dates of first restriction and lockdown for all European countries.
#' (3) 'possible_days_counterfactual.csv', which contains all possible 
#' combinations of counterfactual shifts for all European countries.
#' (4) 'Cases_deaths_data_europe.csv', which contains observed values of cases 
#' and deaths (including 7-day moving averages) for all European countries.
#' 
#' Note that this function also simulates the natural history when 
#' \code{n_days_first_restriction = n_days_lockdown = 0}.
#'
#' @param country Country
#' @param n_days_first_restriction Number of days to bring forward first restriction
#' @param n_days_lockdown Number of days to bring forward lockdown
#' @param max_t Maximum number of days to simulate
#' @param n_runs Number of simulation runs
#' @param prob_equal Whether knot dates should be used with equal probabilities (T/F)
#'
#' @return List of 2 summary dataframes for designated \code{country}:
#' (1) 'summary_daily_cases_sim', which contains mean and 95% centile values
#' (across \code{n_runs}) of daily cases; and
#' (2) 'summary_cumulative_cases_end_sim', which contains mean and 95% centile
#' values (cross \code{n_runs}) of cumulative cases.
#'
#' @examples
#' Simulate_Counterfactual(country = "United Kingdom", 
#' n_days_first_restriction = 0, n_days_lockdown = 3, max_t = 548,
#' n_runs = 100000, prob_equal = FALSE)
Simulate_Counterfactual <- function(country, n_days_first_restriction, n_days_lockdown,
                                    max_t, n_runs, prob_equal) {
  
  # Print warning and stop if n_days_first_restriction or n_days_lockdown are less than zero
  if (n_days_first_restriction < 0 | n_days_lockdown < 0) {
    stop("The following arguments must be >= 0: n_days_first_restriction, n_days_lockdown.")
  }
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Get important dates in designated country (date of lockdown and simulation start date),
  # and save to environment
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_lockdown", "Date_start")) %>%
    setNames(., tolower(names(.)))
  list2env(important_dates, envir = environment())
  
  # Calculate simulation end date
  date_end <- as.Date(date_start + max_t)
  
  # Print message that n_days_lockdown will be ignored if country didn't enter lockdown,
  # and set value of n_days_lockdown to NA
  if (is.na(date_lockdown)) {
    warning(paste0("Lockdown was not implemented in ", country, 
                   ". Parameter n_days_lockdown will be ignored."))
    n_days_lockdown <- as.numeric(NA)
  } 
  
  # Check whether counterfactual can be simulated;
  # print warning and stop if it cannot
  counterfactual_possible <- Check_If_Counterfactual_Possible(country = country,
                                                              n_days_first_restriction = n_days_first_restriction,
                                                              n_days_lockdown = n_days_lockdown)
  if (counterfactual_possible == FALSE) {
    stop(paste0("The specified counterfactual cannot be estimated for ", country, "."))
  }
  
  # Create dataframe of (counterfactual) knots to be used in simulation
  knots_best_country_counterfactual <- Modify_Knot_Dates(country = country,
                                                         n_days_first_restriction = n_days_first_restriction,
                                                         n_days_lockdown = n_days_lockdown)
  
  # Record total number of knots (i.e. number of interventions),
  # and counterfactual dates of first restriction and lockdown
  #n_knots <- knots_best_country_counterfactual %>% pull(N_knots) %>% unique
  date_first_restriction_counterfactual <- knots_best_country_counterfactual %>% 
    pull(Date_first_restriction) %>% unique
  date_lockdown_counterfactual <- knots_best_country_counterfactual %>% 
    pull(Date_lockdown) %>% unique
  
  # Calculate number of simulation runs (N) for each pair of knot dates, and 
  # modify counterfactual knots dataframe
  if (prob_equal == TRUE) {
    prob_knots <- knots_best_country_counterfactual %>% pull(Prob_equal)
  } else {
    prob_knots <- knots_best_country_counterfactual %>% pull(Prob_unequal)
  }
  knots_best_country_counterfactual <- knots_best_country_counterfactual %>% 
    mutate(N = GFE::round_preserve_sum(prob_knots * n_runs))
  
  # Record incident and cumulative cases (MA7) on date_start
  cases_start <- Get_Cases_On_Date(country = country,
                                   date = date_start,
                                   casetypes = c("Daily_cases_MA7", "Cumulative_cases_end_MA7")) %>%
    setNames(., c("inc", "cum"))

  # Create empty list for storing simulated incidence data
  daily_cases_sim <- list()
  
  # (1) Iterate through possible knot date pairs
  for (i in 1:nrow(knots_best_country_counterfactual)) {
    
    # Refilter best knots dataframe by row i and convert to list
    knots_best_country_counterfactual_i <- knots_best_country_counterfactual %>% 
      filter(row_number() == i) %>%
      as.list %>%
      setNames(., tolower(names(.)))
    
    # Save growth parameters (means and SDs) in separate list
    parameters_i <- names(knots_best_country_counterfactual_i) %>%
      str_detect("growth") %>%
      keep(knots_best_country_counterfactual_i, .)
    
    # Estimate incident cases over modelling period
    daily_cases_sim_i <- Simulate_Daily_Cases(date_start = date_start, 
                                              date_end = date_end, 
                                              start_value = cases_start$inc,
                                              n_runs = knots_best_country_counterfactual_i$n,
                                              n_knots = knots_best_country_counterfactual_i$n_knots,
                                              knot_date_1 = knots_best_country_counterfactual_i$knot_date_1, 
                                              knot_date_2 = knots_best_country_counterfactual_i$knot_date_2,
                                              parameters = parameters_i)
    
    # Record simulated incidence data in list
    daily_cases_sim[[i]] <- daily_cases_sim_i
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i / (nrow(knots_best_country_counterfactual) + 1))
    
  }  # (close loop 1 (i))
  
  # Bind all simulated incidence data together
  daily_cases_sim <- do.call(rbind, daily_cases_sim)
  
  # Calculate cumulative cases
  cumulative_cases_end_sim <- Calculate_Cumulative_Cases(daily_cases = daily_cases_sim,
                                                         start_value = cases_start$cum)
  
  # Combine simulated incidence and cumulative into list
  all_cases_sim <- list(daily_cases_sim = daily_cases_sim, 
                        cumulative_cases_end_sim = cumulative_cases_end_sim)
  
  # Calculate mean and 95% centile values across all simulation runs
  summary_all_cases_sim <- all_cases_sim %>%
    map(., .f = ~split(., col(.))) %>%
    map2(., .y = all_cases_sim, .f = ~setNames(.x, colnames(.y))) %>%
    map_depth(., .depth = 2, .f = ~Summarise_Centiles(.)) %>%
    map_depth(., .depth = 2, .f = ~as_tibble(.)) %>%
    map_depth(., .depth = 1, .f = ~bind_rows(., .id = "Date")) %>%
    setNames(., paste0("summary_", names(.)))
    
  # Label summaries with country name and simulation parameters
  summary_all_cases_sim <- summary_all_cases_sim %>%
    map(., .f = ~mutate(., Date = as.Date(Date), 
                        Country = country,
                        N_days_first_restriction = n_days_first_restriction,
                        N_days_lockdown = n_days_lockdown,
                        Date_first_restriction = date_first_restriction_counterfactual,
                        Date_lockdown = date_lockdown_counterfactual) %>% 
          relocate(Country, N_days_first_restriction, N_days_lockdown, 
                   Date_first_restriction, Date_lockdown))
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  close(progress_bar)
  
  # Return list of summary dataframes: simulated daily and cumulative cases
  return(list(summary_daily_cases_sim = summary_all_cases_sim$summary_daily_cases_sim, 
              summary_cumulative_cases_end_sim = summary_all_cases_sim$summary_cumulative_cases_end_sim))
  
}
