#' Simulate a specified counterfactual history for a given country.
#' 
#' Two separate simulation types are permitted: 
#' (1) 'shift_intervention_sequence', which shifts the sequence of observed 
#' interventions (i.e. first restrictions and lockdown) by the specified number 
#' of days (i.e. \code{n_days_first_restriction} and \code{n_days_lockdown}, 
#' respectively); and 
#' (2) 'time_between_interventions', which shifts ONE of the observed 
#' interventions by a specified number of days (i.e. 
#' \code{n_days_first_restriction} or \code{n_days_lockdown}, respectively)
#' and shifts the other intervention to occur \code{days_between_interventions}
#' days before or after the specified intervention.
#' Unused arguments should be specified as missing.
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
#' Note that this function simulates the natural history when 
#' \code{simulation_type = shift_intervention_sequence} and 
#' \code{n_days_first_restriction = n_days_lockdown = 0}.
#'
#' @param country Country
#' @param simulation_type Type of simulation (one of 
#' c("shift_intervention_sequence", "time_between_interventions"))
#' @param n_days_first_restriction Number of days to bring forward first restriction
#' @param n_days_lockdown Number of days to bring forward lockdown
#' @param days_between_interventions Number of days between first restriction
#' and lockdown (if "min", then the gap between interventions is reduced to
#' the minimum possible)
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
#' simulation_type = "shift_intervention_sequence",
#' n_days_first_restriction = 0, n_days_lockdown = 3, 
#' max_t = 548, n_runs = 100000, prob_equal = FALSE)
#' Simulate_Counterfactual(country = "United Kingdom", 
#' simulation_type = "time_between_interventions",
#' n_days_first_restriction = 10, n_days_lockdown = rlang::missing_arg(),
#' days_between_interventions = "min", 
#' max_t = 548, n_runs = 100000, prob_equal = FALSE)
Simulate_Counterfactual <- function(country, simulation_type,
                                    n_days_first_restriction, n_days_lockdown,
                                    days_between_interventions = maybe_missing(missing_arg()),
                                    max_t, n_runs, prob_equal) {
  
  # Get important dates in designated country (dates of first restriction,
  # lockdown and simulation start date), save to environment, and
  # calculate simulation end date
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_first_restriction", 
                                         "Date_lockdown", "Date_start")) 
  list2env(important_dates, envir = environment())
  date_end <- as.Date(date_start + max_t)
  
  # Check that permissible values of simulation parameters are specified;
  # if not, print warning and stop
  error_message <- Check_Simulation_Parameters(simulation_type = simulation_type,
                                               n_days_first_restriction = n_days_first_restriction,
                                               n_days_lockdown = n_days_lockdown,
                                               days_between_interventions = days_between_interventions,
                                               date_lockdown = date_lockdown)
  if (!is.null(error_message)) { 
    error_message <- gsub(pattern = "country", replacement = country, 
                          x = error_message, ignore.case = TRUE)
    stop(cat(paste0("**", error_message, "\n"))) 
  }
  
  # If country didn't enter lockdown but simulation_type 'shift_intervention_sequence'
  # is specified, print warning that parameter n_days_lockdown will be ignored
  # and set value to NA
  if (simulation_type == "shift_intervention_sequence" && is.na(date_lockdown)) {
    warning(paste0("Lockdown was not implemented in ", country, 
                   ". Parameter n_days_lockdown will be ignored."))
    n_days_lockdown <- as.numeric(NA)
  }
  
  # Calculate counterfactual dates of first restriction and lockdown
  # (and missing n_days parameter, if time_between_interventions simulation is specified)
  if (simulation_type == "shift_intervention_sequence") {
    date_first_restriction_counterfactual <- date_first_restriction - 
      n_days_first_restriction
    date_lockdown_counterfactual <- date_lockdown - n_days_lockdown
  } else {  # (simulation_type = "time_between_interventions")
    # Set value of days_between_interventions if "min"
    # (0 if country entered lockdown immediately, 1 otherwise)
    if (days_between_interventions == "min") {
      days_between_interventions <- ifelse(date_first_restriction == date_lockdown,
                                           0, 1)
    }
    if (is_missing(maybe_missing(n_days_first_restriction))) {
      date_lockdown_counterfactual <- date_lockdown - n_days_lockdown
      date_first_restriction_counterfactual <- date_lockdown_counterfactual -
        days_between_interventions
      n_days_first_restriction <- as.numeric(abs(date_first_restriction - 
                                                   date_first_restriction_counterfactual))
    } else {  # (is_missing(n_days_lockdown))
      date_first_restriction_counterfactual <- date_first_restriction - 
        n_days_first_restriction
      date_lockdown_counterfactual <- date_first_restriction_counterfactual +
        days_between_interventions
      n_days_lockdown <- as.numeric(abs(date_lockdown - date_lockdown_counterfactual))
    }
  }
  
  # Check whether counterfactual can be simulated;
  # print warning and stop if it cannot
  counterfactual_possible <- Check_If_Counterfactual_Possible(country = country,
                                                              n_days_first_restriction = n_days_first_restriction,
                                                              n_days_lockdown = n_days_lockdown)
  if (counterfactual_possible == FALSE) {
    error_message <- paste0("The specified counterfactual cannot be estimated for ", country, ".")
    stop(cat(paste0("**", error_message, "\n")))
  }
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Create dataframe of (counterfactual) knots to be used in simulation
  knots_best_country_counterfactual <- Modify_Knot_Dates(country = country,
                                                         n_days_first_restriction = n_days_first_restriction,
                                                         n_days_lockdown = n_days_lockdown)
  
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
                                   casetypes = c("Daily_cases_MA7", 
                                                 "Cumulative_cases_end_MA7")) %>%
    setNames(., c("inc", "cum"))
  
  # Create empty list for storing simulated incidence data
  daily_cases_sim <- list()
  
  # (1) Iterate through possible knot date pairs
  for (i in 1:nrow(knots_best_country_counterfactual)) {
    
    # Refilter best knots dataframe by row i and convert to list
    knots_best_country_counterfactual_i <- knots_best_country_counterfactual %>% 
      slice(i) %>%
      as.list %>%
      setNames(., First_Character_To_Lower(names(.)))
    
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
  days_between_interventions <- ifelse(is_missing(maybe_missing(days_between_interventions)), 
                                       NA, days_between_interventions)
  summary_all_cases_sim <- summary_all_cases_sim %>%
    map(., .f = ~mutate(., Date = as.Date(Date), 
                        Country = country,
                        Simulation_type = simulation_type,
                        N_days_first_restriction = n_days_first_restriction,
                        N_days_lockdown = n_days_lockdown,
                        Days_between_interventions = days_between_interventions,
                        Date_first_restriction = date_first_restriction_counterfactual,
                        Date_lockdown = date_lockdown_counterfactual) %>% 
          relocate(Country, Simulation_type, N_days_first_restriction, N_days_lockdown, 
                   Days_between_interventions, Date_first_restriction, Date_lockdown))
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  close(progress_bar)
  
  # Return list of summary dataframes: simulated daily and cumulative cases
  return(list(summary_daily_cases_sim = summary_all_cases_sim$summary_daily_cases_sim, 
              summary_cumulative_cases_end_sim = summary_all_cases_sim$summary_cumulative_cases_end_sim))
  
}
