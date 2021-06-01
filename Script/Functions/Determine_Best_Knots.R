#' Determine best pairs of knot dates and associated growth parameters for a given country.
#' 
#' #' This code requires the following files to run: 
#' (1) 'Cases_deaths_data_europe.csv', which contains observed values of cases 
#' and deaths (including 7-day moving averages) for all European countries.
#' (2) 'summary_eur.csv', which contains a number of important policy and case 
#' threshold dates for all European countries.
#'
#' @param country Country
#' @param criteria_selection Criteria by which to select best knot dates; one of
#' c("Pois_dev_inc", "Pois_dev_cum")
#' @param n_best Number of best knots to select 
#'
#' @return Named list containing 'knots_best' dataframe, which contains
#' a summary of the knot date pairs and associated growth parameters deemed
#' to be most likely.
#'
#' @examples
#' Determing_Best_Knots(country = "United Kingdom", 
#' criteria_selection = "Pois_dev_inc", n_best = 10)
Determine_Best_Knots <- function(country, 
                                 criteria_selection,
                                 n_best) {
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Get important dates in designated country 
  # (start and end dates, dates of first restriction and lockdown),
  # and save to environment
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_start", "Date_T",
                                         "Date_first_restriction", 
                                         "Date_lockdown")) %>%
    setNames(., tolower(names(.)))
  list2env(important_dates, envir = environment())
  
  # Calculate total number of knots for the given country:
  # If country implemented both first restriction and lockdown, there two knot points;
  # If country implemented only the first restriction, there is only one knot point.
  n_knots <- c(date_first_restriction, date_lockdown) %>% 
    as.logical %>% sum(na.rm = TRUE)
  
  # Calculate all potential combinations of knot date
  knot_dates <- Calculate_Potential_Knots(country = country,
                                          date_first_restriction = date_first_restriction,
                                          date_lockdown = date_lockdown,
                                          date_start = date_start,
                                          window = c(2, 28))
  
  # Print warning and stop if there are no potential knot dates
  if (nrow(knot_dates) == 0) {
    close(progress_bar)
    stop(paste("Knot estimation is not possible for", country, 
               ". Restrictions were imposed long before threshold for modelling was reached."))
  }
  
  # Create empty list to store summary statistics for all possible combinations of knot dates
  knot_summaries <- list()
  
  # (1) Iterate through pairs of candidate knot dates
  # Estimate growth parameters, and estimate growth using those parameters
  for (i in 1:nrow(knot_dates)) {
    
    # Refilter knots dataframe by row i and convert to list
    knot_dates_i <- knot_dates %>%
      slice(i) %>%
      as.list %>%
      setNames(., tolower(names(.)))
    
    # Specify spline model:
    # number of knots that fall within the modelling range, values of knot points,
    # whether to estimate an intercept, names of model covariates, and
    # split dataset
    spline_specification_i <- Specify_Spline_Model(country = country,
                                                   n_knots = n_knots,
                                                   date_min = date_start,
                                                   date_max = date_t, 
                                                   knot_date_1 = knot_dates_i$knot_date_1,
                                                   knot_date_2 = knot_dates_i$knot_date_2)
    
    # Estimate growth parameters using an Arima spline model 
    # and specified number of knots
    parameters_i <- Estimate_Growth_Parameters(n_knots = n_knots,
                                               n_knots_in_range = spline_specification_i$n_knots_in_range,
                                               knot_1 = spline_specification_i$knot_1, 
                                               knot_2 = spline_specification_i$knot_2,
                                               intercept = spline_specification_i$intercept,
                                               covariates = spline_specification_i$covariates,
                                               data = spline_specification_i$data_spline) %>%
      setNames(., tolower(names(.)))
    
    # Skip to next iteration if error occurred in parameter estimation
    if (parameters_i$error_occurred == TRUE) { next }
    
    # Record incident and cumulative cases (MA7) on date_start
    cases_start <- Get_Cases_On_Date(country = country, 
                                     date = date_start, 
                                     casetypes = c("Daily_cases_MA7", "Cumulative_cases_end_MA7")) %>%
      setNames(., c("inc", "cum"))
    
    # Estimate incident cases over modelling period 
    # (no variation - mean growth only)
    daily_cases_sim <- Simulate_Daily_Cases(date_start = date_start,
                                            date_end = date_t,
                                            start_value = cases_start$inc,
                                            n_runs = 1,
                                            n_knots = n_knots,
                                            knot_date_1 = knot_dates_i$knot_date_1,
                                            knot_date_2 = knot_dates_i$knot_date_2,
                                            parameters = parameters_i,
                                            variation = FALSE)
    
    # Calculate cumulative cases
    cumulative_cases_end_sim <- Calculate_Cumulative_Cases(daily_cases = daily_cases_sim,
                                                           start_value = cases_start$cum)
    
    # Calculate and record Poisson deviance
    ## (1) For predicted vs true (7-day moving average) incident cases
    true_inc <- Cases_deaths_data_europe %>% 
      filter(Country == country, Date >= date_start & Date <= date_t) %>%
      pull(Daily_cases_MA7)
    pred_inc <- daily_cases_sim[1, ]
    pois_dev_inc <- Calculate_Poisson_Deviance(obs = true_inc, pred = pred_inc)
    ## (2) For predicted vs true (7-day moving average) cumulative cases
    true_cum <- Cases_deaths_data_europe %>% 
      filter(Country == country, Date >= date_start & Date <= date_t) %>%
      pull(Cumulative_cases_end_MA7)
    pred_cum <- cumulative_cases_end_sim[1, ]
    pois_dev_cum <- Calculate_Poisson_Deviance(obs = true_cum, pred = pred_cum)
    
    # Create summary dataframe of given knot dates,
    # including estimated growth parameters, knot dates, number of knots
    # (total and in range), and Poisson deviances
    knot_summaries_i <- parameters_i %>% 
      bind_cols %>% 
      setNames(., str_to_title(names(.))) %>%
      mutate(Knot_date_1 = knot_dates_i$knot_date_1, 
             Knot_date_2 = knot_dates_i$knot_date_2, 
             N_knots = n_knots, 
             N_knots_in_range = spline_specification_i$n_knots_in_range,
             Pois_dev_inc = pois_dev_inc, 
             Pois_dev_cum = pois_dev_cum) %>%
      relocate(Knot_date_1, Knot_date_2, N_knots, N_knots_in_range)
    
    # Record summary in full list
    knot_summaries[[i]] <- knot_summaries_i
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i / (nrow(knot_dates) + 1))
    
  }  # (close loop 1 (i))
  
  # Bind summaries of all possible knot dates together
  knot_summaries <- knot_summaries %>% bind_rows
  
  # Remove from consideration knot date pairs for which ...
  # (1) growth_factor_1 is less than 1 (because this represents initial uncontrolled growth)
  # (2) any of growth factors are negative
  # (3) growth factor 1 is less than 2, or 2 is less than 3
  # (4) any of growth factor SDs are NaN
  remove_1 <- knot_summaries %>% filter(Growth_factor_1 < 1) 
  remove_2 <- knot_summaries %>% filter(Growth_factor_1 < 0 | Growth_factor_2 < 0 | Growth_factor_3 < 0)
  remove_3 <- knot_summaries %>% filter(Growth_factor_1 < Growth_factor_2 | Growth_factor_2 < Growth_factor_3)
  remove_4 <- knot_summaries %>% filter(is.nan(Growth_factor_1_sd) | is.nan(Growth_factor_2_sd) | is.nan(Growth_factor_3_sd))
  knot_summaries <- anti_join(knot_summaries, remove_1, by = names(knot_summaries)) %>% 
    anti_join(., remove_2, by = names(knot_summaries)) %>%
    anti_join(., remove_3, by = names(knot_summaries)) %>%
    anti_join(., remove_4, by = names(knot_summaries)) 
  
  # Find best knot points (by lowest value of specified criteria),
  # and label with country and dates of first restriction and lockdown
  knots_best <- knot_summaries %>% 
    arrange(eval(parse(text = criteria_selection))) %>% 
    head(n_best) %>% 
    select(-Error_occurred) %>%
    mutate(Country = country, 
           Date_first_restriction = important_dates$date_first_restriction,
           Date_lockdown = important_dates$date_lockdown) %>% 
    relocate(Country, Date_first_restriction, Date_lockdown) %>%
    arrange(Knot_date_1, Knot_date_2)
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  close(progress_bar)
  
  # Return list containing dataframe of best knots
  return(list(knots_best = knots_best))
  
}
