#' Calculate summary statistics  (median, Q1, Q3) for simulation parameters.
#' 
#' The parameters of interest are growth factors 1-3, and the lag periods between
#' each intervention and its associted knot dates. Data is obtained from the
#' 'knots_best.csv' file.
#'
#' @param countries List of countries
#' @param n_decimals Number of decimans to include in output (default = 3)
#' @param out_folder Folder to save parameter summary table in
#'
#' @return Dataframe containing 5 columns: 
#' (1) Parameter = description of parameter;
#' (2) Median = median value of parameter across all \code{countries};
#' (3) Q1 = value of first quartile;
#' (4) Q3 = value of third quartile; and
#' (5) N = number of (non-NA) values that were included in calculation.
#'
#' @examples
#' Calculate_Simulation_Parameter_Summary(countries = c("United Kingdom", 
#' "Germany"), out_folder = "./Output/")
Calculate_Simulation_Parameter_Summary <- function(countries,
                                                   n_decimals = 3,
                                                   out_folder) {
  
  # Import file containing best knots 
  Import_Unloaded_CSV_Files(filenames = "knots_best",
                            silent = TRUE)
  
  # Filter best knots dataframe by designated countries
  knots_best_filt <- knots_best %>%
    filter(Country %in% countries)
  
  # Calculate lags between intervention and knot dates
  knots_best_filt <- knots_best_filt %>%
    mutate(Lag_1 = Knot_date_1 - Date_first_restriction,
           Lag_2 = Knot_date_2 - Date_lockdown) 
  
  # Summarise median, Q1, and Q3 of lags and growth factors,
  # with designated number of decimals
  summary <- knots_best_filt %>%
    select(Lag_1, Lag_2, Growth_factor_1, Growth_factor_2, Growth_factor_3) %>%
    mutate(across(.cols = everything(), as.double)) %>%
    pivot_longer(cols = everything(), names_to = "Parameter", values_to = "Value") %>%
    group_by(Parameter) %>%
    summarise(Median = median(Value, na.rm = TRUE),
              Q1 = quantile(Value, 0.25, na.rm = TRUE),
              Q3 = quantile(Value, 0.75, na.rm = TRUE),
              N = sum(!is.na(Value)),
              .groups = "drop") %>%
    mutate(across(where(is.double), ~round(., digits = n_decimals)))
  
  # Save summary table to specified folder
  write_csv(summary, paste0(out_folder, "simulation_parameter_summary.csv"))
  
  # Return summary table
  return(summary)
  
}
