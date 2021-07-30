#' Calculate median simulation parameters and summary statistics (median, Q1, Q3).
#' 
#' The parameters of interest are growth factors 1-3, and the lag periods between
#' each intervention and its associted knot dates. Data is obtained from the
#' 'knots_best.csv' file. The weighted median of each parameter is computed 
#' within each country, and then overall summary statistics (i.e. across all 
#' countries) are computed.
#' 
#' @param countries List of countries
#' @param n_decimals Number of decimals to include in output (default = 3)
#' @param out_folder Folder to save parameter summary tables in
#'
#' @return Two summary dataframes:
#' (1) 'median_simulation_parameters': contains data pertaining to the weighted 
#' median of each simulation parameter within each country. Table contains columns 
#' for Country, Median_growth_factor_1, Median_growth_factor_2,  
#' Median_growth_factor_3, Lag_1, and Lag_2.
#' (2) 'simulation_parameter_summary': contains data pertaining to the median of 
#' each simulation parameter across all \code{countries}. Table contains columns
#' for Parameter, Median (i.e. median value of weighted Parameter across all 
#' countries), and N (i.e. number of non-NA values included in calculation of Median).
#' 
#' Both summary dataframes are saved as .csv files to the designated output folder.
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
  
  # Calculate weighted median lags and growth factors within each country
  summary_within_countries <- knots_best_filt %>%
    select(Country, Lag_1, Lag_2, Growth_factor_1, Growth_factor_2, 
           Growth_factor_3, Prob_unequal) %>%
    mutate(across(.cols = -Country, as.double)) %>%
    pivot_longer(cols = -c(Country, Prob_unequal), 
                 names_to = "Parameter", values_to = "Value") %>%
    group_by(Country, Parameter) %>%
    summarise(Median = matrixStats::weightedMedian(x = Value, 
                                      w = Prob_unequal,
                                      na.rm = TRUE),
              N = sum(!is.na(Value)),
              .groups = "drop")
  
  # Summarise median, Q1, and Q3 of lags and growth factors across all countries,
  # with designated number of decimals
  simulation_parameter_summary <- summary_within_countries %>%
    select(-Country) %>%
    group_by(Parameter) %>%
    summarise(Med = median(Median, na.rm = TRUE),
              Q1 = quantile(Median, 0.25, na.rm = TRUE),
              Q3 = quantile(Median, 0.75, na.rm = TRUE),
              N = sum(N),
              .groups = "drop") %>%
    rename(Median = Med) %>%
    mutate(across(where(is.double), ~round(., digits = n_decimals)))
  
  # Create (wide) table of median weighted simulation parameters for each country
  # with designated number of decimals
  median_simulation_parameters <- summary_within_countries %>%
    select(Country, Parameter, Median) %>%
    pivot_wider(values_from = Median, names_from = Parameter) %>%
    rename_with(.cols = -Country, .fn = ~paste0("Median_", .)) %>%
    rename_with(.cols = everything(), .fn = ~str_to_sentence(.)) %>%
    mutate(across(where(is.double), ~round(., digits = n_decimals)))
  
  # Save summary tables to specified folder
  write_csv(median_simulation_parameters, 
            paste0(out_folder, "median_simulation_parameters.csv"))
  write_csv(simulation_parameter_summary, 
            paste0(out_folder, "simulation_parameter_summary.csv"))
  
  # Return list of summary tables
  return(list(median_simulation_parameters = median_simulation_parameters,
              simulation_parameter_summary = simulation_parameter_summary))
  
}
