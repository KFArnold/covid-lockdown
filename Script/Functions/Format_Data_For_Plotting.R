#' Create formatted versions of datafiles for creating figures.
#'
#' @param filenames Names of files to format
#' @param silent Whether to return message if formatted files already exist
#' in the global environment (T/F, default is FALSE)
#'
#' @return Named list of formatted files (\code{filenames} appended with "_formatted")
#'
#' @examples
#' Format_Data_For_Plottint(silent = TRUE)
Format_Data_For_Plotting <- function(filenames = c("summary_eur",
                                                   "thresholds_eur",
                                                   "summary_daily_cases_sim_all",
                                                   "summary_cumulative_cases_end_sim_all",
                                                   "model_fit",
                                                   "model_fit_summary",
                                                   "effects_within_countries",
                                                   "effects_within_countries_summary",
                                                   "effects_between_countries"),
                                     silent = FALSE) {
  
  # Determine which of specified filenames do not have formatted versions in
  # the global environment
  filenames_formatted_missing <- setdiff(paste0(filenames, "_formatted"), ls(.GlobalEnv)) 
  
  # If all formatted files already exist, print message and exit
  if (length(filenames_formatted_missing) == 0) {
    message <- ifelse(silent == FALSE, "Formatted files already exist.", "")
    return(message)
  } 
  
  # Determine which of the specified filenames (unformatted) are not in 
  # the global environment
  filenames_missing <- setdiff(filenames, ls(.GlobalEnv))
  
  # If there are any missing unformatted files, import these into the global environment
  if (length(filenames_missing) != 0) {
    
    # Define which of input filenames are files containing simulated data,
    # and which aren't
    sim_files <- filenames[grepl("sim", filenames)]
    other_files <- setdiff(filenames, sim_files)
    
    # Import missing files
    Import_Unloaded_CSV_Files(filenames = other_files, 
                              silent = TRUE)
    Import_All_Simulated_Data(silent = TRUE)
    
  }
  
  # Create empty list for storing formatted dataframes
  list_formatted <- list()
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")
  
  # Format all specified filenames which are not already formatted (order factors),
  # and add these to list of formatted files
  if ("summary_eur_formatted" %in% filenames_formatted_missing) {
    summary_eur_formatted <- summary_eur %>%
      select(Country, contains("Date")) %>%
      pivot_longer(contains("Date"), names_to = "Date", values_to = "Value") %>%
      mutate(Date = factor(Date, levels = date_levels))
    list_formatted[["summary_eur_formatted"]] <- summary_eur_formatted
  }
  if ("thresholds_eur_formatted" %in% filenames_formatted_missing) {
    thresholds_eur_formatted <- thresholds_eur %>%
      mutate(Threshold = factor(Threshold, threshold_levels))
    list_formatted[["thresholds_eur_formatted"]] <- thresholds_eur_formatted
  }
  if ("summary_daily_cases_sim_all_formatted" %in% filenames_formatted_missing) {
    summary_daily_cases_sim_all_formatted <- summary_daily_cases_sim_all %>%
      mutate(Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
    list_formatted[["summary_daily_cases_sim_all_formatted"]] <- 
      summary_daily_cases_sim_all_formatted
    
  }
  if ("summary_cumulative_cases_end_sim_all_formatted" %in% filenames_formatted_missing) {
    summary_cumulative_cases_end_sim_all_formatted <- summary_cumulative_cases_end_sim_all %>%
      mutate(Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
    list_formatted[["summary_cumulative_cases_end_sim_all_formatted"]] <- 
      summary_cumulative_cases_end_sim_all_formatted
    
  }
  if ("model_fit_formatted" %in% filenames_formatted_missing) {
    model_fit_formatted <- model_fit %>%
      mutate(Measure = factor(Measure, levels = model_fit_levels),
             Threshold = factor(Threshold, levels = threshold_levels))
    list_formatted[["model_fit_formatted"]] <- model_fit_formatted
  }
  if ("model_fit_summary_formatted" %in% filenames_formatted_missing) {
    model_fit_summary_formatted <- model_fit_summary %>%
      mutate(Measure = factor(Measure, levels = model_fit_levels),
             Threshold = factor(Threshold, levels = threshold_levels))
    list_formatted[["model_fit_summary_formatted"]] <- model_fit_summary_formatted
  }
  if ("effects_within_countries_formatted" %in% filenames_formatted_missing) {
    effects_within_countries_formatted <- effects_within_countries %>% 
      mutate(Threshold = factor(Threshold, levels = threshold_levels),
             Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
    list_formatted[["effects_within_countries_formatted"]] <- 
      effects_within_countries_formatted
  }
  if ("effects_within_countries_summary_formatted" %in% filenames_formatted_missing) {
    effects_within_countries_summary_formatted <- effects_within_countries_summary %>% 
      mutate(Threshold = factor(Threshold, levels = threshold_levels),
             Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
    list_formatted[["effects_within_countries_summary_formatted"]] <- 
      effects_within_countries_summary_formatted
  }
  if ("effects_between_countries_formatted" %in% filenames_formatted_missing) {
    effects_between_countries_formatted <- effects_between_countries %>%
      mutate(Analysis = factor(Analysis, levels = analysis_levels),
             Exposure = factor(Exposure, levels = exposure_levels),
             Leverage_points = factor(Leverage_points, levels = leverage_levels))
    list_formatted[["effects_between_countries_formatted"]] <- 
      effects_between_countries_formatted
  }
  
  # Return list of formatted files
  return(list_formatted)
  
}
