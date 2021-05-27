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
Format_Data_For_Plotting <- function(filenames = c("thresholds_eur",
                                                   "summary_daily_cases_sim_all",
                                                   "summary_cumulative_cases_end_sim_all"),
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
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")
  
  # Format all specified filenames which are not already formatted 
  # (order factors)
  if ("thresholds_eur_formatted" %in% filenames_formatted_missing) {
    thresholds_eur_formatted <- thresholds_eur %>%
      mutate(Threshold = factor(Threshold, threshold_levels))
  }
  if ("summary_daily_cases_sim_all_formatted" %in% filenames_formatted_missing) {
    summary_daily_cases_sim_all_formatted <- summary_daily_cases_sim_all %>%
      mutate(Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
  }
  if ("summary_cumulative_cases_end_sim_all_formatted" %in% filenames_formatted_missing) {
    summary_cumulative_cases_end_sim_all_formatted <- summary_cumulative_cases_end_sim_all %>%
      mutate(Simulation = factor(Simulation, levels = simulation_levels),
             History = factor(History, levels = history_levels))
  }
  
  # no formatting required for knots_best, Cases_deaths_data_europe 
  
  # Return list of formatted files
  return(list(thresholds_eur_formatted = thresholds_eur,
              summary_daily_cases_sim_all_formatted = summary_daily_cases_sim_all,
              summary_cumulative_cases_end_sim_all_formatted = summary_cumulative_cases_end_sim_all))
  
}
