#' Import all simulated data into the global environment.
#'
#' @param location Folder where simulated data are located
#' @param filenames Vector of filenames containing simulated data 
#'
#' @return Message describing which of \code{filenames} were loaded into the
#' global environment
#'
#' @examples
#' Import_All_Simulated_Data(location = "./Output/)
Import_All_Simulated_Data <- function(location,
                                      filenames = c("summary_daily_cases_sim", 
                                                    "summary_cumulative_cases_end_sim")) {
  
  # Create empty list for all simulated data files
  summary_sim_all <- list()
  
  # Import all simulation files from designated folder
  for (i in filenames) {
    
    # Get names of files in subfolders which contain designated simualtion results
    sim_files <- list.files(path = location,
                            recursive = TRUE, 
                            pattern = paste0(i, ".csv"),
                            full.names = TRUE)
    
    # Read in all files and bind together
    summary_sim_all[[i]] <- lapply(sim_files, read_csv, col_types = cols(Simulation = col_character(),
                                                                         N_days_first_restriction = col_double(),
                                                                         N_days_lockdown = col_double(),
                                                                         Date_first_restriction = col_date(),
                                                                         Date_lockdown = col_date())) %>% 
      reduce(bind_rows) %>% 
      mutate(across(where(is.character), as.factor)) %>%
      arrange(N_days_first_restriction, N_days_lockdown)
    
  }
  
  # Append file names with '_all' and save simulation results as separate objects in global environment
  names(summary_sim_all) <- paste0(names(summary_sim_all), "_all")
  list2env(summary_sim_all, .GlobalEnv)
  
  # Print message
  cat("All simulated data successfully imported into the global environment:",
      paste(names(summary_sim_all), collapse = ", "), sep = "\n")
  
}
