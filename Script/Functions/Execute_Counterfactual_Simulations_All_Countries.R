#' Execute all specified counterfactual simulations for all specified countries.
#' 
#' This code is largely a wrapper for the function 'Simulate_Counterfactual',
#' which simulates a specified counterfactual history for one country. 
#' This code iterates through specified counterfactual conditions, performing
#' the simulations (possibly in parallel) for all designated countries.
#' Simulation-specific summaries of daily and cumulative cases are saved to 
#' separate sub-folders, and summaries across all specified simulations are 
#' returned in a list.
#'
#' @param countries List of countries
#' @param n_days_counterfactual Dataframe with 2 columns ('N_days_first_restriction'
#' and 'N_days_lockdown'), specifying the number of days to bring forward the
#' first restriction and lockdown, respectively
#' @param seed Simulation seed
#' @param max_t Maximum number of days to simulate
#' @param n_runs Number of simulation runs
#' @param prob_equal Whether knot dates should be used with equal probabilities (T/F)
#' @param parallel Whether the simulations should be run in parallel (T/F)
#'
#' @return List ('summary_sim_all') containing two dataframes:
#' (1) 'summary_daily_cases_sim_all', which contains mean and 95% centile values
#' (across \code{n_runs}) of daily cases for all \code{countries} and
#' all specified combinations of \code{n_days_counterfactual}; and 
#' (2) 'summary_cumulative_cases_end_sim_all', which contains mean and 95% 
#' centile values (across \code{n_runs}) of cumulative cases for all 
#' \code{countries} and all specified combinations of \code{n_days_counterfactual}.
#'
#' @examples
#' Execute_Simulation_All_Countries(countries = list("Germany", "United Kingdom"),
#' n_days_counterfactual = tibble(N_days_first_restriction = c(0, 0), N_days_lockdown = c(0, 2)),
#' seed = 13, max_t = 100, n_runs = 100000, prob_equal = FALSE,
#' parallel = TRUE, source_functions = as.vector(lsf.str()))
Execute_Counterfactual_Simulations_All_Countries <- function(countries, n_days_counterfactual,
                                                             seed, max_t, n_runs, prob_equal,
                                                             parallel) {
  
  # Set up parallelisation, if specified
  if (parallel == TRUE) {
    n_cores <- parallel::detectCores()
    cluster <- parallel::makeCluster(n_cores[1] - 1, setup_strategy = "sequential")
    registerDoSNOW(cluster)
    parallel::clusterExport(cl = cluster, varlist = ls(.GlobalEnv), envir = .GlobalEnv)
  }
  
  # Set seed
  set.seed(seed)
  
  # Create empty list to store full summary results of daily and cumulative cases,
  # for all countries from all simulations
  summary_sim_all <- list(summary_daily_cases_sim_all = NULL,
                          summary_cumulative_cases_end_sim_all = NULL)
  
  # Iterate through specified counterfactuals
  for (i in 1:nrow(n_days_counterfactual)) {
    
    # Print simulation number
    print(paste("Simulation", i, "of", nrow(n_days_counterfactual)))
    
    # Set counterfactual shift
    n_days_first_restriction <- n_days_counterfactual[[i, "N_days_first_restriction"]]
    n_days_lockdown <- n_days_counterfactual[[i, "N_days_lockdown"]]
    
    # Label simulation as natural or counterfactual history, and specified number of days
    history <- ifelse(n_days_first_restriction == 0 & n_days_lockdown == 0, 
                      "Natural history", "Counterfactual history")
    
    # Specify folder and path to save simulation results, 
    # and create folder if none exists
    folder <- paste0("Simulation - ", history, " ", 
                     n_days_first_restriction, " ", n_days_lockdown, "/")
    path <- "./Output/Simulations/"
    Create_Folder_If_None_Exists(folder, path)
    
    # Set up progress bar
    iterations <- length(countries)
    progress_bar <- txtProgressBar(min = 1, max = iterations, style = 3)
    progress <- function(n) { setTxtProgressBar(progress_bar, n) }
    options <- list(progress = progress)
    
    # Simulation
    sim_data <- foreach(j = countries, .errorhandling = "pass", 
                        .packages = c("tidyverse"), .options.snow = options) %dopar% 
      Simulate_Counterfactual(country = j, 
                              n_days_first_restriction = n_days_first_restriction, 
                              n_days_lockdown = n_days_lockdown, 
                              max_t = max_t, 
                              n_runs = n_runs, 
                              prob_equal = prob_equal)
    
    # Close progress bar 
    close(progress_bar)
    
    # Combine summary results for all countries; and
    # create Simulation variable (text description of the simulation parameters)
    # and History variable (label as natural/counterfactual history)
    summary_sim_all_i <- list(summary_daily_cases_sim = 
                              bind_rows(map(.x = sim_data, .f = ~.x$summary_daily_cases_sim)),
                            summary_cumulative_cases_end_sim = 
                              bind_rows(map(.x = sim_data, .f = ~.x$summary_cumulative_cases_end_sim))) %>%
      map(., .f = ~.x %>% mutate(Simulation = paste(n_days_first_restriction, n_days_lockdown, sep = ","),
                                 History = history) %>%
            relocate(c(Simulation, History), .after = Country))
    
    # Save all summary tables to simulation-specific folder
    summary_sim_all_i %>% names(.) %>% 
      walk(~ write_csv(summary_sim_all_i[[.]], paste0(path, folder, ., ".csv")))
    
    # Add summary results to full list
    summary_sim_all$summary_daily_cases_sim_all[[i]] <- summary_sim_all_i$summary_daily_cases_sim
    summary_sim_all$summary_cumulative_cases_end_sim_all[[i]] <- summary_sim_all_i$summary_cumulative_cases_end_sim
    
  }
  
  # Stop parallel processing
  if (parallel == TRUE) {
    stopCluster(cluster) 
  }
  
  # Combine summaries of all daily cases, cumulative cases
  summary_sim_all <- as.list(names(summary_sim_all)) %>%
    map(., .f = ~summary_sim_all[[.]] %>% bind_rows) %>%
    setNames(., names(summary_sim_all))
  
  # Return summaries of daily and cumulative cases, for all countries from all simulations
  return(summary_sim_all = summary_sim_all)
  
}
