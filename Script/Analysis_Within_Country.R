################################################################################
#
# Script name:        Analysis within-country.R
# Script description: This script ....
# Author:             KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

## Load required packages ------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(foreach); library(doSNOW)

## Load source functions -------------------------------------------------------

source("./Script/Functions/Calculate_Cumulative_Cases.R")
source("./Script/Functions/Calculate_Parameters_Log.R")
source("./Script/Functions/Check_If_Counterfactual_Possible.R")
source("./Script/Functions/Create_Folder_If_None_Exists.R")
source("./Script/Functions/Get_Cases_On_Date.R")
source("./Script/Functions/Get_Dates.R")
source("./Script/Functions/Modify_Knot_Dates.R")
source("./Script/Functions/Simulate_Counterfactual.R")
source("./Script/Functions/Simulate_Daily_Cases.R")
source("./Script/Functions/Summarise_Centiles.R")

# Create vector of all source functions
source_functions <- as.vector(lsf.str())

## Load data -------------------------------------------------------------------

# Define filenames which contain data required for simulation
filenames <- list("Cases_deaths_data_europe", 
                  "knots_best",
                  "summary_eur",
                  "possible_days_counterfactual")

# Import all dataframes; save all to global environment
dataframes <- filenames %>%
  map(., .f = ~list.files(path = ".",
                          recursive = TRUE, 
                          pattern = paste0(., ".csv"),
                          full.names = TRUE)) %>%
  map(., .f = ~read_csv(.)) %>%
  set_names(filenames)
list2env(dataframes, globalenv()); rm(filenames, dataframes)

# Load list of European countries for which we have both cases/deaths data and policy data,
# and those which entered lockdown
load("./Output/countries_eur.RData")
load("./Output/countries_eur_lockdown.RData")

# IDENTIFICATION OF SIMULATION PARAMETERS --------------------------------------

# Insert code to calculate best knot date pairs and associated growth parameters,
# and calculate possible counterfactual conditions

# (will need to remove import of knots_best.csv and possible_days_counterfactual.csv
# files in previous section)


# SIMULATION -------------------------------------------------------------------

## Define global simulation parameters -----------------------------------------

# Specify combinations of counterfactual conditions to simulate
# (including natural history)
n_days_counterfactual <- tibble(N_days_first_restriction = 
                                  c(0, 0, 0, 0, 0, 1, 3, 5, 7),
                                N_days_lockdown =
                                  c(0, 1, 3, 5, 7, 1, 3, 5, 7))

# Specify simulation parameters
countries <- countries_eur_lockdown
max_t <- 548  # (1.5 years)
n_runs <- 100000
prob_equal <- FALSE 

## Simulate --------------------------------------------------------------------

# Set up parallelisation
n_cores <- parallel::detectCores()
cluster <- parallel::makeCluster(n_cores[1] - 1, setup_strategy = "sequential")
registerDoSNOW(cluster)
parallel::clusterExport(cl = cluster, varlist = source_functions, envir = globalenv())

# Set seed
set.seed(13)

# Iterate through counterfactual conditions
start <- Sys.time()
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
  sim_data <- foreach(i = countries, .errorhandling = "pass", 
                      .packages = c("tidyverse"), .options.snow = options) %dopar% 
    Simulate_Counterfactual(country = i, 
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
  summary_sim_all <- list(summary_daily_cases_sim = 
                            bind_rows(map(.x = sim_data, .f = ~.x$summary_daily_cases_sim)),
                          summary_cumulative_cases_end_sim = 
                            bind_rows(map(.x = sim_data, .f = ~.x$summary_cumulative_cases_end_sim))) %>%
    map(., .f = ~.x %>% mutate(Simulation = paste(n_days_first_restriction, n_days_lockdown, sep = ","),
                               History = history) %>%
          relocate(c(Simulation, History), .after = Country))
  
  # Save all summary tables
  summary_sim_all %>% names(.) %>% 
    walk(~ write_csv(summary_sim_all[[.]], paste0(path, folder, ., ".csv")))
  
}
end <- Sys.time(); end - start  

# Stop parallel processing
stopCluster(cluster)

