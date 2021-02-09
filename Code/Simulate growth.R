# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script simulates the natural and counterfactual growth of COVID-19  
# for all European countries for which we have data relating to both cases and policies.
# It uses the pairs of knot points identified in script 
# 'Summarise countries and identify knot points.R'.

# It also calculates the number of days from lockdown until the mean number of daily cases
# falls below specified population-based thresholds.

# All outputs are saved to simulation-specific sub-folders the project directory (./Results/).

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(foreach); library(parallel); library(doSNOW)

# Define storage directory for formatted data
data_directory_f <- paste0("./Data/Formatted/")

# Define storage directory for results
results_directory <- paste0("./Results/")

# Load formatted data
data_eur <- read_csv(paste0(data_directory_f, "Cases_deaths_data_europe.csv"))
worldbank_eur <- read_csv(paste0(data_directory_f, "Worldbank_data_europe.csv"))

# Import files containing best knot point pairs, country summaries, and threshold values
knots_best <- read_csv(paste0(results_directory, "knots_best.csv"))
summary_eur <- read_csv(paste0(results_directory, "summary_eur.csv"))
thresholds_all <- read_csv(paste0(results_directory, "thresholds_all.csv"))

# Import file containing possible counterfactual conditions
possible_days_counterfactual <- read_csv(paste0(results_directory, "possible_days_counterfactual.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data,
# those which entered lockdown, and those which can be modelled
load(paste0(results_directory, "countries_eur.RData"))
load(paste0(results_directory, "countries_eur_lockdown.RData"))
load(paste0(results_directory, "countries_eur_modelled.RData"))

# ------------------------------------------------------------------------------
# Simulation and analysis
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to simulate the growth (natural or counterfactual) of cases of COVID-19
# Arguments:
# (1) country = country to simulate
# (2) n_days_first_restriction = number of days earlier to simulate first restriction (>= 0)
# (3) n_days_lockdown = number of days earlier to simulate lockdown (>= 0); ignored if country didn't enter lockdown
###### (n_days_first_restriction = n_days_lockdown = 0 indicates natural history)
# (4) max_t = maximum number of days to simulate
# (5) n_runs = number of simulation runs
# (6) prob_equal = whether knot dates should be used with equal probabilities
# Returns: list of three summary dataframes - (1) daily cases, (2) cumulative cases,
# and (3) summary of when significant thresholds for incident cases reached
Simulate_Counterfactual <- function(country, n_days_first_restriction, n_days_lockdown,
                                    max_t, n_runs, prob_equal = c(TRUE, FALSE)) {
  
  # Print warning and stop if n_days_first_restriction or n_days_lockdown are less than zero
  if (n_days_first_restriction < 0 | n_days_lockdown < 0) {
    stop("The following arguments must be >= 0: n_days_first_restriction, n_days_lockdown.")
  }
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Filter cases/deaths, summary, thresholds, best knots, and 
  # possible counterfactual dataframes by country
  data_eur_country <- data_eur %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  thresholds_country <- thresholds_all %>% filter(Country == country)
  knots_best_country <- knots_best %>% filter(Country == country)
  possible_days_counterfactual_country <- possible_days_counterfactual %>% filter(Country == country)
  
  # Record dates of first restriction and lockdown (natural history) in country
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  
  # Print message that n_days_lockdown will be ignored if country didn't enter lockdown
  # Set value of n_days_lockdown to NA
  if (is.na(date_lockdown)) {
    warning(paste0("Lockdown was not implemented in ", country, 
                   ". Parameter n_days_lockdown will be ignored."))
    n_days_lockdown <- as.numeric(NA)
  } else {
    # Print message that smaller n_days parameter will be overriden
    # if country implemented first restriction and lockdown on same day
    # Set smaller n_days parameter to value of other
    if (date_first_restriction == date_lockdown & 
        n_days_first_restriction != n_days_lockdown) {
      if (n_days_first_restriction < n_days_lockdown) {
        warn <- "Parameter n_days_first_restriction will be overriden."
        n_days_first_restriction <- n_days_lockdown
      } else {
        warn <- "Parameter n_days_lockdown will be overriden."
        n_days_lockdown <- n_days_first_restriction
      }
      warning(paste0("First restriction and lockdown were implemented simultaneously in ", country,
                     ". ", warn))
    }
  }
  
  # Combine n_days_first_restriction and n_days_lockdown parameters into dataframe,
  # and check whether the combination of values is amongst the possible counterfactual conditions
  n_days_counterfactual <- tibble(N_days_first_restriction = n_days_first_restriction,
                                  N_days_lockdown = n_days_lockdown)
  match <- plyr::match_df(possible_days_counterfactual_country, n_days_counterfactual,
                          on = c("N_days_first_restriction", "N_days_lockdown"))
  
  # Print warning and stop if counterfactual cannot be estimated
  if (nrow(match) == 0) {
    #print("Stop - cannot estimate counterfactual")
    stop(paste0("The specified counterfactual cannot be estimated for ", country, "."))
  }
  
  # Calculate counterfactual lockdown date
  date_lockdown_counterfactual <- summary_eur_country %>% 
    pull(Date_lockdown) - n_days_lockdown
  
  # Calculate knot dates to be used for simulation
  knots_best_country_sim <- Modify_Knot_Dates(df_knots = knots_best_country, 
                                              n_days_first_restriction = n_days_first_restriction,
                                              n_days_lockdown = n_days_lockdown)
  
  # Calculate number of simulation runs (N) for each pair of knot dates
  if (prob_equal == TRUE) {
    prob_knots <- knots_best_country_sim %>% pull(Prob_equal)
  } else {
    prob_knots <- knots_best_country_sim %>% pull(Prob_unequal)
  }
  knots_best_country_sim <- knots_best_country_sim %>% mutate(N = Round_preserve_sum(prob_knots * n_runs))
  
  # Record simulation start date and calculate end date
  date_start <- summary_eur_country %>% pull(Date_start)
  date_end <- as.Date(date_start + max_t)
  
  # Record incident and cumulative cases (MA7) on date_start - 1
  inc_startminus1 <- data_eur_country %>% filter(Date == (date_start - 1)) %>% pull(Daily_cases_MA7)
  cum_startminus1 <- data_eur_country %>% filter(Date == (date_start - 1)) %>% pull(Cumulative_cases_end_MA7)
  
  # Create empty list for storing simulated incidence data
  daily_cases_sim <- list()
  
  # (1) Iterate through possible knot date pairs
  for (i in 1:nrow(knots_best_country_sim)) {
    
    # Refilter best knots dataframe by row i
    knots_best_country_sim_i <- knots_best_country_sim %>% filter(row_number() == i)
    
    # Record number of knots
    n_knots_i <- knots_best_country_sim_i %>% pull(N_knots)
    
    # Set knot dates
    knot_date_1_i <- knots_best_country_sim_i %>% pull(Knot_date_1)
    knot_date_2_i <- knots_best_country_sim_i %>% pull(Knot_date_2)
    
    # Define growth parameters - means and SDs
    parameters_i <- knots_best_country_sim_i %>% select(contains("Growth")) %>% as.list
    
    # Define number of simulation runs for specified knot dates
    n_runs_i <- knots_best_country_sim_i %>% pull(N)  
    
    # Estimate incident cases over modelling period
    daily_cases_sim_i <- Simulate_Growth(date_start = date_start, 
                                         date_end = date_end, 
                                         start_value = inc_startminus1,
                                         n_runs = n_runs_i,
                                         n_knots = n_knots_i,
                                         knot_date_1 = knot_date_1_i, 
                                         knot_date_2 = knot_date_2_i,
                                         parameters = parameters_i)
    
    # Record simulated incidence data in list
    daily_cases_sim[[i]] <- daily_cases_sim_i
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i / (nrow(knots_best_country_sim) + 1))
    
  }  # (close loop 1 (i))
  
  # Bind all simulated incidence data together
  daily_cases_sim <- do.call(rbind, daily_cases_sim)
  
  # Calculate cumulative cases
  daily_cases_sim_copy <- daily_cases_sim
  daily_cases_sim_copy[, 1] <- cum_startminus1
  cumulative_cases_end_sim <- apply(X = daily_cases_sim_copy, MARGIN = 1, FUN = cumsum) %>% t
  
  # Record summaries (mean, 2.5 and 97.5 centiles)
  ## Daily cases:
  summary_daily_cases_sim <- apply(X = daily_cases_sim, MARGIN = 2, FUN = Summarise_centiles) %>%
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country,
           N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>% 
    relocate(Country, N_days_first_restriction, N_days_lockdown)
  ## Cumulative cases:
  summary_cumulative_cases_end_sim <- apply(X = cumulative_cases_end_sim, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country,
           N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>% 
    relocate(Country, N_days_first_restriction, N_days_lockdown)
  
  # Calculate dates for which thresholds reached
  summary_thresholds <- Calculate_Date_Threshold_Reached(thresholds = thresholds_country, 
                                                         data_sim = summary_daily_cases_sim,
                                                         date_lockdown_counterfactual = date_lockdown_counterfactual) %>%
    mutate(N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>%
    relocate(c(N_days_first_restriction, N_days_lockdown), .after = Country)
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  close(progress_bar)
  
  # Return list of summary dataframes: simulated daily and cumulative cases
  return(list("summary_daily_cases_sim" = summary_daily_cases_sim, 
              "summary_cumulative_cases_end_sim" = summary_cumulative_cases_end_sim,
              "summary_thresholds" = summary_thresholds))
  
}

# Function to modify knot dates (for simulating natural or counterfactual history)
# Arguments:
# (1) df_knots = dataframe containing knot dates for natural history
# (2) n_days_first_restriction = number of days earlier to simulate first restriction
# (3) n_days_lockdown = number of days earlier to simulate lockdown
# Returns: dataframe with knot dates to be used for simulation 
Modify_Knot_Dates <- function(df_knots, n_days_first_restriction, n_days_lockdown) {
  
  # Mutate knot dates for specified scenario
  df_knots_mutate <- df_knots %>%
    mutate(Knot_date_1 = Knot_date_1 - n_days_first_restriction,
           Knot_date_2 = Knot_date_2 - n_days_lockdown)
  
  # Return dataframe
  return(df_knots_mutate)
  
}

# Function to preserve rounded sum
# (from https://www.r-bloggers.com/round-values-while-preserve-their-rounded-sum-in-r/)
# Arguments: vector of values (x), number of digits (default = 0)
Round_preserve_sum <- function(x, digits = 0) {
  up <- 10 ^ digits
  x <- x * up
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y / up
}

# Function to simulate the growth of cases over a specified time period
# Arguments:
# (1) date_start = start date
# (2) date_end = end date
# (3) start value = value of daily cases on which to start simulation
# (4) n_runs = number of simulation runs
# (5) n_knots = number of knots (where growth factor changes)
# (6) knot_date_1 = date of first knot
# (7) knot_date_2 = date of second knot
# (8) parameters = list containing growth parameters 
# Returns: matrix of simulated incident cases
Simulate_Growth <- function(date_start, date_end, start_value,
                            n_runs,
                            n_knots = c(0, 1, 2),
                            knot_date_1, knot_date_2,
                            parameters) {
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_start, to = date_end, by = 1)
  
  # Create matrices for simulated incidence data for given knot dates
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- 
    matrix(nrow = n_runs, ncol = length(dates) + 1,
           dimnames = list(NULL, as.character(seq.Date(from = date_start - 1, to = date_end, by = 1))))
  
  # Initialise matrix with data at date_start - 1
  daily_cases_sim[, 1] <- start_value
  
  # Iterate through dates
  for (t in as.list(dates)) {
    
    # Get daily cases at time t-1
    inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
    
    # Define growth parameters
    if (n_knots == 0) {  # NO knot points
      
      growth <- rlnorm(n = n_runs,
                       meanlog = Calculate_mean_log(mean = parameters$Growth_factor_1, 
                                                    sd = parameters$Growth_factor_1_sd),
                       sdlog = Calculate_sd_log(mean = parameters$Growth_factor_1, 
                                                sd = parameters$Growth_factor_1_sd))
      
    } else if (n_knots == 1) {  # ONE knot point
      
      if (t <= knot_date_1) {
        growth <- rlnorm(n = n_runs,
                         meanlog = Calculate_mean_log(mean = parameters$Growth_factor_1, 
                                                      sd = parameters$Growth_factor_1_sd),
                         sdlog = Calculate_sd_log(mean = parameters$Growth_factor_1, 
                                                  sd = parameters$Growth_factor_1_sd))
      } else {
        growth <- rlnorm(n = n_runs,
                         meanlog = Calculate_mean_log(mean = parameters$Growth_factor_2, 
                                                      sd = parameters$Growth_factor_2_sd),
                         sdlog = Calculate_sd_log(mean = parameters$Growth_factor_2, 
                                                  sd = parameters$Growth_factor_2_sd))
      }
    } else {  # TWO knot points
      
      if (t <= knot_date_1) {
        growth <- rlnorm(n = n_runs,
                         meanlog = Calculate_mean_log(mean = parameters$Growth_factor_1, 
                                                      sd = parameters$Growth_factor_1_sd),
                         sdlog = Calculate_sd_log(mean = parameters$Growth_factor_1, 
                                                  sd = parameters$Growth_factor_1_sd))
      } else if (t <= knot_date_2) {
        growth <- rlnorm(n = n_runs,
                         meanlog = Calculate_mean_log(mean = parameters$Growth_factor_2, 
                                                      sd = parameters$Growth_factor_2_sd),
                         sdlog = Calculate_sd_log(mean = parameters$Growth_factor_2, 
                                                  sd = parameters$Growth_factor_2_sd))
      } else {
        growth <- rlnorm(n = n_runs,
                         meanlog = Calculate_mean_log(mean = parameters$Growth_factor_3, 
                                                      sd = parameters$Growth_factor_3_sd),
                         sdlog = Calculate_sd_log(mean = parameters$Growth_factor_3, 
                                                  sd = parameters$Growth_factor_3_sd))
      }
      
    }
    
    # Calculate daily cases at time t and record
    inc_t <- growth*inc_tminus1
    daily_cases_sim[, as.character(t)] <- inc_t
    
  }  
  
  # Return list of incident cases
  return(daily_cases_sim)
  
}

# Functions to calculate mean and SD of growth factor distributions on log scale
# (equations from https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments)
# Arguments: mean, sd of variable on normal scale
## (1) Mean
Calculate_mean_log <- function(mean, sd) {
  mean_log <- log(mean^2 / sqrt(sd^2 + mean^2))
  return(mean_log)
}
## (2) SD
Calculate_sd_log <- function(mean, sd) {
  sd_log <- sqrt(log(1 + (sd^2 / mean^2)))
  return(sd_log)
}

# Function to summarise mean, 2.5 and 97.5 centiles
# Argument: vector of values (x)
Summarise_centiles <- function(x) {
  x <- x[!is.na(x) & !is.infinite(x)]
  c(Mean = mean(x), 
    C_025 = quantile(x, 0.025, names = FALSE), 
    C_975 = quantile(x, 0.975, names = FALSE))
}

# Function to calculate the first date for which mean simulated incidence data goes below
# one or more thresholds (expressed as a proportion of the total population)
# Arguments:
# (1) thresholds = dataframe of threshold values
# (2) data_sim = dataframe containing summary of simulated incidence data
# (3) date_lockdown_counterfactual = date of lockdown in simulation
# Returns: summary dataframe containing threshold values, dates cases exceeded thresholds,
# and days since lockdown since threshold values reached
Calculate_Date_Threshold_Reached <- function(thresholds, data_sim, date_lockdown_counterfactual) {
  
  # Create empty table to store dates for which specified thresholds reached,
  # and number of days since lockdown
  summary_thresholds <- thresholds %>% mutate(Threshold_exceeded = as.logical(NA),
                                              Date_cases_below_threshold = as.Date(NA),
                                              Days_since_lockdown = as.numeric(NA))
  
  # Calculate maximum mean number of simulated daily cases, and first date this number was reached
  max_daily_cases_sim <- data_sim %>% select(Mean) %>% max
  date_max_daily_cases_sim <- data_sim %>% 
    filter(Mean == max_daily_cases_sim) %>% slice(1) %>% pull(Date)
  
  # Calculate and record dates for which thresholds reached
  for (i in 1:nrow(summary_thresholds)) {
    
    # Define threshold value
    threshold_value_i <- summary_thresholds %>% slice(i) %>% pull(Threshold_value)
    
    # Create T/F indicator for whether threshold value was ever exceeded & record
    threshold_exceeded_i <- max_daily_cases_sim > threshold_value_i
    summary_thresholds[[i, "Threshold_exceeded"]] <- threshold_exceeded_i
    
    # If threshold value was exceeded, find first date cases went below threshold
    # else, record days required since lockdown to fall below threshold as zero
    if (threshold_exceeded_i == TRUE) {
      
      # Filter dataset by dates >= date of max number of daily cases
      data_sim_i <- data_sim %>% 
        filter(Date >= date_max_daily_cases_sim)
      
      # Calculate and record first date cases fall below threshold and days since lockdown, if they exist
      date_cases_below_threshold <- data_sim_i %>% 
        filter(Mean <= threshold_value_i) %>% slice(1) %>% pull(Date)
      if (length(date_cases_below_threshold) != 0) {
        summary_thresholds[[i, "Date_cases_below_threshold"]] <- date_cases_below_threshold
        summary_thresholds[[i, "Days_since_lockdown"]] <- as.numeric(date_cases_below_threshold - date_lockdown_counterfactual)
      }
      
    } else {
      summary_thresholds[[i, "Days_since_lockdown"]] <- 0
    }
    
  }  # (close loop k)
  
  # Return summary table of thresholds
  return(summary_thresholds)
  
}

## Simulation ------------------------------------------------------------------

# Specify simulation parameters
n_days_first_restriction <- 0
n_days_lockdown <- 0
max_t <- 548  # (1.5 years)
n_runs <- 100000
prob_equal <- FALSE 

# Label simulation as natural or counterfactual history, and specified number of days
history <- ifelse(n_days_first_restriction == 0 & n_days_lockdown == 0, 
                  "Natural history", "Counterfactual history")
n_days <- paste(n_days_first_restriction, n_days_lockdown)

# Create folder for storing simulation results, if none already exists
out_folder <- paste0(results_directory, "Simulation - ", history, " ", n_days, "/")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

### Parallelised ---------------------------------------------------------------

# Set up parallelisation
n_cores <- detectCores()
cluster <- parallel::makeCluster(n_cores[1] - 1, setup_strategy = "sequential")
registerDoSNOW(cluster)

# Set up progress bar
iterations <- length(countries_eur_modelled)
progress_bar <- txtProgressBar(min = 1, max = iterations, style = 3)
progress <- function(n) { setTxtProgressBar(progress_bar, n) }
options <- list(progress = progress)

# Set seed
set.seed(13)

# Simulation
start <- Sys.time()
sim_data <- foreach(i = countries_eur_modelled, .errorhandling = "pass", 
                    .packages = "tidyverse", .options.snow = options) %dopar% 
  Simulate_Counterfactual(country = i, 
                          n_days_first_restriction = n_days_first_restriction, 
                          n_days_lockdown = n_days_lockdown, 
                          max_t = max_t, 
                          n_runs = n_runs, 
                          prob_equal = prob_equal)
end <- Sys.time()
end - start  # ~4 mins

# Close progress bar and stop parallel processing
close(progress_bar)
stopCluster(cluster)

# Combine summary results for all countries
summary_daily_cases_sim <- map(.x = sim_data, 
                               .f = ~.x$summary_daily_cases_sim) %>% reduce(bind_rows)
summary_cumulative_cases_end_sim <- map(.x = sim_data, 
                                        .f = ~.x$summary_cumulative_cases_end_sim) %>% reduce(bind_rows)
summary_thresholds <- map(.x = sim_data, 
                          .f = ~.x$summary_thresholds) %>% reduce(bind_rows)

### Sequential -----------------------------------------------------------------

## Set seed
#set.seed(13)
#
## Simulation
#start <- Sys.time()
#sim_data <- foreach(i = countries_eur_modelled, .errorhandling = "pass") %do% 
#  Simulate_Counterfactual(country = i, 
#                          n_days_first_restriction = n_days_first_restriction, 
#                          n_days_lockdown = n_days_lockdown, 
#                          max_t = max_t, 
#                          n_runs = n_runs, 
#                          prob_equal = prob_equal)
#end <- Sys.time()
#end - start  # ~ 10.5 mins
#
## Combine summary results for all countries
#summary_daily_cases_sim <- map(.x = sim_data, 
#                               .f = ~.x$summary_daily_cases_sim) %>% reduce(bind_rows)
#summary_cumulative_cases_end_sim <- map(.x = sim_data, 
#                                        .f = ~.x$summary_cumulative_cases_end_sim) %>% reduce(bind_rows)
#summary_thresholds <- map(.x = sim_data, 
#                          .f = ~.x$summary_thresholds) %>% reduce(bind_rows)

## SAVE all output -------------------------------------------------------------

# Get list of countries simulated, export to simulation subfolder
countries_eur_sim <- summary_daily_cases_sim %>% pull(Country) %>% unique %>% as.list
save(countries_eur_sim, file = paste0(out_folder, "countries_eur_sim.RData"))
#setdiff(countries_eur_modelled, countries_eur_sim)  # countries not simulated

# Create list of all summary datasets to export
summary_sim_all <- list(summary_daily_cases_sim = summary_daily_cases_sim, 
                        summary_cumulative_cases_end_sim = summary_cumulative_cases_end_sim, 
                        summary_thresholds = summary_thresholds)

# For all summary datasets, 
# create Simulation variable which contains a text description of the simulation parameters,
# and History variable labelling simulation as natural/counterfactual history
summary_sim_all <- map(.x = summary_sim_all, 
                       .f = ~.x %>% mutate(Simulation = paste(n_days_first_restriction, n_days_lockdown, sep = ","),
                                           History = history) %>%
                         relocate(c(Simulation, History), .after = Country))

# Save all summary tables
summary_sim_all %>% names(.) %>% 
  walk(~ write_csv(summary_sim_all[[.]], paste0(out_folder, ., ".csv")))
