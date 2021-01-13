# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script simulates the natural history of COVID-19 growth 
# for all European countries for which we have data relating to both cases and policies.
# It uses the 'best' pairs of knot points identified in script 'Identify knot points.R'.

# It also calculates the number of days from lockdown until the mean number of daily cases
# falls below specified thresholds (based on population).

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

# Import files containing best knot point pairs and country summaries
knots_best <- read_csv(paste0(results_directory, "knots_best.csv"))
summary_eur <- read_csv(paste0(results_directory, "summary_eur.csv"))

# Import file containing possible counterfactual conditions
possible_days_counterfactual <- read_csv(paste0(results_directory, "possible_days_counterfactual.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data,
# those which entered lockdown, and those which can be modelled
load(paste0(results_directory, "countries_eur.RData"))
load(paste0(results_directory, "countries_eur_lockdown.RData"))
load(paste0(results_directory, "countries_eur_modelled.RData"))

## Functions -------------------------------------------------------------------

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

# Function to simulate the growth (natural or counterfactual) of cases of COVID-19
# Arguments:
# (1) country = country to simulate
# (2) n_days_first_restriction = number of days earlier to simulate first restriction (>= 0)
# (3) n_days_lockdown = number of days earlier to simulate lockdown (>= 0); ignored if country didn't enter lockdown
###### (n_days_first_restriction = n_days_lockdown = 0 indicates natural history)
# (4) max_t = maximum number of days to simulate
# (5) n_runs = number of simulation runs
# (6) prob_equal = whether knot dates should be used with equal probabilities
# Returns: list of two dataframes - (1) daily and (2) cumulative cases of COVID-19
Simulate_Growth <- function(country, n_days_first_restriction, n_days_lockdown,
                            max_t, n_runs, prob_equal = c(TRUE, FALSE)) {
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Print warning and stop if n_days_first_restriction or n_days_lockdown are less than zero
  if (n_days_first_restriction < 0 | n_days_lockdown < 0) {
    stop("The following arguments must be >= 0: n_days_first_restriction, n_days_lockdown.")
  }
  
  # Filter cases/deaths, summary, best knots, and possible counterfactual dataframes by country
  data_eur_country <- data_eur %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  knots_best_country <- knots_best %>% filter(Country == country)
  possible_days_counterfactual_country <- possible_days_counterfactual %>% filter(Country == country)
  
  # Record dates of first restriction and lockdown in country
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  
  # Record max number of knots 
  max_n_knots <- possible_days_counterfactual_country %>% pull(Max_n_knots) %>% head(1)
  
  # Print message that n_days_lockdown will be ignored if: 
  # Country didn't enter lockdown, lockdown was implemented immediately, or
  # country did enter lockdown but all of best knot pairs only have one knot.
  # Set value of n_days_lockdown to NA
  if (max_n_knots == 1) {
    if (is.na(date_lockdown)) {
      warning(paste0("Lockdown was not implemented in ", country, 
                     ". Parameter n_days_lockdown will be ignored."))
    } else if (date_first_restriction == date_lockdown) {
      warning(paste0("Lockdown was implemented immediately in ", country, 
                     " and thus did not have a unique effect on growth", 
                     ". Parameter n_days_lockdown will be ignored."))
    } else {
      warning(paste0("Lockdown did not have a unique effect on growth in ", country,
                     ". Parameter n_days_lockdown will be ignored."))
    }
    n_days_lockdown <- as.numeric(NA)
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
  
  # Label simulation as natural or counterfactual history
  history <- ifelse(n_days_first_restriction == 0 & (is.na(n_days_lockdown) | n_days_lockdown == 0), 
                    "Natural history", "Counterfactual history")
  
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
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_start, to = date_end, by = 1)
  
  # Create empty matrices for simulated incidence data
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- matrix(nrow = 0, ncol = length(dates) + 1, 
                            dimnames = list(NULL, as.character(seq.Date(from = date_start - 1, to = date_end, by = 1))))
  
  # Define number of best knot point pairs
  n_knots_best <- nrow(knots_best_country_sim)
  
  # (1) Iterate through possible knot date pairs
  for (i in 1:n_knots_best) {
    
    # Refilter best knots dataframe by row i
    knots_best_country_sim_i <- knots_best_country_sim %>% filter(row_number() == i)
    
    # Record number of knots
    n_knots <- knots_best_country_sim_i %>% pull(N_knots)
    
    # Set knot dates
    knot_date_1_i <- knots_best_country_sim_i %>% pull(Knot_date_1)
    knot_date_2_i <- knots_best_country_sim_i %>% pull(Knot_date_2)
    
    # Define growth parameters - means and SDs
    growth_factor_1_i <- knots_best_country_sim_i %>% pull(Growth_factor_1)
    growth_factor_2_i <- knots_best_country_sim_i %>% pull(Growth_factor_2)
    growth_factor_3_i <- knots_best_country_sim_i %>% pull(Growth_factor_3)
    growth_factor_1_sd_i <- knots_best_country_sim_i %>% pull(Growth_factor_1_sd)
    growth_factor_2_sd_i <- knots_best_country_sim_i %>% pull(Growth_factor_2_sd)
    growth_factor_3_sd_i <- knots_best_country_sim_i %>% pull(Growth_factor_3_sd)
    
    # Define number of simulation runs for specified knot dates
    n_runs_i <- knots_best_country_sim_i %>% pull(N)  
    
    # Create matrices for simulated incidence data for given knot dates
    # (1 row per simulation run, 1 col per date)
    daily_cases_sim_i <- 
      matrix(nrow = n_runs_i, ncol = length(dates) + 1,
             dimnames = list(NULL, as.character(seq.Date(from = date_start - 1, to = date_end, by = 1))))
    # Initialise matrix with data at date_start - 1
    daily_cases_sim_i[, 1] <- inc_startminus1
    
    # (2) Iterate through dates
    for (t in as.list(dates)) {
      
      # Get daily cases at time t-1
      inc_tminus1 <- daily_cases_sim_i[, as.character(t-1)]
      
      # Define growth parameters
      if (n_knots == 0) {  # NO knot points
        growth <- rlnorm(n = n_runs_i,
                         meanlog = Calculate_mean_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i),
                         sdlog = Calculate_sd_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i))
      } else if (n_knots == 1) {  # ONE knot point
        if (t <= knot_date_1_i) {
          growth <- rlnorm(n = n_runs_i,
                           meanlog = Calculate_mean_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i),
                           sdlog = Calculate_sd_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i))
        } else {
          growth <- rlnorm(n = n_runs_i,
                           meanlog = Calculate_mean_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i),
                           sdlog = Calculate_sd_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i))
        }
      } else {  # TWO knot points
        if (t <= knot_date_1_i) {
          growth <- rlnorm(n = n_runs_i,
                           meanlog = Calculate_mean_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i),
                           sdlog = Calculate_sd_log(mean = growth_factor_1_i, sd = growth_factor_1_sd_i))
        } else if (t <= knot_date_2_i) {
          growth <- rlnorm(n = n_runs_i,
                           meanlog = Calculate_mean_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i),
                           sdlog = Calculate_sd_log(mean = growth_factor_2_i, sd = growth_factor_2_sd_i))
        } else {
          growth <- rlnorm(n = n_runs_i,
                           meanlog = Calculate_mean_log(mean = growth_factor_3_i, sd = growth_factor_3_sd_i),
                           sdlog = Calculate_sd_log(mean = growth_factor_3_i, sd = growth_factor_3_sd_i))
        }
      }
      
      # Calculate daily cases at time t and record
      inc_t <- growth*inc_tminus1
      daily_cases_sim_i[, as.character(t)] <- inc_t
      
      # Update progress bar
      setTxtProgressBar(progress_bar, i / (n_knots_best + 1))
      
    }  # (close date loop (3), t)
    
    # Bind knot-specific dataframes to full scenario dataframe
    daily_cases_sim <- rbind(daily_cases_sim, daily_cases_sim_i)
    
  }  # (close knot date loop (2), j)
  
  # Calculate cumulative cases
  daily_cases_sim_copy <- daily_cases_sim
  daily_cases_sim_copy[, 1] <- cum_startminus1
  cumulative_cases_end_sim <- apply(X = daily_cases_sim_copy, MARGIN = 1, FUN = cumsum) %>% t
  
  # Record summaries (mean, 2.5 and 97.5 centiles)
  ## Daily cases:
  summary_daily_cases_sim <- apply(X = daily_cases_sim, MARGIN = 2, FUN = Summarise_centiles) %>%
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country,
           History = history, N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>% 
    relocate(Country, History, N_days_first_restriction, N_days_lockdown)
  ## Cumulative cases:
  summary_cumulative_cases_end_sim <- apply(X = cumulative_cases_end_sim, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country,
           History = history, N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>% 
    relocate(Country, History, N_days_first_restriction, N_days_lockdown)
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  
  # Return list of summary dataframes: simulated daily and cumulative cases
  return(list("summary_daily_cases_sim" = summary_daily_cases_sim, 
              "summary_cumulative_cases_end_sim" = summary_cumulative_cases_end_sim))
  
}

# Function to calculate the first date for which mean simulated incidence data goes below
# one or more thresholds (expressed as a proportion of the total population)
# Arguments:
# (1) country = country to calculate
# (2) thresholds = vector of threshold(s), representing proportion of total population
# (3) data_sim = dataframe containing summary of simulated incidence data
# Returns: summary dataframe containing threshold values, dates cases exceeded thresholds,
# and days since lockdown since threshold values reached
Calculate_Date_Threshold_Reached <- function(country, thresholds, data_sim) {
  
  # Filter simulated cases data and summary data by country
  data_sim_country <- data_sim %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Print warning and stop if there is no simulated incidience data for specified country
  if (nrow(data_sim_country) == 0) {
    stop(paste("No data provided for", country))
  }
  
  # Record type of simulation (natural vs counterfactual) and specified interventions
  history <- data_sim_country %>% pull(History) %>% head(1)
  n_days_first_restriction <- data_sim_country %>% pull(N_days_first_restriction) %>% head(1)
  n_days_lockdown <- data_sim_country %>% pull(N_days_lockdown) %>% head(1)
  
  # Record lockdown date
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)  
  
  # Create empty table to store dates for which specified thresholds reached,
  # and number of days since lockdown
  summary_thresholds <- worldbank_eur %>% filter(Country == country, Year == 2019) %>%
    select(Country, Year, Population) %>% expand_grid(Threshold = thresholds) %>%
    mutate(Threshold_value = Population * Threshold,
           Threshold_exceeded = as.logical(NA),
           Date_cases_below_threshold = as.Date(NA),
           Days_since_lockdown = as.numeric(NA))
  
  # Calculate maximum mean number of simulated daily cases, and first date this number was reached
  max_daily_cases_sim <- data_sim_country %>% select(Mean) %>% max
  date_max_daily_cases_sim <- data_sim_country %>% 
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
      data_sim_country_i <- data_sim_country %>% 
        filter(Date >= date_max_daily_cases_sim)
      
      # Calculate and record first date cases fall below threshold and days since lockdown, if they exist
      date_cases_below_threshold <- data_sim_country_i %>% 
        filter(Mean <= threshold_value_i) %>% slice(1) %>% pull(Date)
      if (length(date_cases_below_threshold) != 0) {
        summary_thresholds[[i, "Date_cases_below_threshold"]] <- date_cases_below_threshold
        summary_thresholds[[i, "Days_since_lockdown"]] <- as.numeric(date_cases_below_threshold - date_lockdown)
      }
      
    } else {
      summary_thresholds[[i, "Days_since_lockdown"]] <- 0
    }
    
  }  # (close loop k)
  
  # Label table with type of simulation and specified interventions
  summary_thresholds <- summary_thresholds %>% 
    mutate(History = history, 
           N_days_first_restriction = n_days_first_restriction,
           N_days_lockdown = n_days_lockdown) %>%
    relocate(c(History, N_days_first_restriction, N_days_lockdown), .after = Country)
  
  # Return summary table of thresholds
  return(list("summary_thresholds" = summary_thresholds))
  
}


# ------------------------------------------------------------------------------
# Simulation and analysis
# ------------------------------------------------------------------------------

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

## Parallelised simulation -----------------------------------------------------

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
  Simulate_Growth(country = i, 
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

# Get list of countries simulated, export to simulation subfolder
countries_eur_sim <- summary_daily_cases_sim %>% pull(Country) %>% unique %>% as.list
save(countries_eur_sim, file = paste0(out_folder, "countries_eur_sim.RData"))
#setdiff(countries_eur_modelled, countries_eur_sim)  # countries not simulated

# Export summary results
write_csv(summary_daily_cases_sim, file = paste0(out_folder, "summary_daily_cases_sim.csv"))
write_csv(summary_cumulative_cases_end_sim, file = paste0(out_folder, "summary_cumulative_cases_end_sim.csv"))

## Sequential simulation -------------------------------------------------------

## Set seed
#set.seed(13)
#
## Simulation
#start <- Sys.time()
#sim_data <- foreach(i = countries_eur_modelled, .errorhandling = "pass") %do% 
#  Simulate_Growth(country = i, 
#                  n_days_first_restriction = n_days_first_restriction, 
#                  n_days_lockdown = n_days_lockdown, 
#                  max_t = max_t, 
#                  n_runs = n_runs, 
#                  prob_equal = prob_equal)
#end <- Sys.time()
#end - start  # ~ 10.5 mins
#
## Combine summary results for all countries
#summary_daily_cases_sim <- map(.x = sim_data, 
#                               .f = ~.x$summary_daily_cases_sim) %>% reduce(bind_rows)
#summary_cumulative_cases_end_sim <- map(.x = sim_data, 
#                                        .f = ~.x$summary_cumulative_cases_end_sim) %>% reduce(bind_rows)
#
## Export summary results
#write_csv(summary_daily_cases_sim, file = paste0(out_folder, "summary_daily_cases_sim.csv"))
#write_csv(summary_cumulative_cases_end_sim, file = paste0(out_folder, "summary_cumulative_cases_end_sim.csv"))

## Calculate dates for which thresholds reached --------------------------------

# Specify population-based thresholds 
thresholds <- c(0.0001, 0.00005, 0.00001)

# Calculate dates for which thresholds reached
summary_thresholds <- foreach(i = countries_eur_modelled, .errorhandling = "pass") %do%
  Calculate_Date_Threshold_Reached(country = i, 
                                   thresholds = thresholds, 
                                   data_sim = summary_daily_cases_sim)

# Combine summary results for all countries
summary_thresholds <- map(.x = summary_thresholds, 
                          .f = ~.x$summary_thresholds) %>% reduce(bind_rows)

# Export summary thresholds table
write_csv(summary_thresholds, file = paste0(out_folder, "summary_thresholds.csv"))

