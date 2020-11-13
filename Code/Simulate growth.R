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
library(tidyverse)

# Run source code to import and format data
source("./Code/Import, format, and summarise data.R")

# Import best knot points dataframe 
knots_best <- read_csv(paste0(out, "Best knot points.csv")) %>% group_by(Country)

# Set storage directory for outputs
out <- paste0("./Results/")

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

# ------------------------------------------------------------------------------
# Simulation
# ------------------------------------------------------------------------------

# Define maximum number of days to simulate
max_t <- 548  # (1.5 years)

# Define number of simulation runs per country
n_runs <- 100000

# Create lists to store SUMMARY outputs from all simulations
summary_daily_cases_sim <- list()  # daily cases
summary_cumulative_cases_end_sim <- list()  # cumulative cases

# Set seed
set.seed(23)

# (1) Iterate through countries
start <- Sys.time()
for (i in countries_eur_lockdown) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, summary, best knots dataframes by country
  data_eur_i <- data_eur %>% filter(Country == country)
  summary_eur_i <- summary_eur %>% filter(Country == country)
  knots_best_i <- knots_best %>% filter(Country == country)
  
  # Record important dates
  date_50 <- summary_eur_i %>% pull(Date_50)  # (starting date of simulation)
  date_end <- as.Date(date_50 + max_t)  # (end date of simulation) 
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)  # (date of lockdown)
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_50, to = date_end, by = 1)
  
  # Create empty matrices for simulated data for given country
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim_i <- cumulative_cases_end_sim_i <- 
    matrix(nrow = 0, ncol = length(dates) + 1,
           dimnames = list(NULL, as.character(seq.Date(from = date_50 - 1, to = date_end, by = 1))))
  
  # Calculate number of simulation runs for each pair of knot dates
  prob_knots <- knots_best_i %>% pull(Prob_unequal)
  knots_best_i <- knots_best_i %>% mutate(N = Round_preserve_sum(prob_knots * n_runs))
  
  # Define number of best knot point pairs
  n_knots_best_i <- nrow(knots_best_i)
  
  # (2) Iterate through possible knot date pairs
  for (j in 1:n_knots_best_i) {
    
    # Refilter best knots dataframe by row j
    knots_best_j <- knots_best_i %>% filter(row_number() == j)
    
    # Record number of knot dates
    n_knots <- knots_best_j %>% pull(N_knots)
    
    # Set knot dates
    knot_date_1_j <- knots_best_j %>% pull(Knot_date_1)
    knot_date_2_j <- knots_best_j %>% pull(Knot_date_2)
    
    # Define growth parameters - means and SDs
    growth_factor_1_j <- knots_best_j %>% pull(Growth_factor_1)
    growth_factor_2_j <- knots_best_j %>% pull(Growth_factor_2)
    growth_factor_3_j <- knots_best_j %>% pull(Growth_factor_3)
    growth_factor_1_sd_j <- knots_best_j %>% pull(Growth_factor_1_sd)
    growth_factor_2_sd_j <- knots_best_j %>% pull(Growth_factor_2_sd)
    growth_factor_3_sd_j <- knots_best_j %>% pull(Growth_factor_3_sd)
    
    # Define number of simulation runs for specified knot dates
    n_runs_j <- knots_best_j %>% pull(N)  
    
    # Create matrices for simulated data for given knot dates
    # (1 row per simulation run, 1 col per date)
    daily_cases_sim_j <- cumulative_cases_end_sim_j <- 
      matrix(nrow = n_runs_j, ncol = length(dates) + 1,
             dimnames = list(NULL, as.character(seq.Date(from = date_50 - 1, to = date_end, by = 1))))
    # Initialise matrices with data at date_50 - 1
    daily_cases_sim_j[, 1] <- data_eur_i %>% 
      filter(Date == (date_50 - 1)) %>% pull(Daily_cases)
    cumulative_cases_end_sim_j[, 1] <- data_eur_i %>% 
      filter(Date == (date_50 - 1)) %>% pull(Cumulative_cases_end)
    
    # (3) Iterate through dates
    for (t in as.list(dates)) {
      
      # Get daily and cumulative cases from time t-1
      inc_tminus1 <- daily_cases_sim_j[, as.character(t-1)]
      cum_tminus1 <- cumulative_cases_end_sim_j[, as.character(t-1)]
      
      # Define growth parameters
      if (n_knots == 0) {  # NO knot points
        growth <- rlnorm(n = n_runs_j,
                         meanlog = Calculate_mean_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j),
                         sdlog = Calculate_sd_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j))
      } else if (n_knots == 1) {  # ONE knot point
        if (t <= knot_date_1_j) {
          growth <- rlnorm(n = n_runs_j,
                           meanlog = Calculate_mean_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j),
                           sdlog = Calculate_sd_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j))
        } else {
          growth <- rlnorm(n = n_runs_j,
                           meanlog = Calculate_mean_log(mean = growth_factor_2_j, sd = growth_factor_2_sd_j),
                           sdlog = Calculate_sd_log(mean = growth_factor_2_j, sd = growth_factor_2_sd_j))
        }
      } else {  # TWO knot points
        if (t <= knot_date_1_j) {
          growth <- rlnorm(n = n_runs_j,
                           meanlog = Calculate_mean_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j),
                           sdlog = Calculate_sd_log(mean = growth_factor_1_j, sd = growth_factor_1_sd_j))
        } else if (t <= knot_date_2_j) {
          growth <- rlnorm(n = n_runs_j,
                           meanlog = Calculate_mean_log(mean = growth_factor_2_j, sd = growth_factor_2_sd_j),
                           sdlog = Calculate_sd_log(mean = growth_factor_2_j, sd = growth_factor_2_sd_j))
        } else {
          growth <- rlnorm(n = n_runs_j,
                           meanlog = Calculate_mean_log(mean = growth_factor_3_j, sd = growth_factor_3_sd_j),
                           sdlog = Calculate_sd_log(mean = growth_factor_3_j, sd = growth_factor_3_sd_j))
        }
      }
      
      # Calculate daily cases at time t and record
      inc_t <- growth*inc_tminus1
      daily_cases_sim_j[, as.character(t)] <- inc_t
      
      # Calculate cumulative cases at end of time t and record
      cum_t <- cum_tminus1 + inc_t
      cumulative_cases_end_sim_j[, as.character(t)] <- cum_t
      
    }  # (close date loop (3), t)
    
    # Bind knot-specific dataframes to full scenario dataframe
    daily_cases_sim_i <- rbind(daily_cases_sim_i, daily_cases_sim_j)
    cumulative_cases_end_sim_i <- rbind(cumulative_cases_end_sim_i, cumulative_cases_end_sim_j)
    
  }  # (close knot date loop (2), j)
  
  # Print status update
  cat('\r', paste("Summarising data from simulation", grep(country, unlist(countries_eur_lockdown)), 
                  "of", length(countries_eur_lockdown), "...    ", sep = " "))
  
  # Record summaries (mean, 2.5 and 97.5 centiles)
  ## Daily cases:
  summary_daily_cases_sim_i <- apply(X = daily_cases_sim_i, MARGIN = 2, FUN = Summarise_centiles) %>%
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country) %>% 
    relocate(Country)
  summary_daily_cases_sim[[i]] <- summary_daily_cases_sim_i
  ## Cumulative cases:
  summary_cumulative_cases_end_sim_i <- apply(X = cumulative_cases_end_sim_i, MARGIN = 2, FUN = Summarise_centiles) %>% 
    round(digits = 2) %>% t %>% as_tibble(rownames = "Date") %>% 
    mutate(Date = as.Date(Date), Country = country) %>% 
    relocate(Country)
  summary_cumulative_cases_end_sim[[i]] <- summary_cumulative_cases_end_sim_i
  
}  # (close country loop (1), k)
end <- Sys.time()
end - start  # ~ 8 mins

# Combine summary results for all countries
summary_daily_cases_sim <- bind_rows(summary_daily_cases_sim)
summary_cumulative_cases_end_sim <- bind_rows(summary_cumulative_cases_end_sim)

# Export all summary data
write_csv(summary_daily_cases_sim, path = paste0(out, "Simulation summary - daily cases.csv"))
write_csv(summary_cumulative_cases_end_sim, path = paste0(out, "Simulation summary - cumulative cases.csv"))

# Remove loop variables
rm(i, j, t, country, data_eur_i, summary_eur_i, knots_best_i,
   date_50, date_end, date_lockdown, dates, 
   daily_cases_sim_i, cumulative_cases_end_sim_i, 
   summary_daily_cases_sim_i, summary_cumulative_cases_end_sim_i, 
   prob_knots, n_knots_best_i,
   knots_best_j, n_knots, knot_date_1_j, knot_date_2_j,
   growth_factor_1_j, growth_factor_2_j, growth_factor_3_j, 
   growth_factor_1_sd_j, growth_factor_2_sd_j, growth_factor_3_sd_j,
   n_runs_j, daily_cases_sim_j, cumulative_cases_end_sim_j,
   inc_tminus1, cum_tminus1, growth, inc_t, cum_t)

# ------------------------------------------------------------------------------
# Calculate dates for which thresholds reached
# ------------------------------------------------------------------------------

# Import file containing daily cases simulation summary, if necessary
summary_daily_cases_sim <- read_csv(paste0(out, "Simulation summary - daily cases.csv"))

# Define thresholds to consider
# (proportions of total population)
thresholds <- c(0.0001, 0.00005, 0.00001)

# Create lists to store summary of thresholds
summary_thresholds <- list()  # thresholds

for (i in countries_eur_lockdown) {
  
  # Define country
  country <- i
  
  # Filter data by country
  summary_daily_cases_sim_i <- summary_daily_cases_sim %>% filter(Country == country)
  summary_eur_i <- summary_eur %>% filter(Country == country)
  
  # Record lockdown date
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)  # (date of lockdown)
  
  # Create empty table to store dates for which specified thresholds reached,
  # and number of days since lockdown
  summary_thresholds_i <- worldbank_eur %>% filter(Country == country, Year == 2019) %>%
    select(Country, Year, Population) %>% expand_grid(Threshold = thresholds) %>%
    mutate(Threshold_value = Population * Threshold,
           Threshold_exceeded = as.logical(NA),
           Date_cases_below_threshold = as.Date(NA),
           Days_since_lockdown = as.numeric(NA))
  
  # Calculate maximum number of daily cases, and first date this number was reached
  max_daily_cases_sim <- summary_daily_cases_sim_i %>% select(Mean) %>% max()
  date_max_daily_cases_sim <- summary_daily_cases_sim_i %>% filter(Mean == max_daily_cases_sim) %>% slice(1) %>% pull(Date)
  
  # Calculate and record dates for which thresholds reached
  for (k in 1:nrow(summary_thresholds_i)) {
    
    # Define threshold value
    threshold_value_k <- summary_thresholds_i %>% slice(k) %>% pull(Threshold_value)
    
    # Create T/F indicator for whether threshold value was ever exceeded & record
    threshold_exceeded_k <- max_daily_cases_sim > threshold_value_k
    summary_thresholds_i[[k, "Threshold_exceeded"]] <- threshold_exceeded_k
    
    # If threshold value was exceeded, find first date cases went below threshold
    # else, record days required since lockdown to fall below threshold as zero
    if (threshold_exceeded_k == TRUE) {
      
      # Filter dataset by dates >= date of max number of daily cases
      summary_daily_cases_sim_k <- summary_daily_cases_sim_i %>% 
        filter(Date >= date_max_daily_cases_sim)
      
      # Calculate and record first date cases fall below threshold and days since lockdown, if they exist
      date_cases_below_threshold <- summary_daily_cases_sim_k %>% 
        filter(Mean <= threshold_value_k) %>% slice(1) %>% pull(Date)
      if (length(date_cases_below_threshold) != 0) {
        summary_thresholds_i[[k, "Date_cases_below_threshold"]] <- date_cases_below_threshold
        summary_thresholds_i[[k, "Days_since_lockdown"]] <- as.numeric(date_cases_below_threshold - date_lockdown)
      }
      
    } else {
      summary_thresholds_i[[k, "Days_since_lockdown"]] <- 0
          }
    
  }  # (close loop k)
  
  # Record threshold summaries in list
  summary_thresholds[[i]] <- summary_thresholds_i
  
}

# Combine summary results for all countries
summary_thresholds <- bind_rows(summary_thresholds)

# Export summary data
write_csv(summary_thresholds, path = paste0(out, "Simulation summary - thresholds.csv"))

# Remove loop variables
rm(i, k, country, summary_daily_cases_sim_i, summary_eur_i,
   date_lockdown, summary_thresholds_i, max_daily_cases_sim, date_max_daily_cases_sim,
   threshold_value_k, threshold_exceeded_k,
   summary_daily_cases_sim_k, date_cases_below_threshold)

