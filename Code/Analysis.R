# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script examines cross-country and within-country effects of 
# lockdown timing on subsequent length of time under lockdown, 
# growth factor under lockdown, and/or total cases.

# All outputs are saved to the project directory (./Results/).

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(foreach); library(car)

# Define storage directory for formatted data
data_directory_f <- paste0("./Data/Formatted/")

# Define storage directory for results
results_directory <- paste0("./Results/")

# Load formatted data
data_eur <- read_csv(paste0(data_directory_f, "Cases_deaths_data_europe.csv"))
worldbank_eur <- read_csv(paste0(data_directory_f, "Worldbank_data_europe.csv"))

# Import files containing best knot date pairs, median growth factors, country summaries, and thresholds
knots_best <- read_csv(paste0(results_directory, "knots_best.csv"))
median_growth_factors <- read_csv(paste0(results_directory, "median_growth_factors.csv"))
summary_eur <- read_csv(paste0(results_directory, "summary_eur.csv"))
thresholds_eur <- read_csv(paste0(results_directory, "thresholds_eur.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data,
# those which entered lockdown, and those which can be modelled 
load(paste0(results_directory, "countries_eur.RData"))
load(paste0(results_directory, "countries_eur_lockdown.RData"))
load(paste0(results_directory, "countries_eur_modelled.RData"))

## Import simulated data -------------------------------------------------------

# Define filenames which contain simulated data
files <- c("summary_daily_cases_sim", "summary_cumulative_cases_end_sim", "summary_thresholds_sim")

# Import all simulation files (i.e. natural and counterfactual histories) from Results subfolders
sim_data_all <- list()
for (i in files) {
  
  # Get names of files in subfolders which contain designated simualtion results
  sim_files <- list.files(path = "./Results",
                          recursive = TRUE, 
                          pattern = paste0(i, ".csv"),
                          full.names = TRUE)
  
  # Read in all files and bind together
  sim_data_all[[i]] <- lapply(sim_files, read_csv, col_types = cols(Simulation = col_character(),
                                                                    N_days_first_restriction = col_double(),
                                                                    N_days_lockdown = col_double(),
                                                                    Date_first_restriction = col_date(),
                                                                    Date_lockdown = col_date())) %>% 
    reduce(bind_rows) %>% 
    arrange(N_days_first_restriction, N_days_lockdown)
  
}

# Append file names with '_all' and save simulation results as separate objects in global environment
names(sim_data_all) <- paste0(names(sim_data_all), "_all")
list2env(sim_data_all, globalenv()); rm(sim_data_all)

# Create dataframe for simulated cumulative cases at beginning of time t
summary_cumulative_cases_beg_sim_all <- summary_cumulative_cases_end_sim_all %>% 
  group_by(Country, History, N_days_first_restriction, N_days_lockdown) %>%
  mutate(across(c(Mean, C_025, C_975), ~lag(., n = 1, default = NA))) %>% ungroup

# Combine all daily/cumulative cases data into single dataframe,
# and remove individual files
names <- colnames(summary_daily_cases_sim_all)
col_join <- names[! names %in% c("Mean", "C_025", "C_975")]
summary_cases_sim_all <- full_join(summary_cumulative_cases_beg_sim_all,
                                   summary_daily_cases_sim_all,
                                   by = col_join,
                                   suffix = c("_cumulative_cases_beg", "_daily_cases")) %>%
  full_join(., summary_cumulative_cases_end_sim_all,
            by = col_join) %>%
  rename_at(vars(Mean, C_025, C_975), function(x) {paste0(x, "_cumulative_cases_end")})
rm(summary_cumulative_cases_beg_sim_all, summary_daily_cases_sim_all, summary_cumulative_cases_end_sim_all)

# ------------------------------------------------------------------------------
# Parameter summaries
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to calculate summary (median, Q1, Q3) of simulation parameters 
# (i.e. growth factors 1-3, lag periods between interventions and knot dates)
# Arguments:
# (1) countries = list of countries
# (2) n_decimals = number of decimals to include
# Returns table containing parameter, median, Q1, Q3, and number of observations
Calculate_Parameter_Summary <- function(countries,
                                        n_decimals = 3) {
  
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
  
  # Return summary table
  return(summary)
  
}

## Calculate parameter summaries -----------------------------------------------

# Calculate simulation parameter summaries
parameter_summary <- Calculate_Parameter_Summary(countries = countries_eur_lockdown)

# Calculate median growth factors for each country -----------------------------

# Calculate median growth factors for each country among best knots
median_growth_factors <- knots_best %>% 
  group_by(Country) %>%
  summarise(Median_growth_factor_1 = median(Growth_factor_1, na.rm = TRUE),
            Median_growth_factor_2 = median(Growth_factor_2, na.rm = TRUE),
            Median_growth_factor_3 = median(Growth_factor_3, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup

# Save all output --------------------------------------------------------------

# Save table of parameter summaries
write_csv(parameter_summary, paste0(results_directory, "parameter_summary.csv"))

# Save table of median growth factors for each country
write_csv(median_growth_factors, file = paste0(results_directory, "median_growth_factors.csv"))

# ------------------------------------------------------------------------------
# Model fit
# ------------------------------------------------------------------------------

# Evaluate model fit based on three metrics:
# (1) How well simulated model predicts time to reach thresholds
# (2) Poisson deviance for both incident and cumulative cases
# (3) Difference between total cases observed vs simulated

## Functions -------------------------------------------------------------------

# Function to calculate the first date for which observed daily cases (MA7) go below
# one or more thresholds (expressed as a proportion of the total population)
# Arguments:
# (1) country = country to calculate
# Returns: summary dataframe containing threshold values, dates cases exceeded thresholds,
# and days since first restriction / lockdown since threshold values reached
Calculate_Date_Threshold_Reached_Obs <- function(country) {
  
  # Filter summary and thresholds datasets by country
  summary_eur_country <- summary_eur %>% filter(Country == country)
  thresholds_eur_country <- thresholds_eur %>% filter(Country == country)
  
  # Record important dates
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  date_T <- summary_eur_country %>% pull(Date_T)  # final date of data included in modelling period
  
  # Filter case data by country, modelling period
  data_eur_country <- data_eur %>% filter(Country == country, Date <= date_T)
  
  # Calculate maximum mean number of daily cases (MA7), and first date this number was reached
  max_daily_cases_MA7 <- data_eur_country %>% select(Daily_cases_MA7) %>% max
  date_max_daily_cases_MA7 <- data_eur_country %>% 
    filter(Daily_cases_MA7 == max_daily_cases_MA7) %>% slice(1) %>% pull(Date)
  
  # Create empty table to store dates for which specified thresholds reached,
  # and number of days since lockdown
  summary_thresholds <- thresholds_eur_country %>% mutate(Threshold_exceeded = as.logical(NA),
                                                          Date_cases_below_threshold = as.Date(NA),
                                                          Days_since_first_restriction = as.numeric(NA),
                                                          Days_since_lockdown = as.numeric(NA))
  
  # Calculate and record dates for which thresholds reached
  for (i in 1:nrow(summary_thresholds)) {
    
    # Define threshold value
    threshold_value_i <- summary_thresholds %>% slice(i) %>% pull(Threshold_value)
    
    # Create T/F indicator for whether threshold value was ever exceeded & record
    threshold_exceeded_i <- max_daily_cases_MA7 > threshold_value_i
    summary_thresholds[[i, "Threshold_exceeded"]] <- threshold_exceeded_i
    
    # If threshold value was exceeded, find first date MA7 cases went below threshold
    # else, record days required since first restriction / lockdown to fall below threshold as zero
    if (threshold_exceeded_i == TRUE) {
      
      # Filter dataset by dates >= date of max number of daily cases
      data_eur_country_i <- data_eur_country %>% filter(Date >= date_max_daily_cases_MA7)
      
      # Calculate and record first date cases fall below threshold and days since lockdown, if they exist
      date_cases_below_threshold <- data_eur_country_i %>% 
        filter(Daily_cases_MA7 <= threshold_value_i) %>% slice(1) %>% pull(Date)
      if (length(date_cases_below_threshold) != 0) {
        summary_thresholds[[i, "Date_cases_below_threshold"]] <- date_cases_below_threshold
        summary_thresholds[[i, "Days_since_first_restriction"]] <- as.numeric(date_cases_below_threshold - date_first_restriction)
        summary_thresholds[[i, "Days_since_lockdown"]] <- as.numeric(date_cases_below_threshold - date_lockdown)
      }
    } else {
      summary_thresholds[[i, "Days_since_first_restriction"]] <- 0
      summary_thresholds[[i, "Days_since_lockdown"]] <- 0
    }
    
  }
  
  # Return summary table of thresholds
  return(summary_thresholds)
  
}

# Function to calculate the difference between the time it takes to reach 
# population-based thresholds as observed vs as simulated in natural history
# Arguments:
# (1) country = country to calculate
# Returns: summary dataframe containing country and difference (both raw and percent)
Calculate_Diff_Time_To_Threshold <- function(country) {
  
  # Filter observed and simulated summary thresholds tables
  # (do not consider thresholds that were never exceeded)
  summary_thresholds_obs_country <- summary_thresholds_obs %>%
    filter(Country == country, Days_since_lockdown != 0) %>%
    select(Threshold, Days_since_lockdown)
  summary_thresholds_sim_country <- summary_thresholds_sim_all %>% 
    filter(Country == country, Days_since_lockdown != 0, Simulation == "0,0") %>% 
    select(Threshold, Days_since_lockdown)
  
  # Join summary tables and calculate difference in time to reach thresholds
  # between observed and simulated
  time_to_thresholds_natural_history <-
    full_join(summary_thresholds_obs_country, summary_thresholds_sim_country,
              by = "Threshold", 
              suffix = c("_obs", "_sim")) %>%
    mutate(Diff = as.numeric(Days_since_lockdown_obs - Days_since_lockdown_sim),
           Pct_diff = Diff / Days_since_lockdown_obs) %>%
    select(-contains("Days")) %>%
    rename(Number = Diff, Pct = Pct_diff) %>%
    pivot_longer(cols = c(Number, Pct), names_to = "Type", values_to = "Value") %>%
    mutate(Country = country, Measure = "Diff_time_to_threshold") %>%
    relocate(Country, Measure) 
  
  # Return dataframe 
  return(time_to_thresholds_natural_history)
  
}

# Function to calculate the difference between total cases at the end of the
# simulation period (date_T) as observed vs as simulated in natural history
# Arguments:
# (1) country = country to calculate
# Returns: summary dataframe containing country and difference (both raw and percent)
Calculate_Diff_Total_Cases <- function(country) {
  
  # Filter cases and summary data by specified country
  summary_eur_country <- summary_eur %>% filter(Country == country)
  data_eur_country <- data_eur %>% filter(Country == country)
  
  # Filter simulated data by specified country, natural history
  summary_cases_sim_country <- summary_cases_sim_all %>% 
    filter(Country == country, History == "Natural history")
  
  # Get simulation end date
  date_T <- summary_eur_country %>% pull(Date_T)
  
  # Calculate total observed and simulated cases on date_T, and difference
  cases_T_obs <- data_eur_country %>% filter(Date == date_T) %>% pull(Cumulative_cases_end)
  cases_T_sim <- summary_cases_sim_country %>% filter(Date == date_T) %>% pull(Mean_cumulative_cases_end)
  diff_cases_T <- cases_T_obs - cases_T_sim
  pct_diff_cases_T <- diff_cases_T / cases_T_obs
  
  # Return difference
  return(tibble(Country = country,
                Measure = "Diff_total_cases",
                Type = c("Number", "Pct"), 
                Value = c(diff_cases_T, pct_diff_cases_T)))
}

# Function to calculate the Poisson deviance between the observed 
# incident and cumulave cases (MA7) and the mean simulated incident and cumulative cases
# Arguments:
# (1) country = country to calculate
# Returns: summary dataframe containing country, Poisson deviance of incident cases,
# and Poisson deviance of cumulative cases
Calculate_Pois_Dev_Natural_History <- function(country) {
  
  # Filter summary data by specified country
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Get simulation start and end dates
  date_start <- summary_eur_country %>% pull(Date_start)
  date_T <- summary_eur_country %>% pull(Date_T)
  
  # Filter observed cases data by specified country and simulation period
  data_eur_country <- data_eur %>% 
    filter(Country == country, Date >= date_start, Date <= date_T)
  
  # Filter simulated cases data by specified country, simulation period, and natural history
  summary_cases_sim_country <- summary_cases_sim_all %>% 
    filter(Country == country, Date >= date_start, Date <= date_T,
           History == "Natural history")
  
  # Calculate and record Poisson deviance
  ## (1) For predicted vs true (7-day moving average) incident cases
  true_inc <- data_eur_country %>% pull(Daily_cases_MA7)
  pred_inc <- summary_cases_sim_country %>% pull(Mean_daily_cases)
  pois_dev_inc <- Calc_Pois_Dev(obs = true_inc, sim = pred_inc)
  ## (2) For predicted vs true (7-day moving average) cumulative cases
  true_cum <- data_eur_country %>% pull(Cumulative_cases_end_MA7) 
  pred_cum <- summary_cases_sim_country %>% pull(Mean_cumulative_cases_end)
  pois_dev_cum <- Calc_Pois_Dev(obs = true_cum, sim = pred_cum)
  
  # Return dataframe of Poisson deviance for both incident and cumulative cases
  return(tibble(Country = country,
                Measure = c("Pois_dev_inc", "Pois_dev_cum"),
                Type = "Number",
                Value = c(pois_dev_inc, pois_dev_cum)))
  
}

# Function to calculate Poisson deviance between two vectors
# (from: https://en.wikipedia.org/wiki/Deviance_(statistics))
# Arguments: 
# (1) obs = vector of observed values
# (2) sim = vector of simulated/predicted values
Calc_Pois_Dev <- function(obs, sim) {
  
  #D <- 2 * sum(obs * log(obs / sim) - (obs - sim))
  D <- 2 * sum(obs * log(obs / sim) - (obs - sim), na.rm = TRUE)
  return(D)
  
}

## Summarise observed vs simulated time to thresholds --------------------------

# Calculate dates for which thresholds reached in observed data
summary_thresholds_obs <- foreach(i = countries_eur, .errorhandling = "pass") %do%
  Calculate_Date_Threshold_Reached_Obs(country = i) %>%
  bind_rows

# Define countries for which natural history was simulated
countries <- summary_cases_sim_all %>% filter(History == "Natural history") %>% 
  pull(Country) %>% unique %>% as.list

# Calculate difference between observed and simulated time to reach thresholds
diff_time_to_thresholds <- foreach(i = countries, 
                                   .errorhandling = "pass") %do%
  Calculate_Diff_Time_To_Threshold(country = i) %>%
  bind_rows

# Calculate summary statistics for difference in time to thresholds
diff_time_to_thresholds_summary <- diff_time_to_thresholds %>%
  group_by(Measure, Threshold, Type) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            IQR = IQR(Value, na.rm = TRUE),
            N_countries = sum(!is.na(Value)),
            .groups = "keep")

## Difference in total cases ---------------------------------------------------

# Calculate difference in total cases for all modelled countries
diff_total_cases <- foreach(i = countries, 
                            .errorhandling = "pass") %do%
  Calculate_Diff_Total_Cases(country = i) %>%
  bind_rows 

# Calculate summary statistics for difference in total cases
diff_total_cases_summary <- diff_total_cases %>%
  group_by(Measure, Type) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            IQR = IQR(Value, na.rm = TRUE),
            N_countries = sum(!is.na(Value)),
            .groups = "keep")

## Poisson deviance ------------------------------------------------------------

# Calculate Poisson deviance between observed and simulated natural histories
pois_dev_natural_history <- foreach(i = countries, 
                                    .errorhandling = "pass") %do%
  Calculate_Pois_Dev_Natural_History(country = i) %>%
  bind_rows

# Calculate summary statistics for Poisson deviance
pois_dev_natural_history_summary <- pois_dev_natural_history %>%
  group_by(Measure, Type) %>%
  summarise(Mean = mean(Value, na.rm = TRUE),
            SD = sd(Value, na.rm = TRUE),
            Median = median(Value, na.rm = TRUE),
            IQR = IQR(Value, na.rm = TRUE),
            N_countries = sum(!is.na(Value)),
            .groups = "keep")

## Combine and save all model fit data -----------------------------------------

# Combine all model fit statistics into single dataframe
model_fit <- diff_time_to_thresholds %>% 
  full_join(., diff_total_cases) %>%
  full_join(., pois_dev_natural_history) %>%
  arrange(Country) %>%
  relocate(Threshold, .after = last_col())

# Combine all model fit summary statistics into single dataframe
model_fit_summary <- diff_time_to_thresholds_summary %>%
  full_join(., diff_total_cases_summary) %>%
  full_join(., pois_dev_natural_history_summary)

# Save both model fit dataframes
write_csv(model_fit, paste0(results_directory, "model_fit.csv"))
write_csv(model_fit_summary, paste0(results_directory, "model_fit_summary.csv"))

## Define countries to exclude from subsequent analyses ------------------------

# Define countries to exclude from analyses
countries_excluded_all <- c("Russia")  # exclude from all analyses
countries_excluded_length_lockdown <- countries_excluded_all
countries_excluded_time_to_threshold <- c(countries_excluded_all, "San Marino")
countries_excluded_total_cases <- countries_excluded_all

# Save lists of excluded countries
save(countries_excluded_all, 
     file = paste0(results_directory, "countries_excluded_all.RData"))
save(countries_excluded_length_lockdown, 
     file = paste0(results_directory, "countries_excluded_length_lockdown.RData"))
save(countries_excluded_time_to_threshold, 
     file = paste0(results_directory, "countries_excluded_time_to_threshold.RData"))
save(countries_excluded_total_cases, 
     file = paste0(results_directory, "countries_excluded_total_cases.RData"))

# ------------------------------------------------------------------------------
# Analysis: lockdown timing - cross-country
# ------------------------------------------------------------------------------

# Effects of interest:
# (1) Effect of lockdown timing (i.e. cases at lockdown) on length of lockdown
# (2) Effect of lockdown timing (i.e. cases at lockdown) on growth factor under lockdown
# All may be adjusted or unadjusted effects

## Functions -------------------------------------------------------------------

# Function to estimate the effect of cases at lockdown on length of time under lockdown 
# and median growth factor under lockdown 
# (unadjusted and adjusted effects, logged and unlogged exposures/covariates)
# Arguments:
# (1) countries = list of countries to include in analysis
# (2) outcomes = vector outcomes 
# (3) exposures_trans = multilevel list containing exposures (different measures of cases)
##### and all potential transformations of each exposure to consider
# (4) covariates_trans = multilevel list containing covariates to adjust for
##### and all potential transformations of each covariate
# Returns: two dataframes: (1) all adjusted and unadjusted effects for each 
# combination of exposure and outcome; (2) effects only from best-fitting models (by BIC)
Estimate_Effects_Between_Countries <- 
  function(countries, 
           outcomes = c("Length_lockdown", "Median_growth_factor_lockdown"),
           exposures_trans = list(list("Exposure" = "Daily_cases_MA7",
                                       "Transformation" = c("Daily_cases_MA7", "log(Daily_cases_MA7)")),
                                  list("Exposure"= "Cumulative_cases_beg",
                                       "Transformation" = c("Cumulative_cases_beg", "log(Cumulative_cases_beg)"))),
           covariates_trans = list(list("Covariate" = "Area_sq_km",
                                        "Transformation" = c("Area_sq_km", "log(Area_sq_km)", "I(1/Area_sq_km)")),
                                   list("Covariate"= "Population",
                                        "Transformation" = c("Population", "log(Population)")))) {
    
    # Define exposures and covariates
    exposures <- exposures_trans %>% map(., .f = ~.x$Exposure) %>% unlist
    covariates <- covariates_trans %>% map(., .f = ~.x$Covariate) %>% unlist
    
    # Get data for all outcomes, exposures, and covariates for designated countries
    ## Exposures: Cases on lockdown dates
    data_cases <- data_eur %>% filter(Country %in% countries) %>% 
      left_join(., select(summary_eur, c(Country, Date_lockdown)), by = "Country") %>% 
      filter(Date == Date_lockdown) %>%
      select(Country, Date_lockdown, all_of(exposures))
    ## Covariates: Country stats
    data_covariates <- summary_eur %>% filter(Country %in% countries) %>% 
      select(Country, all_of(covariates)) 
    ## Outcomes
    data_length_lockdown <- summary_eur %>% filter(Country %in% countries) %>% 
      select(Country, Length_lockdown)
    data_growth_factors <- median_growth_factors %>% 
      filter(Country %in% countries) %>% 
      group_by(Country) %>% 
      summarise(Median_growth_factor_lockdown = ifelse(!is.na(Median_growth_factor_3),
                                                       Median_growth_factor_3, Median_growth_factor_2),
                .groups = "keep") %>% ungroup
    
    # Combine all data for modelling into single dataframe, remove individual datasets
    data_model <- data_cases %>% 
      full_join(., data_covariates, by = "Country") %>%
      full_join(., data_length_lockdown, by = "Country") %>%
      full_join(., data_growth_factors, by = "Country") %>%
      mutate(ID = rownames(.))
    
    # Determine all possible combinations of covariate transformations
    covariate_combinations <- covariates_trans %>% 
      map(., .f = ~.x$Transformation) %>%
      expand.grid %>%
      unite(., col = "Combination", sep = ", ", remove = TRUE) %>%
      pull(Combination)
    
    # Create grid with all combinations of exposure, outcome, and covariates,
    # and define formula
    grid <- expand_grid(Outcome = outcomes, 
                        Exposure = map(.x = exposures_trans, .f = ~.x$Transformation) %>% unlist, 
                        Covariates = c(NA, covariate_combinations)) %>%
      mutate(Independent_vars = ifelse(!is.na(Covariates), 
                                       paste0(Exposure, ", ", Covariates),
                                       Exposure),
             Formula = paste(Outcome, " ~ ", gsub(", ", " + ", Independent_vars))) %>%
      select(-Independent_vars)
    
    # Evaluate each formula
    models <- map(.x = grid$Formula, .f = ~as.formula(.x)) %>%
      map(~lm(.x, data = data_model))
    
    # Pull estimated effects and CI bounds from each formula
    effects <- map(.x = models, 
                   .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                                "CI_lower" = confint(.x)[2, 1],
                                "CI_upper" = confint(.x)[2, 2],
                                "R_squared" = summary(.x)$r.squared,
                                "BIC" = BIC(.x),
                                "N_countries" = length(summary(.x)$residuals))) %>% 
      reduce(bind_rows) %>%
      mutate(Leverage_points = "Included") %>%
      bind_cols(grid, .) %>% select(-Formula)
    
    # Identify data points with high leverage
    leverage <- map(.x = models, .f = ~car::influencePlot(.x)) %>%
      map(.f = ~tibble(ID = (rownames(.x))))
    
    # Create vector of countries with high leverage
    leverage_countries <- leverage %>%
      map(., .f = ~left_join(.x, data_model, by = "ID")) %>%
      map(., .f = ~pull(.x, Country)) %>%
      map(., .f = ~paste(.x, collapse = ", "))
    
    # Re-run models without points of high leverage
    models_no_leverage <- leverage %>%
      map(., .f = ~anti_join(data_model, .x, by = "ID")) %>%
      map2(.y = grid$Formula, .f = ~lm(.y, data = .x)) 
    
    # Pull estimated effects and CI bounds from each formula
    effects_no_leverage <- map(.x = models_no_leverage, 
                               .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                                            "CI_lower" = confint(.x)[2, 1],
                                            "CI_upper" = confint(.x)[2, 2],
                                            "R_squared" = summary(.x)$r.squared,
                                            "BIC" = BIC(.x),
                                            "N_countries" = length(summary(.x)$residuals))) %>% 
      map2(.y = leverage_countries, 
           .f = ~mutate(.x, Leverage_points_excluded = .y)) %>%
      reduce(bind_rows) %>%
      mutate(Leverage_points = "Excluded") %>%
      relocate(Leverage_points, .before = Leverage_points_excluded) %>%
      bind_cols(grid, .) %>% select(-Formula)
    
    # Bind all estimated effects together
    all_effects <- bind_rows(effects, effects_no_leverage) %>%
      arrange(Outcome, Exposure, Covariates)
    
    # Find best-fitting (adjusted) models for each combination of exposure and outcome,
    # and bind with corresponding unadjusted models and models without leverage points
    best_effects <- map(.x = as.list(exposures),
                        .f = ~filter(effects, str_detect(Exposure, .x))) %>%
      map(., .f = ~filter(., !is.na(Covariates))) %>%
      map(., .f = ~group_by(., Outcome)) %>%
      map(., .f = ~filter(., BIC == min(BIC))) %>%
      reduce(bind_rows) %>%
      split(., seq(nrow(.))) %>%
      map(., .f = ~select(., c(Outcome, Exposure, Covariates))) %>%
      map(., .f = ~bind_rows(., tibble(Outcome = .x$Outcome,
                                       Exposure = .x$Exposure,
                                       Covariates = NA))) %>%
      map(., .f = ~left_join(., all_effects,
                             by = c("Outcome", "Exposure", "Covariates"))) %>%
      reduce(bind_rows) %>%
      arrange(Outcome)
    
    # Return list of all estimated effects
    return(list(all_effects = all_effects,
                best_effects = best_effects))
    
  }

## Estimate between-country effects --------------------------------------------

# Define countries to include in analysis
countries <- countries_eur_lockdown

# Estimate between-country effects
effects_between_countries <- Estimate_Effects_Between_Countries(countries = countries,
                                                                outcomes = "Length_lockdown")

# Split list of effects into two dataframes containg effects from all models 
# and from only best models
effects_between_countries_all <- effects_between_countries[[1]]
effects_between_countries_best <- effects_between_countries[[2]]

# Save results
write_csv(effects_between_countries_all, 
          file = paste0(results_directory, "effects_between_countries_all.csv"))
write_csv(effects_between_countries_best, 
          file = paste0(results_directory, "effects_between_countries_best.csv"))

# ------------------------------------------------------------------------------
# Analysis: lockdown timing - within-country
# ------------------------------------------------------------------------------

# Effects of interest:
# (1) Effect of lockdown timing on length of time under lockdown
# (2) Effect of lockdown timing on time to reach population-based thresholds
# (3) Effect of lockdown timing on total cases at end of simulation period

## Functions -------------------------------------------------------------------

# Function to calculate the length of lockdown for all countries simulated,
# for a designated counterfactual simulation
# Arguments:
# (1) simulation = counterfacual simulation 
# Returns dataframe containing length of lockdown for all countries simulated in designated simulation
Calculate_Length_Lockdown_Sim <- function(simulation) {
  
  # Calculate mean simulation incident cases on date of easing under natural history simulation
  lockdown_eased_thresholds_sim <- summary_eur %>% 
    select(Country, Date_eased) %>%
    full_join(., summary_cases_sim_all, by = "Country") %>%
    filter(History == "Natural history", Date == Date_eased) %>%
    select(Country, Lockdown_eased_threshold = Mean_daily_cases)
  
  # Label simulation as natural or counterfactual history
  history <- ifelse(simulation == "0,0", 
                    "Natural history", "Counterfactual history")
  
  # Filter dataframe of simulated data by specified simulation
  summary_cases_sim_all_filt <- summary_cases_sim_all %>% filter(Simulation == simulation)
  
  # Create list of countries which were simulated
  countries_sim <- summary_cases_sim_all_filt %>% pull(Country) %>% unique %>% as.list
  
  # Calculate length of lockdown for all simulated countries
  summary_length_lockdown_sim <- foreach(i = countries_sim, .errorhandling = "pass") %do%
    Calculate_Length_Lockdown_Sim_Country(country = i,
                                          thresholds = lockdown_eased_thresholds_sim,
                                          data_sim = summary_cases_sim_all_filt) %>%
    reduce(bind_rows) %>%
    mutate(Simulation = simulation, History = history) %>%
    relocate(c(Simulation, History), .after = Country)
  
  # Return summary table of length of lockdown for all simulated countries
  return(summary_length_lockdown_sim)
  
}

# Function to calculate the first date for which mean simulated incidence data 
# for a specified country goes below mean simulated incidence
# on the date lockdown was eased in the natural history simulation
# Arguments:
# (1) country = country to calculate
# (2) thresholds = dataframe containing all values for mean simulated incidence on date of easing (natural history)
# (3) data_sim = dataframe containing simulated data
# Returns: dataframe containing length of lockdown for specified country
Calculate_Length_Lockdown_Sim_Country <- function(country, thresholds, data_sim) {
  
  # Filter simulated data and thresholds dataframes by country
  data_sim_country <- data_sim %>% filter(Country == country)
  thresholds_country <- thresholds %>% filter(Country == country)
  
  # Record description of simulation
  description <- data_sim_country %>%
    select(Country, N_days_first_restriction, N_days_lockdown,
           Date_first_restriction, Date_lockdown) %>% unique
  
  # Record counterfactual dates of first restrition and lockdown
  date_first_restriction_counterfactual <- data_sim_country %>% pull(Date_first_restriction) %>% unique
  date_lockdown_counterfactual <- data_sim_country %>% pull(Date_lockdown) %>% unique
  
  # Get mean daily cases on date lockdown was eased by country
  lockdown_eased_threshold_country <- thresholds_country %>%
    pull(Lockdown_eased_threshold)
  
  # Calculate maximum mean number of simulated daily cases, and first date this number was reached
  max_daily_cases_sim <- data_sim_country %>% select(Mean_daily_cases) %>% max
  date_max_daily_cases_sim <- data_sim_country %>% 
    filter(Mean_daily_cases == max_daily_cases_sim) %>% slice(1) %>% pull(Date)
  
  # Create T/F indicator for whether lockdown threshold was ever exceeded
  threshold_exceeded <- max_daily_cases_sim > lockdown_eased_threshold_country
  
  # If threshold value was exceeded, find first date cases went below threshold
  # else, record days required since first restriction / lockdown to fall below threshold as zero
  if (threshold_exceeded == TRUE) {
    
    # Filter dataset by dates >= date of max number of daily cases
    data_sim_country_filt <- data_sim_country %>% 
      filter(Date >= date_max_daily_cases_sim)
    
    # Calculate first date cases fall below threshold and days since lockdown, if they exist
    date_cases_below_threshold <- data_sim_country_filt %>% 
      filter(Mean_daily_cases <= lockdown_eased_threshold_country) %>% slice(1) %>% pull(Date)
    if (length(date_cases_below_threshold) != 0) {
      days_since_first_restriction <- as.numeric(date_cases_below_threshold - date_first_restriction_counterfactual)
      days_since_lockdown <- as.numeric(date_cases_below_threshold - date_lockdown_counterfactual)
    } else {
      date_cases_below_threshold <- days_since_first_restriction <- days_since_lockdown <- NA
    }
    
  } else {
    date_cases_below_threshold <- NA
    days_since_first_restriction <- days_since_lockdown <- 0
  }
  
  # Record all summary data
  summary_length_lockdown_sim_country <- description %>%
    full_join(., thresholds_country, by = "Country") %>%
    mutate(Threshold_exceeded = threshold_exceeded,
           Date_cases_below_threshold = as.Date(date_cases_below_threshold),
           Days_since_first_restriction = as.numeric(days_since_first_restriction),
           Days_since_lockdown = as.numeric(days_since_lockdown))
  
  # Return summary table of length of lockdown
  return(summary_length_lockdown_sim_country)
  
}

# Function to esimate between-country effects: percentage change in
# (1) length of lockdown, (2) time to thresholds, and (3) total cases
# Arguments:
# (1) countries = list of countries to include in analysis
# (2) outcomes = vector of outcomes to return
# Returns: list of 2 dataframes: effects within countries, and median effects
Estimate_Effects_Within_Countries <- function(countries,
                                              outcomes = c("pct_change_length_lockdown",
                                                           "pct_change_time_to_threshold",
                                                           "pct_change_cases")) {
  
  # Estimate percentage change in length of lockdown, median effect
  if ("pct_change_length_lockdown" %in% outcomes) {
    pct_change_length_lockdown <- 
      Calculate_Pct_Change_Length_Lockdown(countries = countries[!countries %in% countries_excluded_length_lockdown])
  }
  
  # Estimate percentage change in time to reach thresholds, median effect
  if ("pct_change_time_to_threshold" %in% outcomes) {
    pct_change_time_to_threshold <-
      Calculate_Pct_Change_Time_To_Threshold(countries = countries[!countries %in% countries_excluded_time_to_threshold])
  }
  
  # Estimate percentage change in total cases
  if ("pct_change_cases" %in% outcomes) {
    pct_change_cases <- 
      Calculate_Pct_Change_Total_Cases(countries = countries[!countries %in% countries_excluded_total_cases])
  }
  
  # Combine all estimated effects into list
  effects_all <- map(.x = outcomes, .f = ~eval(parse(text = .x)))
  
  # Combine all within-country effects
  effects_within_countries <- map(.x = effects_all,
                                  .f = ~.x$pct_change) %>% 
    reduce(full_join) %>%
    arrange(Country, Simulation)
  
  # Combine all within-country mean effects
  effects_within_countries_summary <- map(.x = effects_all,
                                          .f = ~.x$pct_change_summary) %>% 
    reduce(full_join) %>%
    relocate(N_countries, .after = last_col()) %>%
    relocate(any_of("Threshold"), .after = Outcome) %>%
    arrange(Simulation)
  
  # Return list of effects
  return(list(effects_within_countries = effects_within_countries,
              effects_within_countries_summary = effects_within_countries_summary))
  
}

# Function to estimate the percentage change in the length of lockdown
# for various counterfactual histories, compared to the natural history 
# Arguments:
# (1) countries = list of countries to estimate
# Returns: list of two dataframes: (1) percentage change in length of lockdown compared to natural history,
### and (2) median percentage change for each counterfactual history
Calculate_Pct_Change_Length_Lockdown <- function(countries) {
  
  # Filter dataframe containing lengths of lockdown by designated countries
  summary_length_lockdown_sim_countries <- summary_length_lockdown_sim %>% 
    filter(Country %in% countries) %>%
    select(Country:History, Length_lockdown = Days_since_lockdown)
  
  # Calculate length of lockdown under natural history 
  length_lockdown_natural_history <- summary_length_lockdown_sim_countries %>% 
    filter(Simulation == "0,0") %>%
    select(Country, Length_lockdown_natural_history = Length_lockdown)
  
  # Calculate percent change in length of lockdown compared to natural history, label
  pct_change_length_lockdown <- summary_length_lockdown_sim_countries %>% 
    full_join(., length_lockdown_natural_history, by = "Country") %>%
    mutate(Pct_change = 
             ((Length_lockdown - Length_lockdown_natural_history) / Length_lockdown_natural_history)) %>%
    select(-contains("Length")) %>%
    mutate(Outcome = "Length_lockdown") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Simulation)
  
  # Calculate median percent change, quartiles
  pct_change_length_lockdown_summary <- pct_change_length_lockdown %>%
    group_by(Simulation, History, Outcome) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = as.numeric(n()),
              .groups = "keep") %>% ungroup
  
  # Return dataframes
  return(list("pct_change" = pct_change_length_lockdown,
              "pct_change_summary" = pct_change_length_lockdown_summary))
  
}

# Function to estimate the percentage change in the number of days since lockdown
# required to reach population-based threshold values for various counterfactual histories, 
# compared to the natural history 
# Arguments:
# (1) countries = list of countries to estimate
# Returns: list of two dataframes: (1) percentage change in time to thresholds compared to natural history,
### and (2) median percentage change for each counterfactual history
Calculate_Pct_Change_Time_To_Threshold <- function(countries) {
  
  # Filter thresholds table by given country and population-based thresholds
  summary_thresholds_sim_countries <- summary_thresholds_sim_all %>% 
    filter(Country %in% countries) %>%
    select(Country:History, Threshold, Days_since_lockdown)
  
  # Calculate number of days to reach thresholds in natural history
  days_since_lockdown_natural_history <- summary_thresholds_sim_countries %>% 
    filter(History == "Natural history") %>%
    select(Country, Threshold, Days_since_lockdown_natural_history = Days_since_lockdown) 
  
  # Calculate percent change in days to reach thresholds from natural history, label
  pct_change_time_to_threshold <- summary_thresholds_sim_countries %>%
    full_join(., days_since_lockdown_natural_history, by = c("Country", "Threshold")) %>%
    mutate(Pct_change = 
             ((Days_since_lockdown - Days_since_lockdown_natural_history) / Days_since_lockdown_natural_history)) %>%
    select(-contains("Days")) %>%
    mutate(Outcome = "Time_to_threshold") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Country)
  
  # Calculate median percent change, quartiles
  pct_change_time_to_threshold_summary <- pct_change_time_to_threshold %>% 
    group_by(Simulation, History, Outcome, Threshold) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = n(),
              .groups = "keep") %>% ungroup
  
  # Return dataframes
  return(list("pct_change" = pct_change_time_to_threshold,
              "pct_change_summary" = pct_change_time_to_threshold_summary))
  
}

# Function to estimate the percentage change in the number of cumulative cases
# for various counterfactual histories, compared to the natural history 
# Arguments:
# (1) countries = list of countries to estimate
# Returns: list of two dataframes: (1) percentage change in total cases compared to natural history,
### and (2) median percentage change for each counterfactual history
Calculate_Pct_Change_Total_Cases <- function(countries) {
  
  # Filter summary dataframe by countries
  summary_eur_countries <- summary_eur %>% filter(Country %in% countries)
  
  # Record final date of cases included in analysis (date_T)
  date_T <- summary_eur_countries %>% select(Country, Date_T)
  
  # Filter simulated data by country, join with date_T
  summary_cases_sim_countries <- summary_cases_sim_all %>% 
    filter(Country %in% countries) 
  
  # Create summary table of cumulative cases on date_T
  summary_cases_sim_countries_T <- summary_cases_sim_countries %>%
    full_join(., date_T, by = "Country") %>%
    filter(Date == Date_T) %>%
    select(Country:History, Mean_cumulative_cases_end)
  
  # Calculate cumulative cases on date_T in natural history
  cases_natural_history <- summary_cases_sim_countries_T %>% 
    filter(History == "Natural history") %>% 
    select(Country, Mean_cumulative_cases_end_natural_history = Mean_cumulative_cases_end)
  
  # Calculate percent change in cases from natural history, label
  pct_change_cases <- summary_cases_sim_countries_T %>%
    full_join(., cases_natural_history, by = "Country") %>%
    mutate(Pct_change = 
             ((Mean_cumulative_cases_end - Mean_cumulative_cases_end_natural_history) / 
                Mean_cumulative_cases_end_natural_history)) %>%
    select(-contains("Mean")) %>%
    mutate(Outcome = "Total_cases") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Country)
  
  # Calculate median percent change, quartiles
  pct_change_cases_summary <- pct_change_cases %>% 
    group_by(Simulation, History, Outcome) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = n(),
              .groups = "keep") %>% ungroup
  
  # Return dataframe
  return(list("pct_change" = pct_change_cases,
              "pct_change_summary" = pct_change_cases_summary))
  
}

## Estimate length of lockdown across simulations ------------------------------

# Define simulations
simulations <- summary_cases_sim_all %>% pull(Simulation) %>% unique

# For each counterfactual history, calculate length of lockdown for all countries
summary_length_lockdown_sim <- foreach(i = simulations, .errorhandling = "pass") %do%
  Calculate_Length_Lockdown_Sim(simulation = i) %>%
  reduce(bind_rows) %>%
  arrange(Country, N_days_first_restriction, N_days_lockdown)

# Export summary table of length of lockdown
write_csv(summary_length_lockdown_sim, paste0(results_directory, "summary_length_lockdown_sim.csv"))

## Estimate within-country effects ---------------------------------------------

# Define countries to include in analysis
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded_all]

# Define outcomes to include in analysis
outcomes <- c("pct_change_length_lockdown", "pct_change_time_to_threshold", "pct_change_cases")

# Estimate within-country effects
effects_within_countries_all <- 
  Estimate_Effects_Within_Countries(countries = countries,
                                    outcomes = outcomes)

# Save within-country individual and median effects as separate dataframes
effects_within_countries <- effects_within_countries_all$effects_within_countries
effects_within_countries_summary <- effects_within_countries_all$effects_within_countries_summary

# Save both effects dataframes
write_csv(effects_within_countries, paste0(results_directory, "effects_within_countries.csv"))
write_csv(effects_within_countries_summary, paste0(results_directory, "effects_within_countries_summary.csv"))

