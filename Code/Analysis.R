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
library(tidyverse); library(foreach)

# Define storage directory for formatted data
data_directory_f <- paste0("./Data/Formatted/")

# Define storage directory for results
results_directory <- paste0("./Results/")

# Load formatted data
data_eur <- read_csv(paste0(data_directory_f, "Cases_deaths_data_europe.csv"))
worldbank_eur <- read_csv(paste0(data_directory_f, "Worldbank_data_europe.csv"))

# Import files containing best knot date pairs, median growth factors, and country summaries
knots_best <- read_csv(paste0(results_directory, "knots_best.csv"))
median_growth_factors <- read_csv(paste0(results_directory, "median_growth_factors.csv"))
summary_eur <- read_csv(paste0(results_directory, "summary_eur.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data,
# those which entered lockdown, and those which can be modelled 
load(paste0(results_directory, "countries_eur.RData"))
load(paste0(results_directory, "countries_eur_lockdown.RData"))
load(paste0(results_directory, "countries_eur_modelled.RData"))

# Define countries to exclude from analysis
countries_excluded <- c("Russia", "San Marino")

## Import simulated data -------------------------------------------------------

# Define filenames which contain simulated data
files <- c("summary_daily_cases_sim", "summary_cumulative_cases_end_sim", "summary_thresholds")

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
                                                                    N_days_lockdown = col_double())) %>% 
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
# Analysis: lockdown timing - cross-country
# ------------------------------------------------------------------------------

# Effects of interest:
# (1) Effect of cases at lockdown on length of time under lockdown
# (2) Effect of cases at lockdown on growth factor under lockdown
# Both may be adjusted or unadjusted effects

## Data preparation-------------------------------------------------------------

# Define countries, exposures, and covariates to examine
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded]
exposures <- c("Cumulative_cases_beg", "Cumulative_cases_beg_MA7", "Daily_cases", "Daily_cases_MA7")
covariates <- c("Area_sq_km", "Population")

# Get data required for modelling
## Lockdown dates
data_lockdown <- summary_eur %>% 
  filter(Country %in% countries) %>% 
  select(Country, Date_lockdown)
## Cases on lockdown dates
data_cases <- data_eur %>% 
  filter(Country %in% countries) %>% 
  full_join(., data_lockdown, by = "Country") %>% 
  filter(Date == Date_lockdown) %>%
  select(Country, Date_lockdown, all_of(exposures))
## Country areas and population
data_area <- worldbank_eur %>% 
  filter(Country %in% countries) %>% 
  filter(Year == 2018) %>% select(Country, Area_sq_km)
data_population <- worldbank_eur %>% 
  filter(Country %in% countries) %>% 
  filter(Year == 2018) %>% select(Country, Population)
## Thresholds, and days since lockdown to reach threshold
data_thresholds <- summary_thresholds_all %>% 
  filter(Country %in% countries) %>% 
  filter(History == "Natural history") %>%
  select(Country, Threshold, Days_since_lockdown)
## Median growth factor under lockdown
data_growth_factors <- median_growth_factors %>% 
  filter(Country %in% countries) %>% 
  group_by(Country) %>% 
  summarise(Median_growth_factor_lockdown = ifelse(!is.na(Median_growth_factor_3),
                                                   Median_growth_factor_3, Median_growth_factor_2),
            .groups = "keep") %>% ungroup

# Combine all data for modelling into single dataframe, remove individual datasets
data_model <- data_cases %>% 
  full_join(., data_area, by = "Country") %>%
  full_join(., data_population, by = "Country") %>%
  full_join(., data_thresholds, by = "Country") %>%
  full_join(., data_growth_factors, by = "Country")
rm(data_lockdown, data_cases, data_area, data_thresholds, data_growth_factors)

## Functions -------------------------------------------------------------------

# Function to estimate the effect of cases at lockdown on length of time under lockdown 
# (i.e. number of days from lockdown required for incident cases to go below given threshold)
# Arguments:
# (1) outcome = number of days since lockdown required to reach a threshold
# (2) exposures = vector of exposures (different measures of cases)
# (3) covariates = vector of covariates to adjust for
# (4) data = dataframe containing data for all exposures, covariates, outcome, and threshold
# (5) threshold = threshold to consider
# Returns: table of adjusted and unadjusted effects for each exposure on the outcome, for the given threshold
Estimate_effect_days_to_threshold <- function(outcome = "Days_since_lockdown", 
                                              exposures = c("Cumulative_cases_beg", 
                                                            "Cumulative_cases_beg_MA7", 
                                                            "Daily_cases", 
                                                            "Daily_cases_MA7"), 
                                              covariates, data, threshold) {
  
  # Select relevant variables from input data and filter by defined threshold
  data_i <- data %>% select(Country, all_of(outcome), all_of(exposures), all_of(covariates), Threshold) %>%
    filter(Threshold == threshold)
  
  # Create grid with all combinations of exposure, outcome, and covariates,
  # and define formula
  grid <- expand_grid(Outcome = outcome, 
                      Exposure = exposures, 
                      Covariates = c(NA, paste(covariates, collapse = ", "))) %>%
    mutate(Independent_vars = ifelse(!is.na(Covariates), 
                                     paste0(Exposure, ", ", Covariates),
                                     Exposure),
           Formula = paste(Outcome, " ~ ", gsub(", ", " + ", Independent_vars))) %>%
    select(-Independent_vars)
  
  # Evaluate each formula
  models <- map(.x = grid$Formula, .f = ~as.formula(.x)) %>%
    map(~lm(.x, data = data_i))
  
  # Pull estimated effects and CI bounds from each formula
  effects <- map(.x = models, 
                 .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                              "CI_lower" = confint(.x)[2, 1],
                              "CI_upper" = confint(.x)[2, 2],
                              "N_countries" = length(summary(.x)$residuals))) %>% 
    reduce(bind_rows)
  
  # Bind estimated effects to grid
  grid <- bind_cols(grid, effects) %>% select(-Formula)
  
  # Label grid of estimated effects with thresholds
  grid <- grid %>% mutate(Threshold = threshold)
  
  # Return grid
  return(grid)
  
}

# Function to estimate the effect of cases at lockdown on growth factor under lockdown
# Arguments:
# (1) outcome = median growth factor under lockdown
# (2) exposures = vector of exposures (different measures of cases)
# (3) covariates = vector of covariates to adjust for
# (4) data = dataframe containing data for all exposures, covariates, outcome
# Returns: table of adjusted and unadjusted effects for each exposure on the outcome
Estimate_effect_growth_factor_lockdown <- function(outcome = "Median_growth_factor_lockdown", 
                                                   exposures, covariates, data) {
  
  # Select relevant variables from input data
  data_i <- data %>% select(Country, all_of(outcome), all_of(exposures), all_of(covariates)) %>% unique
  
  # Create grid with all combinations of exposure, outcome, and covariates,
  # and define formula
  grid <- expand_grid(Outcome = outcome, 
                      Exposure = exposures, 
                      Covariates = c(NA, paste(covariates, collapse = ", "))) %>%
    mutate(Independent_vars = ifelse(!is.na(Covariates), 
                                     paste0(Exposure, ", ", Covariates),
                                     Exposure),
           Formula = paste(Outcome, " ~ ", gsub(", ", " + ", Independent_vars))) %>%
    select(-Independent_vars)
  
  # Evaluate each formula
  models <- map(.x = grid$Formula, .f = ~as.formula(.x)) %>%
    map(~lm(.x, data = data_i))
  
  # Pull estimated effects and CI bounds from each formula
  effects <- map(.x = models, 
                 .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                              "CI_lower" = confint(.x)[2, 1],
                              "CI_upper" = confint(.x)[2, 2],
                              "N_countries" = length(summary(.x)$residuals))) %>% 
    reduce(bind_rows)
  
  # Bind estimated effects to grid
  grid <- bind_cols(grid, effects) %>% select(-Formula)
  
  # Return grid
  return(grid)
  
}

## Analysis --------------------------------------------------------------------

# Define thresholds
thresholds <- data_model %>% pull(Threshold) %>% unique

# Estimate effects: 
# (1) cases at lockdown on length of time under lockdown 
effect_days_to_threshold <- foreach(i = thresholds, .errorhandling = "pass") %do%
  Estimate_effect_days_to_threshold(exposures = exposures,
                                    covariates = covariates,
                                    data = data_model,
                                    threshold = i) %>% 
  reduce(bind_rows) %>%
  arrange(Outcome, Exposure, Covariates) 
# (2) cases at lockdown on growth factor under lockdown
effect_growth_factor_lockdown <- Estimate_effect_growth_factor_lockdown(exposures = exposures,
                                                                        covariates = covariates,
                                                                        data = data_model) %>% 
  arrange(Outcome, Exposure, Covariates) 

# Bind all effects together
effects_between_countries <- bind_rows(effect_days_to_threshold, effect_growth_factor_lockdown)

# Save results
write_csv(effects_between_countries, file = paste0(results_directory, "effects_between_countries.csv"))

# ------------------------------------------------------------------------------
# Analysis: lockdown timing - within-country
# ------------------------------------------------------------------------------

# Effects of interest:
# (1) Effect of timing of lockdown on length of time under lockdown
# (2) Effect of timing of lockdown on total cases at end of simulation period

## Functions -------------------------------------------------------------------

# Function to estimate the percentage change in the number of days since lockdown
# required to reach population-based threshold values for various counterfactual histories, 
# compared to the natural history for a particular country
# Arguments:
# (1) country = country to estimate
##### (including natural history)
# Returns: summary table containing thresholds for each specified history, 
# and percentage change in cumulative cases from natural history
Calculate_pct_change_days_to_threshold <- function(country) {
  
  # Filter thresholds table by given country
  summary_thresholds_country <- summary_thresholds_all %>% 
    filter(Country == country) %>%
    select(Country:N_days_lockdown, Threshold, Days_since_lockdown)
  
  # Calculate number of days to reach thresholds in natural history
  thresholds_nat_hist <- summary_thresholds_country %>% 
    filter(History == "Natural history") %>%
    select(Threshold, Days_since_lockdown) %>%
    rename(Days_since_lockdown_nat_hist = Days_since_lockdown)
  
  # Calculate percent change in days to reach thresholds from natural history
  summary_thresholds_country <- full_join(summary_thresholds_country, thresholds_nat_hist, by = "Threshold") %>%
    mutate(Pct_change_days_since_lockdown = 
             ((Days_since_lockdown - Days_since_lockdown_nat_hist) / Days_since_lockdown_nat_hist)) %>%
    select(-Days_since_lockdown_nat_hist)
  
  # Return dataframe
  return(summary_thresholds_country)
  
}

# Function to estimate the percentage change in the number of cumulative cases
# for various counterfactual histories, compared to the natural history for a particular country
# Arguments:
# (1) country = country to estimate
# Returns: summary table containing number of cumulative cases on date_T 
# for each specified history, and percentage change in cumulative cases from natural history
Calculate_pct_change_cases <- function(country) {
  
  # Filter summary dataframe by country
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Record final date of cases included in analysis (date_T)
  date_T <- summary_eur_country %>% pull(Date_T)
  
  # Filter simulated data by country
  summary_cases_sim_country <- summary_cases_sim_all %>% 
    filter(Country == country)
  
  # Create summary table of cumulative cases on date_T
  summary_cases_sim_country_T <- summary_cases_sim_country %>% filter(Date == date_T) %>%
    select(Country, Simulation, History, N_days_first_restriction, N_days_lockdown,
           contains("cumulative_cases_end"))
  
  # Calculate cumulative cases on date_T in natural history
  cases_nat_hist <- summary_cases_sim_country_T %>% filter(History == "Natural history") %>% 
    pull(Mean_cumulative_cases_end)
  
  # Calculate percent change in cumulative cases from natural history
  summary_cases_sim_country_T <- summary_cases_sim_country_T %>% 
    group_by(Simulation) %>%
    mutate(Pct_change_cumulative_cases_end = ((Mean_cumulative_cases_end - cases_nat_hist) / cases_nat_hist)) %>%
    ungroup
  
  # Return dataframe
  return(summary_cases_sim_country_T)
  
}

## Analysis --------------------------------------------------------------------

# Define countries to be included in analysis
countries <- countries_eur_modelled[!countries_eur_modelled %in% countries_excluded]

# Estimate effects:
# (1) percentage change in the number of days since lockdown to reach thresholds 
# under natural vs counterfactual histories
pct_change_days_to_threshold <- foreach(i = countries,
                                        .errorhandling = "pass") %do%
  Calculate_pct_change_days_to_threshold(country = i) %>%
  bind_rows
# (2) percentage change in the number of cumulative cases 
# under counterfactual vs natural histories
pct_change_cases <- foreach(i = countries,
                            .errorhandling = "pass") %do%
  Calculate_pct_change_cases(country = i) %>%
  bind_rows

# Estimate mean effects:
# (1) percentage change in the number of days since lockdown to reach thresholds 
# under natural vs counterfactual histories
pct_change_days_to_threshold_mean <- pct_change_days_to_threshold %>% 
  group_by(Simulation, History, Threshold) %>%
  summarise(Mean_pct_change_days_since_lockdown = mean(Pct_change_days_since_lockdown, na.rm = TRUE),
            SD_pct_change_days_since_lockdown = sd(Pct_change_days_since_lockdown, na.rm = TRUE),
            N_countries_days_since_lockdown = n(),
            .groups = "keep")
# (2) percentage change in the number of cumulative cases 
# under counterfactual vs natural histories
pct_change_cases_mean <- pct_change_cases %>% 
  group_by(Simulation, History) %>%
  summarise(Mean_pct_change_cumulative_cases_end = mean(Pct_change_cumulative_cases_end, na.rm = TRUE),
            SD_pct_change_cumulative_cases_end = sd(Pct_change_cumulative_cases_end, na.rm = TRUE),
            N_countries_cumulative_cases_end = n(),
            .groups = "keep")

# Combine separate effect dataframes 
## Effects
effects_within_countries <- full_join(pct_change_days_to_threshold, pct_change_cases,
                                    by = c("Country", "Simulation", "History", "N_days_first_restriction", "N_days_lockdown"))
## Mean effects
effects_within_countries_mean <- full_join(pct_change_days_to_threshold_mean, pct_change_cases_mean,
                                         by = c("Simulation", "History"))

# Save results
write_csv(effects_within_countries, file = paste0(results_directory, "effects_within_countries.csv"))
write_csv(effects_within_countries_mean, file = paste0(results_directory, "effects_within_countries_mean.csv"))

