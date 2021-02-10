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

# Define countries to exclude from analysis
countries_excluded_all <- c("Russia")  # exclude from all analyses
countries_excluded_length_lockdown <- countries_excluded_all
countries_excluded_time_to_threshold <- c(countries_excluded_all, "San Marino")
countries_excluded_total_cases <- countries_excluded_all

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
# Analysis: lockdown timing - cross-country
# ------------------------------------------------------------------------------

# Effects of interest:
# (1) Effect of lockdown timing (i.e. cases at lockdown) on length of lockdown
# (2) Effect of lockdown timing (i.e. cases at lockdown) on growth factor under lockdown
# All may be adjusted or unadjusted effects

## Functions -------------------------------------------------------------------

# Function to estimate the effect of cases at lockdown on length of time under lockdown 
# and median growth factor under lockdown (unadjusted and adjusted effects)
# Arguments:
# (1) countries = list of countries to include in analysis
# (2) outcomes = vector outcomes 
# (3) exposures = vector of exposures (different measures of cases)
# (4) covariates = vector of covariates to adjust for
# Returns: table of adjusted and unadjusted effects for each exposure on the outcome
Estimate_Effects_Between_Countries <- function(countries, 
                                               outcomes = c("Length_lockdown", "Median_growth_factor_lockdown"),
                                               exposures = c("Cumulative_cases_beg", "Cumulative_cases_beg_MA7", 
                                                             "Daily_cases", "Daily_cases_MA7"),
                                               covariates = c("Area_sq_km", "Population")) {
  
  # Get data for all outcomes, exposures, and covariates for designated countries
  ## Exposures: Cases on lockdown dates
  data_cases <- data_eur %>% filter(Country %in% countries) %>% 
    left_join(., select(summary_eur, c(Country, Date_lockdown)), by = "Country") %>% 
    filter(Date == Date_lockdown) %>%
    select(Country, Date_lockdown, all_of(exposures))
  ## Covariates: Country stats
  data_covariates <- worldbank_eur %>% group_by(Country) %>% 
    arrange(Country, desc(Year)) %>% 
    select(Country, all_of(covariates)) %>%
    summarise(across(covariates, ~first(na.omit(.))), .groups = "keep") %>%
    ungroup
  ## Outcomes
  data_length_lockdown <- summary_eur %>% select(Country, Length_lockdown)
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
    full_join(., data_growth_factors, by = "Country")
  
  # Create grid with all combinations of exposure, outcome, and covariates,
  # and define formula
  grid <- expand_grid(Outcome = outcomes, 
                      Exposure = exposures, 
                      Covariates = c(NA, paste(covariates, collapse = ", "))) %>%
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
                              "N_countries" = length(summary(.x)$residuals))) %>% 
    reduce(bind_rows)
  
  # Bind estimated effects to grid
  grid <- bind_cols(grid, effects) %>% select(-Formula)
  
  # Return grid
  return(grid)
  
}

## Estimate between-country effects --------------------------------------------

# Define countries to include in analysis
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded_all]

# Estimate between-country effects
effects_between_countries <- Estimate_Effects_Between_Countries(countries = countries)

# Save results
write_csv(effects_between_countries, file = paste0(results_directory, "effects_between_countries.csv"))

# Save list of countries excluded
save(countries_excluded_all, file = paste0(results_directory, "countries_excluded_all.RData"))

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

# Function to esimate all between-country effects: percentage change in
# (1) length of lockdown, (2) time to thresholds, and (3) total cases
# Arguments:
# (1) countries = list of countries to include in analysis
# Returns: list of 2 dataframes: all effects within countries, and median effects
Estimate_Effects_Within_Countries <- function(countries) {
  
  # Estimate percentage change in length of lockdown, median effect
  pct_change_length_lockdown <- 
    Calculate_Pct_Change_Length_Lockdown(countries = countries[!countries %in% countries_excluded_length_lockdown])
  
  # Estimate percentage change in time to reach thresholds, median effect
  pct_change_time_to_threshold <-
    Calculate_Pct_Change_Time_To_Threshold(countries = countries[!countries %in% countries_excluded_time_to_threshold])
  
  # Estimate percentage change in total cases
  pct_change_cases <- 
    Calculate_Pct_Change_Total_Cases(countries = countries[!countries %in% countries_excluded_total_cases])
  
  # Combine all estimated effects into list
  effects_all <- list(pct_change_length_lockdown, pct_change_time_to_threshold, pct_change_cases)
  
  # Combine all within-country effects
  effects_within_countries <- map(.x = effects_all,
                                  .f = ~.x$pct_change) %>% 
    reduce(full_join) %>%
    arrange(Country, Simulation)
  
  # Combine all within-country mean effects
  effects_within_countries_median <- map(.x = effects_all,
                                         .f = ~.x$pct_change_median) %>% 
    reduce(full_join) %>%
    relocate(N_countries, .after = last_col()) %>%
    relocate(Threshold, .after = Outcome) %>%
    arrange(Simulation)
  
  # Return list of effects
  return(list(effects_within_countries = effects_within_countries,
              effects_within_countries_median = effects_within_countries_median))
  
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
    mutate(Outcome = "Length of lockdown") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Simulation)
  
  # Calculate median percent change, quartiles
  pct_change_length_lockdown_median <- pct_change_length_lockdown %>%
    group_by(Simulation, History, Outcome) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = as.numeric(n()),
              .groups = "keep") %>% ungroup
  
  # Return dataframes
  return(list("pct_change" = pct_change_length_lockdown,
              "pct_change_median" = pct_change_length_lockdown_median))
  
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
    mutate(Outcome = "Time to threshold") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Country)
  
  # Calculate median percent change, quartiles
  pct_change_time_to_threshold_median <- pct_change_time_to_threshold %>% 
    group_by(Simulation, History, Outcome, Threshold) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = n(),
              .groups = "keep") %>% ungroup
  
  # Return dataframes
  return(list("pct_change" = pct_change_time_to_threshold,
              "pct_change_median" = pct_change_time_to_threshold_median))
  
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
    mutate(Outcome = "Total cases") %>%
    relocate(Outcome, .after = History) %>%
    arrange(Country)
  
  # Calculate median percent change, quartiles
  pct_change_cases_median <- pct_change_cases %>% 
    group_by(Simulation, History, Outcome) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = n(),
              .groups = "keep") %>% ungroup
  
  # Return dataframe
  return(list("pct_change" = pct_change_cases,
              "pct_change_median" = pct_change_cases_median))
  
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
countries <- countries_eur_modelled[!countries_eur_modelled %in% countries_excluded_all]

# Estimate within-country effects
effects_within_countries_all <- Estimate_Effects_Within_Countries(countries = countries)

# Save within-country individual and median effects as separate dataframes
effects_within_countries <- effects_within_countries_all$effects_within_countries
effects_within_countries_median <- effects_within_countries_all$effects_within_countries_median

# Save both effects dataframes
write_csv(effects_within_countries, paste0(results_directory, "effects_within_countries.csv"))
write_csv(effects_within_countries_median, paste0(results_directory, "effects_within_countries_median.csv"))

# Save list of countries excluded
save(countries_excluded_length_lockdown, 
     file = paste0(results_directory, "countries_excluded_length_lockdown.RData"))
save(countries_excluded_time_to_threshold, 
     file = paste0(results_directory, "countries_excluded_time_to_threshold.RData"))
save(countries_excluded_total_cases, file = paste0(results_directory, "countries_excluded_total_cases.RData"))

