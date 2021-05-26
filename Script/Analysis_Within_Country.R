################################################################################
#
# Script name:        Analysis within-country.R
# Script description: This script ....
# Author:             KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)
library(lspline); library(forecast)
library(foreach); library(doSNOW)

# Load all source functions from "./Script/Functions/" folder
list.files("./Script/Functions", full.names = TRUE) %>% walk(~source(.))

# Load list of European countries for which we have both cases/deaths data 
# and policy data,and those which entered lockdown
load("./Output/countries_eur.RData")
load("./Output/countries_eur_lockdown.RData")

# Define folder for outputs
folder_output <- "./Output/"

# IDENTIFICATION OF SIMULATION PARAMETERS --------------------------------------

## Load data -------------------------------------------------------------------

# Define filenames which contain data required for identification 
# of simulation parameters
filenames <- list("Cases_deaths_data_europe", 
                  "summary_eur")

# Import all dataframes; save all to global environment
dataframes <- filenames %>%
  map(., .f = ~list.files(path = ".",
                          recursive = TRUE, 
                          pattern = paste0(., ".csv"),
                          full.names = TRUE)) %>%
  map(., .f = ~read_csv(.)) %>%
  set_names(filenames)
list2env(dataframes, globalenv()); rm(filenames, dataframes)

## Identify parameters ---------------------------------------------------------

# Specify countries to identify parameters for
countries <- countries_eur[countries_eur != "Russia"]

# Identify best knot dates and simulation parameters for all countries
# and save to output folder
start <- Sys.time()
knots_best <- 
  Execute_Parameter_Estimation_All_Countries(countries = countries,
                                             criteria_selection = "Pois_dev_inc",
                                             criteria_likelihood = "Pois_dev_cum",
                                             n_best = 10,
                                             parallel = TRUE,
                                             out_folder = folder_output)
end <- Sys.time(); end - start  # ~2.5 mins

# Calculate possible counterfactuals and save to output folder
possible_days_counterfactual <- foreach(j = countries,
                                        .errorhandling = "pass") %do%
  Calculate_Possible_Counterfactual_Days(country = j,
                                         knots = knots_best) %>%
  map(., .f = ~.x$possible_days_counterfactual) %>% 
  reduce(bind_rows) %>% 
  arrange(Country)
write_csv(possible_days_counterfactual, 
          file = paste0(folder_output, "possible_days_counterfactual.csv"))

# SIMULATION -------------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Define filenames which contain data required for simulation 
# of natural/counterfactual histories
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

## Simulate --------------------------------------------------------------------

# Specify countries to simulate
countries <- countries_eur_lockdown[countries_eur_lockdown != "Russia"]

# Specify combinations of counterfactual conditions to simulate
# (including natural history)
n_days_counterfactual <- tibble(N_days_first_restriction = 
                                  c(0, 0, 0, 0, 0, 1, 3, 5, 7),
                                N_days_lockdown =
                                  c(0, 1, 3, 5, 7, 1, 3, 5, 7))

# Run all counterfactual simulations for all countries 
# and save to sub-folder in output folder
start <- Sys.time()
summary_sim_all <- 
  Execute_Counterfactual_Simulations_All_Countries(countries = countries,
                                                   n_days_counterfactual = n_days_counterfactual,
                                                   seed = 13,
                                                   max_t = 548,
                                                   n_runs = 100000,
                                                   prob_equal = FALSE,
                                                   parallel = TRUE,
                                                   out_folder = paste0(folder_output, "Simulations/"))
end <- Sys.time(); end - start  # ~27 mins



