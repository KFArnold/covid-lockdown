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
library(tidyverse)
library(lspline); library(forecast)
library(foreach); library(doSNOW)

## Load source functions -------------------------------------------------------

# Load all source functions from "./Script/Functions/" folder
list.files("./Script/Functions", full.names = TRUE) %>% walk(~source(.))

## Load data -------------------------------------------------------------------

## Define filenames which contain data required for simulation
#filenames <- list("Cases_deaths_data_europe", 
#                  "knots_best",
#                  "summary_eur",
#                  "possible_days_counterfactual")
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

# Load list of European countries for which we have both cases/deaths data and policy data,
# and those which entered lockdown
load("./Output/countries_eur.RData")
load("./Output/countries_eur_lockdown.RData")

# IDENTIFICATION OF SIMULATION PARAMETERS --------------------------------------

# Specify parameters
countries <- countries_eur[countries_eur != "Russia"] 
criteria <- "Pois_dev_inc"  # criteria for selecting best knot points
n_best <- 10  # maximum number of best knot points to select per country

# Identify best knot dates and simulation parameters for all countries
start <- Sys.time()
knots_best <- 
  Execute_Parameter_Estimation_All_Countries(countries = countries,
                                             criteria = criteria,
                                             n_best = n_best,
                                             parallel = TRUE)
end <- Sys.time(); end - start  # ~2.3 mins




# Insert code to calculate best knot date pairs and associated growth parameters,
# and calculate possible counterfactual conditions


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
seed <- 13
max_t <- 548  # (1.5 years)
n_runs <- 100000
prob_equal <- FALSE 

## Simulate --------------------------------------------------------------------

# Run all counterfactual simulations for all countries 
start <- Sys.time()
summary_sim_all <- 
  Execute_Counterfactual_Simulations_All_Countries(countries = countries,
                                                   n_days_counterfactual = n_days_counterfactual,
                                                   seed = seed,
                                                   max_t = max_t,
                                                   n_runs = n_runs,
                                                   prob_equal = prob_equal,
                                                   parallel = TRUE,
                                                   source_functions = source_functions)
end <- Sys.time(); end - start  # ~27 mins



