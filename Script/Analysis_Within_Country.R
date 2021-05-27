################################################################################
#
# Script name:        Analysis within-country.R
# Script description: This script ....
# Author:             @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)
library(lspline); library(forecast)
library(foreach); library(doSNOW)
library(RColorBrewer); library(scales); library(ggpubr); library(ggrepel)

# Load all source functions from "./Script/Functions/" folder
list.files("./Script/Functions", full.names = TRUE) %>% walk(~source(.))

# Load list of European countries for which we have both cases/deaths data 
# and policy data,and those which entered lockdown
load("./Output/countries_eur.RData")
load("./Output/countries_eur_lockdown.RData")

# Define folder for outputs and subfolder for figures
folder_output <- "./Output/"
folder_figures <- paste0(folder_output, "Figures/")

# Create folders for outputs and figures if they do not already exist
Create_Folder_If_None_Exists(folder = folder_output)
Create_Folder_If_None_Exists(folder = folder_figures)

# IDENTIFICATION OF SIMULATION PARAMETERS --------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for identification of simulation 
# parameters into the global environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = list("Cases_deaths_data_europe", 
                                           "summary_eur"))

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

## Plot fitted splines ---------------------------------------------------------

# Create figures of fitted splines and save to subfolder
figure_splines <- foreach(j = countries, 
                          .errorhandling = "pass") %do% 
  Plot_Splines(country = j,
               out = paste0(folder_figures, "Fitted splines by country"))

# Create combined figure of fitted splines for all countries
Plot_Combined(plotlist = figure_splines, 
              title = "Exponential growth of COVID-19 cases: Fitted splines",
              title_size = 30,
              out_folder = folder_figures,
              out_name = "Figure - Fitted splines.png")

# Create combined figure of fitted splines for sample of countries
countries_sample <- list("Greece", "Switzerland", "Spain")
index <- match(countries_sample, countries)
Plot_Combined(plotlist = figure_splines[index],
              cols = length(index), 
              labels = "AUTO",
              title = "Exponential growth of COVID-19 cases: Fitted splines",
              title_size = 20,
              out_folder = folder_figures,
              out_name = "Figure - Fitted splines (sample).png", 
              return = FALSE)

# SIMULATION -------------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for simulation into the global
# environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = list("Cases_deaths_data_europe", 
                                           "summary_eur",
                                           "knots_best",
                                           "possible_days_counterfactual"))

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
list2env(summary_sim_all, .GlobalEnv)

## Figures ---------------------------------------------------------------------

# Specify countries and simulations to plot
countries <- countries_eur_lockdown[countries_eur_lockdown != "Russia"]
simulations <- c("0,0", "0,3", "0,7", "3,3", "7,7")

# Create figures of simulation results and save to subfolder
figure_sim_results <- foreach(j = countries, .errorhandling = "pass") %do% 
  Plot_Simulation_Results(country = j, 
                          simulations = simulations,
                          out = paste0(folder_figures, "Simulation results by country"))

# Create combined figure of incident and cumulative cases for sample of countries
countries_sample <- list("Greece", "Switzerland", "Spain")
index <- match(countries_sample, countries)
index %>%
  map(., .f = ~figure_sim_results[[.x]]) %>%
  map(., .f = ~.x$plots_two_annotated) %>%
  Plot_Combined(plotlist = .,
                width = 6*2,
                rows = length(.),
                labels = "AUTO",
                title = " ",
                title_size = 30,
                out_folder = folder_figures,
                out_name = "Figure - Simulation results (sample).png",
                return = FALSE)

# ANALYSIS ---------------------------------------------------------------------

# model fit
# reduction





