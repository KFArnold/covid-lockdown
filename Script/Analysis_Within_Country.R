################################################################################
#
# Script name:        Analysis_Within_Country.R
# Script description: This script estimates the within-country effects of 
#                     lockdown timing on on length of lockdown and total cases
#                     among countries in Europe via simulation. Simulation
#                     parameters are estimated first, and then various
#                     'natural' and 'counterfactual' histories are simulated for
#                     each country individually. Model fit is assessed, and 
#                     median effects are calculated across all countries.
# Author:             @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(magrittr); library(rlang)
library(lspline); library(forecast)
library(foreach); library(doSNOW)
library(RColorBrewer); library(scales); library(ggpubr); library(ggrepel); library(ggh4x)

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
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe", 
                                        "summary_eur"))

## Identify simulation parameters ----------------------------------------------

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

## Summarise simulation parameters ---------------------------------------------

# Specify countries to summarise parameters for
countries <- countries_eur_lockdown[countries_eur_lockdown != "Russia"]

# Calculate and save simulation parameter summaries
simulation_parameter_summary <- 
  Calculate_Simulation_Parameter_Summary(countries = countries,
                                         out_folder = folder_output)

# Calculate median growth factors for each country among best knots, and save
median_growth_factors <- knots_best %>% 
  group_by(Country) %>%
  summarise(Median_growth_factor_1 = median(Growth_factor_1, na.rm = TRUE),
            Median_growth_factor_2 = median(Growth_factor_2, na.rm = TRUE),
            Median_growth_factor_3 = median(Growth_factor_3, na.rm = TRUE),
            .groups = "keep") %>%
  ungroup %T>%
  write_csv(., file = paste0(folder_output, "median_growth_factors.csv"))

## Plot fitted splines ---------------------------------------------------------

# Specify countries 
countries <- countries_eur[countries_eur != "Russia"]

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
              out_name = "Fitted splines.png", 
              return = FALSE)

# Create combined figure of fitted splines for sample of countries
countries_sample <- list("Greece", "Netherlands", "Spain")
index <- match(countries_sample, countries)
Plot_Combined(plotlist = figure_splines[index],
              cols = length(index), 
              labels = "AUTO",
              title = "Exponential growth of COVID-19 cases: Fitted splines",
              title_size = 20,
              out_folder = folder_figures,
              out_name = "Fitted splines (sample).png", 
              return = FALSE)

# SIMULATION -------------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for simulation into the global
# environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe", 
                                        "summary_eur",
                                        "knots_best",
                                        "possible_days_counterfactual"))

## Simulate --------------------------------------------------------------------

# Specify countries to simulate
countries <- countries_eur[!countries_eur %in% c("Monaco", "Russia")]

# Specify combinations of counterfactual conditions to simulate
# (including natural history)
simulations <- list(list(Simulation_type = "shift_intervention_sequence",
                         N_days_first_restriction = 0,
                         N_days_lockdown = 0,
                         Description = "Natural history"),
                    list(Simulation_type = "shift_intervention_sequence",
                         N_days_first_restriction = 7,
                         N_days_lockdown = 7,
                         Description = "Earlier intervention sequence (7,7)"),
                    list(Simulation_type = "time_between_interventions",
                         N_days_first_restriction = 0,
                         Days_between_interventions = "min",
                         Description = "Earliest possible lockdown"),
                    list(Simulation_type = "time_between_interventions",
                         N_days_first_restriction = 7,
                         Days_between_interventions = "min",
                         Description = "Earlier first restriction (7) and earliest possible lockdown"))

# Run all counterfactual simulations for all countries 
# and save to sub-folder in output folder
start <- Sys.time()
summary_sim_all <- 
  Execute_Counterfactual_Simulations_All_Countries(countries = countries,
                                                   simulations = simulations,
                                                   seed = 13,
                                                   max_t = 548,
                                                   n_runs = 100000,
                                                   prob_equal = FALSE,
                                                   out_folder = paste0(folder_output, "Simulations/"))
end <- Sys.time(); end - start  # ~36 mins (not parallelised)
list2env(summary_sim_all, .GlobalEnv)

## Plot simulation results -----------------------------------------------------

# Specify countries, simulations, and thresholds to plot
countries <- countries_eur[!countries_eur %in% c("Monaco", "Russia")]
simulations <- c("Natural history",
                 "Earlier intervention sequence (7,7)",
                 "Earliest possible lockdown",
                 "Earlier first restriction (7) and earliest possible lockdown")
thresholds <- c("1 case per 100,000", "1 case per 20,000", "1 case per 10,000")

# Create figures of simulation results and save to subfolder
figure_sim_results <- foreach(j = countries, 
                              .errorhandling = "pass") %do% 
  Plot_Simulation_Results(country = j, 
                          simulations = simulations,
                          thresholds = thresholds,
                          out = paste0(folder_figures, "Simulation results by country"))

# Create combined figure of incident and cumulative cases for sample of countries
# with common legend
countries_sample <- list("Greece", "Netherlands", "Spain")
index <- match(countries_sample, countries)
common_legend <- figure_sim_results[[1]]$legend
figure_sim_results_sample <- index %>%
  map(., .f = ~figure_sim_results[[.x]]) %>%
  map(., .f = ~.x$plots_two_annotated) %>%
  Plot_Combined(plotlist = .,
                width = 6*2,
                rows = length(.),
                labels = "AUTO",
                title = " ",
                title_size = 30,
                return = TRUE) %>%
  gridExtra::grid.arrange(., common_legend, nrow = 2, 
                          heights = c(6*length(countries_sample), 1)) 
ggsave(filename = paste0(folder_figures, "Simulation results (sample).png"),
       plot = figure_sim_results_sample, 
       width = 6*2, height = 6*length(countries_sample) + 1, 
       limitsize = FALSE)

# MODEL FIT --------------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for model fit assessment into the global
# environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe", 
                                        "summary_eur",
                                        "thresholds_eur"))
Import_All_Simulated_Data()

## Assess model fit ------------------------------------------------------------

# Define countries for which natural history was simulated
countries <- summary_daily_cases_sim_all %>% 
  filter(History == "Natural history") %>% 
  pull(Country) %>% unique %>% as.character %>% as.list

# Calculate model fit statistics for all simulated countries
model_fit_all <- Execute_Model_Fit_Assessment_All_Countries(countries = countries,
                                                            out_folder = folder_output)
list2env(model_fit_all, envir = .GlobalEnv); rm(model_fit_all)

## Plot model fit --------------------------------------------------------------

# Create figure of all model fit statistics, with outliers labelled
figure_model_fit <- Plot_Model_Fit(countries = countries, 
                                   out_folder = folder_figures)

## Plot model residuals --------------------------------------------------------

# Create figure of model residuals
figure_model_residuals <- foreach(j = countries, 
                                  .errorhandling = "pass") %do% 
  Plot_Model_Residuals_All(country = j,
                           out_folder = paste0(folder_figures, "Model residuals by country/"))

# Create combined figures (all countries) for residuals of incident and cumulative cases
Plot_Combined(plotlist = map(.x = figure_model_residuals, .f = ~.x$plot_inc),
              title = "Model residuals: incident cases",
              title_size = 30,
              out_folder = folder_figures,
              out_name = "Model residuals (incident cases).png",
              return = FALSE)
Plot_Combined(plotlist = map(.x = figure_model_residuals, .f = ~.x$plot_cum),
              title = "Model residuals: cumulative cases",
              title_size = 30,
              out_folder = folder_figures,
              out_name = "Model residuals (cumulative cases).png",
              return = FALSE)

# ANALYSIS ---------------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for analysis into the global
# environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = c("summary_eur",
                                        "thresholds_eur"))
Import_All_Simulated_Data(filenames = c("summary_daily_cases_sim",
                                        "summary_cumulative_cases_end_sim"))

## Estimate within-country effects ---------------------------------------------

# Specify countries to include in analysis
countries <- countries_eur[!countries_eur %in% c("Monaco", "Russia")]

# Calculate within-country effects for all simulated countries
effects_within_country_all <- Execute_Within_Country_Analysis(countries = countries,
                                                              out_folder = folder_output)
list2env(effects_within_country_all, envir = .GlobalEnv); rm(effects_within_country_all)

## Plot within-country effects -------------------------------------------------

# Specify simulations and descriptions to plot
simulations <- c("Earlier intervention sequence (7,7)",
                 "Earliest possible lockdown",
                 "Earlier first restriction (7) and earliest possible lockdown")
description <- "all simulations"

# Create figure of within-country effects
figure_effects_within_country <- 
  Plot_Effects_Within_Country_All(simulations = simulations, 
                                  plots = c("plot_time_to_thresholds",
                                            "plot_length_lockdown",
                                            "plot_total_cases"),
                                  description = description, 
                                  out_folder = folder_figures)
