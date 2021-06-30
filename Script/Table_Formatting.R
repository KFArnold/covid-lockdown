################################################################################
#
# Script name:        Table_Formatting.R
# Script description: This script formats various .csv files related to data
#                     processing and within- and between-country analyses, so
#                     that they may be easily inserted into the manuscript.
# Author:             @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)
library(ggpubr); library(RColorBrewer)
library(lubridate)

# Load all source functions from "./Script/Functions/" folder
list.files("./Script/Functions", full.names = TRUE) %>% walk(~source(.))

# Load list of European countries for which we have both cases/deaths data 
# and policy data
load("./Output/countries_eur.RData")

# Define folder for outputs and subfolders for formatted tables
folder_output <- "./Output/"
folder_tables <- paste0(folder_output, "Tables/")

# Create folders for outputs and tables if they do not already exist
Create_Folder_If_None_Exists(folder = folder_output)
Create_Folder_If_None_Exists(folder = folder_tables)

# TABLE FORMATTING -------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import files containing observed data and important dates
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                        "summary_eur",
                                        "knots_best",
                                        "effects_between_countries",
                                        "effects_within_countries_summary"),
                          silent = FALSE)

## Create formatted tables -----------------------------------------------------

# Create and save formatted table of descriptive statistics, density and QQ plots
summary_descriptives <- Summary_Table_Descriptive_Statistics(countries = countries_eur,
                                                             out_folder = folder_tables)

# Create and save formatted table of important dates
summary_dates <- Summary_Table_Important_Dates(countries = countries_eur,
                                               out_folder = folder_tables)

# Create and save formatted table of best knot dates
countries <- knots_best %>% pull(Country) %>% unique %>% as.list
#countries <- list("Greece", "Netherlands","Spain")
summary_best_knots <- Summary_Table_Best_Knots(countries = countries,
                                               out_folder = folder_tables)

# Create and save formatted table of between-country effects
summary_effects_between_countries <- 
  Summary_Table_Effects_Between_Country(outcomes = "Length_lockdown",
                                        leverage_points = "Included",
                                        out_folder = folder_tables)

# Save formatted summary table of within-country effects
summary_effects_within_countries <- 
  Summary_Table_Effects_Within_Country(outcomes = c("Length_lockdown",
                                                    "Total_cases"),
                                       out_folder = folder_tables)

# Save formatted country-specific and summary tables of model fit statistics
summary_model_fit <- Summary_Tables_Model_Fit(out_folder = folder_tables)
