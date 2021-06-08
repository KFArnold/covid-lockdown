################################################################################
#
# Script name:        Analysis_Between_Country.R
# Script description: This script ....
# Author:             @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse)
library(RColorBrewer); library(scales); library(ggpubr); library(ggh4x)

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

# ESTIMATION OF BETWEEN-COUNTRY EFFECTS ----------------------------------------

## Load data -------------------------------------------------------------------

# Import files which contain data required for analysis into the global
# environment, if they are not already loaded
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                        "summary_eur"))

## Estimate between-country effects --------------------------------------------

# Define countries to include in analysis
countries <- countries_eur_lockdown[countries_eur_lockdown != "Russia"]

# Estimate between-country effects
effects_between_countries <- 
  Analyse_Effects_Between_Country(countries = countries,
                                  outcomes = "Length_lockdown",
                                  out_folder = folder_output)

## Plot between-country effects ------------------------------------------------

# Create figure of between-country effects
figure_effects_between_country <- 
  Plot_Effects_Between_Country_All(plots = "plot_length_lockdown",
                                   out_folder = folder_figures)
