################################################################################
#
# Script name:        Data_Processing.R
# Script description: This script ....
# Author:             @KFArnold
#
################################################################################

# SETUP ------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(wbstats); library(sjlabelled)

# Load all source functions from "./Script/Functions/" folder
list.files("./Script/Functions", full.names = TRUE) %>% walk(~source(.))

# Define folders for unformatted and formatted data, 
# and folder for outputs and subfolder for figures
folder_data_unformatted <- "./Data/Unformatted/"
folder_data_formatted <- "./Data/Formatted/"
folder_output <- "./Output/"
folder_figures <- paste0(folder_output, "Figures/")

# Create folders for data, outputs, and figures if they do not already exist
Create_Folder_If_None_Exists(folder = folder_data_unformatted)
Create_Folder_If_None_Exists(folder = folder_data_formatted)
Create_Folder_If_None_Exists(folder = folder_output)
Create_Folder_If_None_Exists(folder = folder_figures)

# DATA DOWNLOADING/UPDATING ----------------------------------------------------

# WARNING: Running this section of code will overwrite any old datafiles which
# have been previously downloaded!

## CSSE data (cases and deaths) ------------------------------------------------

## Define online source, filenames, and out folder
#source <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/")
#filenames <- c("time_series_covid19_confirmed_global.csv",
#               "time_series_covid19_deaths_global.csv")
#out_folder <- paste0(folder_data_unformatted, "CSSE data/")
#
## Download data
#Download_Data(source = source,
#              filenames = filenames,
#              out_folder = out_folder)

## OxCGRT data (policies) ------------------------------------------------------

## Define online source, filenames, and out folder
#source <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/"
#filenames <- "OxCGRT_latest.csv"
#out_folder <- paste0(folder_data_unformatted, "OxCGRT data/")
#
## Download data
#Download_Data(source = source,
#              filenames = filenames,
#              out_folder = out_folder)

## World Bank data (demographics) ----------------------------------------------

## Define indicators to download and names to appear in dataframe, and out folder
#indicators <- c("SP.POP.TOTL", "AG.LND.TOTL.K2")
#indicators_names <- c("Population", "Area_sq_km")
#out_folder <- paste0(folder_data_unformatted, "World Bank data/")
#
## Create folder for saving data if it doesn't already exist
#Create_Folder_If_None_Exists(folder = out_folder)
#
## Download data
#wb_data(indicator = indicators,
#        start_date = 2015,
#        end_date = 2020) %>%
#  remove_all_labels %>%
#  as_tibble %>%
#  rename_with(.cols = all_of(indicators), ~indicators_names) %>%
#  rename_with(~str_to_title(.)) %>%
#  write_csv(., file = paste0(out_folder, "Worldbank_data.csv"))

# DATA FORMATTING --------------------------------------------------------------




# DATA PROCESSING  -------------------------------------------------------------

## Calculate important dates ---------------------------------------------------




## Caculate important threshold values -----------------------------------------

# Define population-based thresholds
# (0.001 cases per 100 or 1 case per 100,000; 
# 0.005 cases per 100 or 1 case per 20,000;
# 0.01 cases per 100 or 1 case per 10,000)
pop_thresholds <- c(0.00001, 0.00005, 0.0001)

# Calculate population-based threshold values and threshold values for lockdown
# easing (i.e. number of daily cases (MA7) on date of lockdown easing)
thresholds_eur <- foreach(j = countries_eur, 
                          .errorhandling = "pass") %do%
  Calculate_Threshold_Values(country = j,
                             pop_thresholds = pop_thresholds) %>%
  reduce(bind_rows)


write_csv(thresholds_eur,
          file = paste0(folder_output, "thresholds_eur.csv"))

