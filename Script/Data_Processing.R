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
library(tidyverse); library(wbstats); library(sjlabelled); library(caTools)
library(foreach)
library(RColorBrewer)

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
#Download_Source_Data(source = source,
#                     filenames = filenames,
#                     out_folder = out_folder)

## OxCGRT data (policies) ------------------------------------------------------

## Define online source, filenames, and out folder
#source <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/"
#filenames <- "OxCGRT_latest.csv"
#out_folder <- paste0(folder_data_unformatted, "OxCGRT data/")
#
## Download data
#Download_Source_Data(source = source,
#                     filenames = filenames,
#                     out_folder = out_folder)

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

# Define all European countries
# (from https://www.worldometers.info/geography/how-many-countries-in-europe/)
countries <- list("Albania", "Andorra", "Austria", "Belarus", "Belgium", 
                  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czech Republic",
                  "Denmark", "Estonia", "Finland", "France", "Germany", 
                  "Greece", "Holy See", "Hungary", "Iceland", "Ireland", 
                  "Italy", "Latvia", "Liechtenstein", "Lithuania", 
                  "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro",
                  "Netherlands", "North Macedonia", "Norway", "Poland",
                  "Portugal", "Romania", "Russia", "San Marino",
                  "Serbia", "Slovak Republic", "Slovenia", "Spain", "Sweden", 
                  "Switzerland", "Ukraine", "United Kingdom")

# Format all source data for European countries
source_data <- Format_Source_Data(countries = countries, 
                                  source_folder = folder_data_unformatted,
                                  out_folder = folder_data_formatted)
list2env(source_data, envir = .GlobalEnv); rm(source_data)

# DATA PROCESSING  -------------------------------------------------------------

## Load data -------------------------------------------------------------------

# Import formatted data files into the global environment, 
# if they are not already loaded
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                        "Policy_data_europe",
                                        "Worldbank_data_europe"))

# Load list of European countries for which we have both cases/deaths data 
# and policy data
load(paste0(folder_output, "countries_eur.RData"))

## Summarise countries ---------------------------------------------------------

### Calculate most recent demographic statistics -------------------------------

# Calculate most recent demographic statistics from World Bank data for all countries
demographics <- Worldbank_data_europe %>%
  group_by(Country) %>% 
  arrange(Country, desc(Year)) %>%
  summarise(across(c(Area_sq_km, Population), ~first(na.omit(.))), .groups = "keep") %>%
  ungroup

### Calculate date of first case -----------------------------------------------

# Calculate date of first case for all countries
date_first_case <- Cases_deaths_data_europe %>% 
  group_by(Country) %>%
  filter(Daily_cases >= 1) %>%
  slice(1) %>%
  select(Country, Date_1 = Date) %>%
  ungroup

### Calculate important policy dates -------------------------------------------

# Define the following:
# (a) policy measures of interest
# (b) cutoff points for whether measures have been implemented
### (1 corresponds to measure recommended, 2-3 corresponds to measure required),
# (c) geographic scope(s) for measures of interest
### (0 corresponds to targeted, 1 corresponds to general)
# (d) threshold values (i.e. how many measures together constitute a given restriction)

# Any restrictions (recommended or required, targeted or general)
measures_any_restriction <- list("measures" = c("C1_School_closing", 
                                                "C2_Workplace_closing", 
                                                "C3_Cancel_public_events",
                                                "C5_Close_public_transport", 
                                                "C6_Stay_at_home_requirements",
                                                "C7_Restrictions_on_internal_movement"),
                                 "cutoffs" = c(1, 1, 1, 1, 1, 1),
                                 "scope" = c(0, 1), 
                                 "threshold" = 1)
# Lockdown measure (required, general)
measures_lockdown <- list("measures" = "C6_Stay_at_home_requirements",
                          "cutoffs" = 2,
                          "scope" = 1,
                          "threshold" = 1)
# Alternate group of measures which together are broadly equivalent to lockdown 
# (required, general)
measures_lockdown_alt <- list("measures" = c("C1_School_closing", 
                                             "C2_Workplace_closing", 
                                             "C3_Cancel_public_events",
                                             "C5_Close_public_transport", 
                                             "C7_Restrictions_on_internal_movement"),
                              "cutoffs" = c(2, 2, 2, 2, 2),
                              "scope" = 1,
                              "threshold" = 3)

# Calculate important policy dates for all countries
policy_dates <- foreach(j = countries_eur, 
                        .errorhandling = "pass") %do%
  Calculate_Policy_Dates(country = j,
                         measures_any_restriction = measures_any_restriction,
                         measures_lockdown = measures_lockdown,
                         measures_lockdown_alt = measures_lockdown_alt) %>% 
  reduce(bind_rows)

# Make list of all countries which entered lockdown, and save to output folder
countries_eur_lockdown <- policy_dates %>% 
  filter(!is.na(Date_lockdown)) %>% 
  pull(Country) %>% 
  as.character %>% as.list
save(countries_eur_lockdown, file = paste0(folder_output, "countries_eur_lockdown.RData"))
## print note about any countries which did not enter lockdown:
if (length(countries_eur_lockdown) != length(countries_eur)) {
  unavail <- setdiff(unlist(countries_eur), unlist(countries_eur_lockdown))
  cat("Note that the following countries did not enter lockdown:",
      paste0(unavail, collapse = ", "), 
      sep = "\n")
  rm(unavail)
} 

# Calculate length of full lockdown for all countries
policy_dates <- policy_dates %>%
  mutate(Length_lockdown = as.numeric(Date_lockdown_eased - Date_lockdown))

### Calculate date range to include in simulation and analysis -----------------

# Calculate range of dates to be included in analysis (Date_start to Date_T)
# for all countries:
# Date_start represents the first full day for which total cases exceeded the 
# defined population-based threshold or 5 cases, which ever is greater; and
# Date_T represents 28 days after the date of lockdown easing
date_range <- foreach (j = countries_eur,
                       .errorhandling = "pass") %do%
  Calculate_Date_Range_For_Analysis(country = j,
                                    policy_dates = policy_dates) %>%
  bind_rows

# Manually replace start date for Denmark, Estonia, Finland, Norway, Slovenia, and Sweden
date_range <- date_range %>% 
  mutate(Date_start = if_else(Country == "Denmark", as.Date("2020-03-14") + 3, Date_start),
         Date_start = if_else(Country == "Estonia", as.Date("2020-03-16") + 3, Date_start),
         Date_start = if_else(Country == "Finland", as.Date("2020-03-15") + 3, Date_start),
         Date_start = if_else(Country == "Norway", as.Date("2020-03-14") + 3, Date_start),
         Date_start = if_else(Country == "Slovenia", as.Date("2020-03-18") + 3, Date_start),
         Date_start = if_else(Country == "Sweden", as.Date("2020-03-14") + 3, Date_start))

### Combine all summary data ---------------------------------------------------

# Combine summary data from all countries 
summary_eur <- demographics %>%
  full_join(., date_first_case, by = "Country") %>%
  full_join(., policy_dates, by = "Country") %>%
  full_join(., date_range, by = "Country")

# Save summary table to output folder
write_csv(summary_eur, file = paste0(folder_output, "summary_eur.csv"))

### Plot important dates -------------------------------------------------------

# Specify dates to include in figure, and ordering variable
dates <- c("Date_1", "Date_first_restriction", "Date_lockdown", "Date_lockdown_eased")
order <- "Date_lockdown"

# Create figure of important dates
figure_important_dates <- Plot_Important_Dates(countries = countries_eur, 
                                               dates = dates,
                                               order = order,
                                               out_folder = folder_figures)

## Calculate important threshold values -----------------------------------------

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

# Save table containing threshold values to output folder
write_csv(thresholds_eur, file = paste0(folder_output, "thresholds_eur.csv"))
