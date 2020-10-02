# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script imports/formats COVID-19 cases, deaths, and policies data 
# for countries around the world, and selects a subset of European countries to be analysed 

# It then produces a table which summarises the state of the pandemic in each country:
# (1) First date at which each country exceeded 100 cases (and cases/deaths on this day);
# (2) First date at which any restriction was imposed (and cases/deaths on this day);
# (3) Date at which the country entered lockdown (and cases/deaths on this day);
# (4) Date at which the country came out of lockdown (and cases/deaths on this day).
# This summary table is exported to the Results folder


# Notes:
# May have to impute some values (look at Peter's code) - find NA's
# Need to take into account whether restrictions are targeted or general?
# Lithuania, Portugal, Spain, and UK have negative incidence

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(readr); library(dplyr); library(ggplot2); library(caTools)

# Run source code to update external data
#source("./Code/Update data.R")

# Define all European countries
# (from https://www.worldometers.info/geography/how-many-countries-in-europe/)
countries <- list("Albania", "Andorra", "Austria", "Belarus", "Belgium", 
                  "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Czechia",
                  "Denmark", "Estonia", "Finland", "France", "Germany", 
                  "Greece", "Holy See", "Hungary", "Iceland", "Ireland", 
                  "Italy", "Latvia", "Liechtenstein", "Lithuania", 
                  "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro",
                  "Netherlands", "North Macedonia", "Norway", "Poland",
                  "Portugal", "Romania", "Russia", "San Marino",
                  "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", 
                  "Switzerland", "Ukraine", "United Kingdom")

# Set storage directory for outputs
out <- paste0("./Results/")

## Functions -------------------------------------------------------------------

# Function to summarise the number of measures that exceed a cutoff value
# (each measure has its own specific cutoff value)
# Arguments: 
# DATA = full policies dataframe (i.e. contains all measures) for ONE country;
# MEASURES = a list of the measures to evaluate;
# CUTOFFS = a list of the cutoff points corresponding to each individual measure;
# Returns: 
# Dataframe with 2 columns: (1) Date; (2) N_measures
Summarise_N_Measures <- function(DATA, MEASURES, CUTOFFS) {
  
  # Print warning if each measure doesn't have its own cutoff point
  if (length(MEASURES) != length(CUTOFFS)) {
    print("Error: Each measure must have its own cutoff point.")
  }
  
  # Subset dataset to include only selected Measures
  data <- DATA %>% ungroup() %>% select(Date, unlist(MEASURES))
  
  # For each measure, determine whether it is greater than or equal to its cutoff
  # If yes, assign 1; if no, assign 0
  for (i in 1:length(MEASURES)) {
    measure <- MEASURES[[i]]
    data[, measure] <- ifelse(data[, measure] >= CUTOFFS[[i]], 1, 0)
  }
  
  # Calculate number of measures above cutoff (i.e. row sums)
  data$N_measures <- apply(data[, unlist(MEASURES)], 1, sum, na.rm = TRUE)
  
  # Retain Date, N_measures variables
  data <- data %>% select(Date, N_measures)
  
  # Return dataframe with Date, N_measures variables
  return(data)
  
}

# ------------------------------------------------------------------------------
# Import and format data
# ------------------------------------------------------------------------------

# Load data
## (1) CSSE data (cumulative cases and deaths)
cases <- read_csv("./Data/CSSE data/time_series_covid19_confirmed_global.csv")
deaths <- read_csv("./Data/CSSE data/time_series_covid19_deaths_global.csv")
## (2) OxCGRT data (government policies)
policies <- read_csv("./Data/OxCGRT data/OxCGRT_latest.csv",
                     col_types = cols(RegionName = col_character(),
                                      RegionCode = col_character())) %>% 
  mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))

# Replace slashes and spaces with underscores
names(cases) <- str_replace_all(names(cases), c("/" = "_", " " = "_"))
names(deaths) <- str_replace_all(names(deaths), c("/" = "_", " " = "_"))
names(policies) <- str_replace_all(names(policies), c("/" = "_", " " = "_"))

# Convert datasets to long form, select relevant variables, 
# rename country variable, convert characters to factors,
# and order / group by Province and Country
cases <- cases %>% gather(Date, Cumulative_cases_end, -(1:4)) %>% 
  mutate(Date = as.Date(Date, format = "%m_%d_%y")) %>% 
  select(-c(Lat, Long)) %>% rename(c(Country = Country_Region)) %>% 
  mutate_if(is.character, as.factor) %>%
  group_by(Province_State, Country) %>% arrange(Country, Date)
deaths <- deaths %>% gather(Date, Cumulative_deaths_end, -(1:4)) %>% 
  mutate(Date = as.Date(Date, format = "%m_%d_%y")) %>% 
  select(-c(Lat, Long)) %>% rename(c(Country = Country_Region)) %>% 
  mutate_if(is.character, as.factor) %>%
  group_by(Province_State, Country) %>% arrange(Country, Date)
policies <- policies %>% rename(c(Country = CountryName,
                                  Province_State = RegionName)) %>% 
  select(-contains("Code"), c(Date, C1_School_closing:C8_International_travel_controls)) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(Province_State, Country) %>% arrange(Country, Date)

# Calculate daily cases and deaths
cases <- cases %>% mutate(Cumulative_cases_beg = lag(Cumulative_cases_end, n = 1, default = 0),
                          Daily_cases = Cumulative_cases_end - Cumulative_cases_beg) %>%
  relocate(c(Cumulative_cases_beg, Daily_cases), .before = Cumulative_cases_end)
deaths <- deaths %>% mutate(Cumulative_deaths_beg = lag(Cumulative_deaths_end, n = 1, default = 0), 
                            Daily_deaths = Cumulative_deaths_end - Cumulative_deaths_beg) %>%
  relocate(c(Cumulative_deaths_beg, Daily_deaths), .before = Cumulative_deaths_end)

# Calculate 7-day moving averages of daily and cumulative cases and deaths
cases <- cases %>% mutate(Cumulative_cases_beg_MA7 = round(runmean(Cumulative_cases_beg, k = 7, alg = "C", endrule = "mean"), 3),
                          Daily_cases_MA7 = round(runmean(Daily_cases, k = 7, alg = "C", endrule = "mean"), 3),
                          Cumulative_cases_end_MA7 = round(runmean(Cumulative_cases_end, k = 7, alg = "C", endrule = "mean"), 3))
deaths <- deaths %>% mutate(Cumulative_deaths_beg_MA7 = round(runmean(Cumulative_deaths_beg, k = 7, alg = "C", endrule = "mean"), 3),
                            Daily_deaths_MA7 = round(runmean(Daily_deaths, k = 7, alg = "C", endrule = "mean"), 3),
                            Cumulative_deaths_end_MA7 = round(runmean(Cumulative_deaths_end, k = 7, alg = "C", endrule = "mean"), 3))

# Merge cases and deaths datasets into single dataframe
data_all <- full_join(cases, deaths) %>% relocate(contains("MA7"), .after = last_col())
rm(cases, deaths)  # (remove separate datasets)

# Create variables for: date of first case (Date_0), 
# date at which cases first exceeded 100 (Date_100), 
# number of days since 100 cases (Days_since_100), and
# date for which data can be reasonably assumed complete (Date_max)
data_all <- data_all %>% mutate(Date_0 = Date[which(Daily_cases >= 1)[1]],
                                Date_100 = Date[which(Cumulative_cases_beg >= 100)[1]],
                                Days_since_100 = as.numeric(Date - Date_100),
                                Date_max = max(Date) - 7)

# Remove data after Date_max, since this is likely incomplete
data_all <- data_all %>% filter(Date <= Date_max)

# Rename Czech Republic and Slovak Republic in policies dataset
policies <- policies %>% mutate(Country = recode(Country, "Czech Republic" = "Czechia"),
                                Country = recode(Country, "Slovak Republic" = "Slovakia"))

# Retain data for countries in Europe only
# and remove dependencies (of the Netherlands, UK, France, and Denmark)
# and drop unused levels
data_eur <- data_all %>% filter(Country %in% countries) %>% ungroup(Province_State) %>%
  filter(is.na(Province_State)) %>% select(-Province_State) %>% droplevels
policies_eur <- policies %>% filter(Country %in% countries) %>% ungroup(Province_State) %>%
  filter(is.na(Province_State)) %>% select(-Province_State) %>% droplevels

# Create list of European countries for which we have both cases/deaths data and policy data
countries_eur <- as.list(intersect(levels(data_eur$Country), levels(policies_eur$Country)))
## print note about any countries which both are not avaiable:
if (length(countries_eur) != length(countries)) {
  unavail <- setdiff(levels(data_eur$Country), levels(policies_eur$Country))
  cat(paste0("Note that the following countries do not have both cases/deaths
  data and policy data available:\n", 
             paste0(unavail, collapse = ", ")))
  rm(unavail)
} 

# Retain data for European countries for which we have both cases/deaths data and policy data
data_eur <- data_eur %>% filter(Country %in% countries_eur) %>% droplevels
policies_eur <- policies_eur %>% filter(Country %in% countries_eur) %>% droplevels

# Remove non-European dataframes and lists
rm(countries, data_all, policies)

# ------------------------------------------------------------------------------
# Summarise countries 
# ------------------------------------------------------------------------------

# Create summary table which defines first date at which cases first exceeded 100 (Date_100)
summary_eur <- data_eur %>% filter(Date == Date_100) %>% 
  select(-c(Date_100, Days_since_100, contains(c("end", "MA7")))) %>%
  rename_at(vars(-c(Country, Date_0, Date_max)), .funs = list(~ paste0(., "_100"))) %>%
  relocate(c(Date_0, Date_max), .before = Date_100)

# Create empty summary tables to store:
# (1) Max number of restrictions
summary_max_number_restrictions <- data_eur %>% 
  select(Country) %>% mutate(Max_number_restrictions = as.numeric(NA))
summary_max_number_restrictions <- summary_max_number_restrictions[0, ]
# (2) Date of first restriction (including cases and deaths)
summary_first_restriction <- data_eur %>% 
  select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_first_restriction"))) 
summary_first_restriction <- summary_first_restriction[0, ]
# (3) Date of lockdown (including cases and deaths)
summary_lockdown <- data_eur %>% 
  select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown"))) 
summary_lockdown <- summary_lockdown[0, ]
# (4) Date of lockdown end (including cases and deaths)
summary_lockdown_end <- data_eur %>% 
  select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown_end"))) 
summary_lockdown_end <- summary_lockdown_end[0, ]
# (5) Date of lockdown easing (including cases and deaths)
summary_lockdown_eased <- data_eur %>% 
  select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown_eased"))) 
summary_lockdown_eased <- summary_lockdown_eased[0, ]


# Define requirements of interest and cutoff points for whether measures have been implemented
# [1 corresponds to measure recommended, 2-3 corresponds to measure required]
## (1) Lockdown measure
measures_lockdown <- list("C6_Stay_at_home_requirements")
cutoffs_lockdown <- list(2)
#length(measures_lockdown) == length(cutoffs_lockdown)
## (2) Alternate group of measures which together are broadly equivalent to lockdown
measures_lockdown_alt <- list("C1_School_closing", "C2_Workplace_closing", "C3_Cancel_public_events",
                              "C5_Close_public_transport", "C7_Restrictions_on_internal_movement")
cutoffs_lockdown_alt <- list(2, 2, 2, 2, 2)
#length(measures_lockdown_alt) == length(cutoffs_lockdown_alt)
## (3) Full group of measures (i.e. any restrictions)
measures_any_restriction <- as.list(sort(unlist(c(measures_lockdown, measures_lockdown_alt))))
cutoffs_any_restriction <- list(1, 1, 1, 1, 1, 1)
#length(measures_any_restriction) == length(cutoffs_any_restriction)

# Define threshold values, i.e. how many measures together constitute a given restriction
threshold_lockdown <- 1
threshold_lockdown_alt <- 2
threshold_any_restriction <- 1

# Record country summary data
for (i in 1:nrow(summary_eur)) {
  
  # Define country
  country <- summary_eur[[i, "Country"]] %>% as.character()
  
  # Filter cases/deaths and policies datasets by country
  policies_eur_i <- policies_eur %>% filter(Country == country)
  data_eur_i <- data_eur %>% filter(Country == country)
  
  # Define date_max (i.e. last date for which data can be reasonably assumed complete)
  date_max <- data_eur_i %>% pull(Date_max) %>% head(1)
  
  # Filter policies data by date_max
  policies_eur_i <- policies_eur_i %>% filter(Date <= date_max)
  
  # Calculate max number of restrictions --------
  # (either recommended or required)
  
  # Determine number of recommended or required measures values on each date
  data_any_restriction <- Summarise_N_Measures(DATA = policies_eur_i,
                                               MEASURES = measures_any_restriction,
                                               CUTOFFS = cutoffs_any_restriction)
  
  # Determine max number of recommended or required measures
  max_number_restrictions <- data_any_restriction %>% summarise(max(N_measures)) %>% pull()
  
  # Create summary of max number of measures
  summary_max_number_restrictions_i <- tibble(Country = as.factor(country), 
                                              Max_number_restrictions = max_number_restrictions)
  
  # Add max number of measures to summary table
  summary_max_number_restrictions <- bind_rows(summary_max_number_restrictions_i, summary_max_number_restrictions)
  
  # Summarise first restriction --------
  
  # Determine whether threshold of 1 exceeded for each date 
  # (i.e. whether ANY measures were recommended)
  data_any_restriction <- data_any_restriction %>% 
    mutate(Threshold_exceeded = ifelse(N_measures >= threshold_any_restriction, TRUE, FALSE))
  
  # Create indicator for whether any restriction was recommended/required
  any_restriction_tf <- any(data_any_restriction$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Find first instance where any measures were recommended
  # and summarise cases/deaths on this date
  if (any_restriction_tf == TRUE) {
    # Filter by dates where any measures were recommended
    data_any_restriction_filt <- data_any_restriction %>% filter(Threshold_exceeded == TRUE)
    # Define date of first restriction as first date where threshold was exceeded
    date_first_restriction <- data_any_restriction_filt %>% pull(Date) %>% min(na.rm = TRUE)
    # Create summary of cases/deaths on date of first restriction
    summary_first_restriction_i <- data_eur_i %>% filter(Date == date_first_restriction) %>% 
      select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
      rename_at(vars(-Country), .funs = list(~ paste0(., "_first_restriction"))) 
    # Merge with full first restriction summary dataset
    summary_first_restriction <- bind_rows(summary_first_restriction, summary_first_restriction_i)
  }
  
  # Summarise lockdown --------
  
  # Determine number of the following required measures on each date
  # (1) lockdown (general or targeted)
  data_lockdown <- Summarise_N_Measures(DATA = policies_eur_i,
                                        MEASURES = measures_lockdown,
                                        CUTOFFS = cutoffs_lockdown)
  # (2) alternate lockdown measures
  data_lockdown_alt <- Summarise_N_Measures(DATA = policies_eur_i, 
                                            MEASURES = measures_lockdown_alt, 
                                            CUTOFFS = cutoffs_lockdown_alt)
  
  # Determine change in number of restrictions from previous day
  # (negative value indicates restriction(s) lifted, ...
  # ...zero indicates no change, positive value indicates restriction(s) increased)
  data_lockdown <- data_lockdown %>% 
    mutate(N_measures_diff = N_measures - lag(N_measures, n = 1, default = NA))
  data_lockdown_alt <- data_lockdown_alt %>% 
    mutate(N_measures_diff = N_measures - lag(N_measures, n = 1, default = NA))
  
  # Merge two dataframes together
  data_lockdown_all <- full_join(data_lockdown, data_lockdown_alt, by = "Date", suffix = c("_lockdown", "_lockdown_alt"))
  
  # Determine whether lockdown measures or alternate lockdown threshold is exceeded for each date,
  # and whether number of lockdown measures or alternate lockdown measures decreases (i.e. diff is negative) for each date
  data_lockdown_all <- data_lockdown_all %>% 
    mutate(Threshold_exceeded = ifelse(N_measures_lockdown >= threshold_lockdown |
                                         N_measures_lockdown_alt >= threshold_lockdown_alt,
                                       TRUE, FALSE),
           Diff_neg = ifelse(N_measures_diff_lockdown < 0 | N_measures_diff_lockdown_alt < 0, 
                             TRUE, FALSE)) %>%
    relocate(-contains("diff"))
  
  # Create indicator for whether lockdown was entered
  # (TRUE if threshold exceeded on any date, FALSE if threshold never exceeded)
  lockdown_tf <- any(data_lockdown_all$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Analyse countries which entered lockdown
  if (lockdown_tf == TRUE) {
    
    ## Beginning of lockdown ##
    # (first date where either lockdown or alternate lockdown measures were required)
    
    # Filter lockdown data by dates where lockdown threshold was exceeded
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Threshold_exceeded == TRUE)
    # Define lockdown date as first date where threshold was exceeded
    date_lockdown <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
    # Create summary of cases/deaths on date of lockdown
    summary_lockdown_i <- data_eur_i %>% filter(Date == date_lockdown) %>% 
      select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
      rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown"))) 
    # Merge with full lockdown summary dataset
    summary_lockdown <- bind_rows(summary_lockdown, summary_lockdown_i)
    
    ## Lockdown eased ##
    # (first date after lockdown date where number of measures decreased)
    
    # Filter lockdown data by dates greater than lockdown date
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Date >= date_lockdown)
    
    # Create indicator for whether lockdown was eased
    # (TRUE if difference is negative on any date, FALSE if difference always >= 0)
    lockdown_eased_tf <- any(data_lockdown_all_filt$Diff_neg == TRUE, na.rm = TRUE)
    
    # Analyse countries which eased lockdown
    if (lockdown_eased_tf == TRUE) {
      # Re-filter data by dates where difference is negative
      data_lockdown_all_filt <- data_lockdown_all_filt %>% filter(Diff_neg == TRUE)
      # Define lockdown easing date as first date with negative difference
      date_lockdown_eased <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
      # Create summary of cases/deaths on date lockdown eased
      summary_lockdown_eased_i <- data_eur_i %>% filter(Date == date_lockdown_eased) %>% 
        select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
        rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown_eased")))
      # Merge with full lockdown easing summary dataset
      summary_lockdown_eased <- bind_rows(summary_lockdown_eased, summary_lockdown_eased_i)
    }
    
    ## End of lockdown ##
    # (first date AFTER lockdown date where neither lockdown measures no alt lockdown measures were required)
    
    # Filter lockdown data by dates greater than lockdown date
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Date >= date_lockdown)
    
    # Create indicator for whether lockdown was ended
    lockdown_end_tf <- any(data_lockdown_all_filt$Threshold_exceeded == FALSE, na.rm = TRUE)
    
    # Analyse countries which ended lockdown
    if (lockdown_end_tf == TRUE) {
      # Re-filter data by dates where lockdown threshold wasn't exceeded
      data_lockdown_all_filt <- data_lockdown_all_filt %>% filter(Threshold_exceeded == FALSE) 
      # Define lockdown end date
      date_lockdown_end <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
      # Create summary of cases/deaths on date of lockdown end
      summary_lockdown_end_i <- data_eur_i %>% filter(Date == date_lockdown_end) %>% 
        select(-c(Date_0, Date_100, Days_since_100, Date_max, contains(c("end", "MA7")))) %>%
        rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown_end"))) 
      # Merge with full lockdown summary dataset
      summary_lockdown_end <- bind_rows(summary_lockdown_end, summary_lockdown_end_i)
    }
    
  }  # (close loop 2 - countries which entered lockdown)
  
}  # (close loop 1 - all countries)
# Remove measures/cutoffs, loop objects
rm(measures_lockdown, measures_lockdown_alt, measures_any_restriction,
   cutoffs_lockdown, cutoffs_lockdown_alt, cutoffs_any_restriction,
   threshold_lockdown, threshold_lockdown_alt, threshold_any_restriction)
rm(i, country, data_eur_i, policies_eur_i, date_max,
   any_restriction_tf, lockdown_tf, lockdown_eased_tf, lockdown_end_tf,
   summary_max_number_restrictions_i, summary_first_restriction_i, 
   summary_lockdown_i, summary_lockdown_eased_i, summary_lockdown_end_i,
   data_any_restriction, data_any_restriction_filt, 
   data_lockdown, data_lockdown_alt, data_lockdown_all, data_lockdown_all_filt,
   max_number_restrictions, date_first_restriction, 
   date_lockdown, date_lockdown_eased, date_lockdown_end)

# Combine all summary datasets; remove separate dataframes
summary_eur <- full_join(summary_eur, summary_max_number_restrictions, by = "Country") %>%
  full_join(., summary_first_restriction, by = "Country") %>%
  full_join(., summary_lockdown, by = "Country") %>% 
  full_join(., summary_lockdown_eased, by = "Country") %>%
  full_join(., summary_lockdown_end, by = "Country") %>% ungroup
rm(summary_max_number_restrictions, summary_first_restriction, 
   summary_lockdown, summary_lockdown_eased, summary_lockdown_end)

# Export  summary table
write_csv(summary_eur, path = paste0(out, "Country summaries.csv"))

# ------------------------------------------------------------------------------
# Determine countries that entered lockdown
# ------------------------------------------------------------------------------

# Define European countries which entered lockdown
countries_eur_lockdown <- summary_eur %>% filter(!is.na(Date_lockdown)) %>% 
  pull(Country) %>% as.character %>% as.list
## print note about any countries which did not enter lockdown:
if (length(countries_eur_lockdown) != length(countries_eur)) {
  unavail <- setdiff(unlist(countries_eur), unlist(countries_eur_lockdown))
  cat(paste0("Note that the following countries did not enter lockdown:\n", 
             paste0(unavail, collapse = ", ")))
  rm(unavail)
} 
# EXCLUDE RUSSIA HERE...?


# Create datasets for countries which entered lockdown
data_eur_lockdown <- data_eur %>% filter(Country %in% countries_eur_lockdown) %>% droplevels
policies_eur_lockdown <- policies_eur %>% filter(Country %in% countries_eur_lockdown) %>% droplevels

# Create summary table for countries which entered lockdown
summary_eur_lockdown <- summary_eur %>% filter(Country %in% countries_eur_lockdown)

