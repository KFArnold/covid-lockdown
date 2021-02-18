# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script summarises important dates in the COVID-19 timeline for all
# European countries for which we have data relating to both cases and policies.
# These dates include case thresholds (e.g. date of first confirmed case)
# and policy implemetations (e.g. date of first restriction, 
# date when restrictions eased, date of lockdown, date when lockdown eased).

# This script then identifies when the effects of the first restriction and 
# lockdown (if applicable) were realised, by identifying the 'knot dates' 
# at which the growth factor changed.

# Lastly, this script identifies the number of days earlier that the first
# restriction and lockown measures may be (counterfactually) estimated.

# All outputs are saved to the project directory (./Results/).

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(scales); library(lspline); library(forecast)
library(foreach); library(parallel); library(doSNOW)

# Define storage directory for formatted data
data_directory_f <- paste0("./Data/Formatted/")

# Define storage directory for results
results_directory <- paste0("./Results/")

# Load formatted data
data_eur <- read_csv(paste0(data_directory_f, "Cases_deaths_data_europe.csv"))
policies_eur <- read_csv(paste0(data_directory_f, "Policy_data_europe.csv")) 
worldbank_eur <- read_csv(paste0(data_directory_f, "Worldbank_data_europe.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data
load(paste0(results_directory, "countries_eur.RData"))

# ------------------------------------------------------------------------------
# Summarise countries 
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to calculate the dates for which a country implements (and ends) certain measures
# Arguments:
# (1) country = country to analyse
# (2) measures_any_restriction = list containing 3 items:
##### (a) measures = list of measures constituting any restriction, 
##### (b) cutoffs = list of cutoffs corresponding to measures, and
##### (c) threshold = number representing minimum number of measures consituting any restriction
# (3) measures_lockdown = list containing 3 items:
##### (a) measures = list of measures constituting lockdown, 
##### (b) cutoffs = list of cutoffs corresponding to measures, and
##### (c) threshold = number representing minimum number of measures consituting lockdown
# (4) measures_lockdown_alt = list containing 3 items:
##### (a) measures = list of alternative measures constituting lockdown, 
##### (b) cutoffs = list of cutoffs corresponding to measures, and
##### (c) threshold = number representing minimum number of measures consituting alternative lockdown
Calculate_Policy_Dates <- function(country, measures_any_restriction, 
                                   measures_lockdown, measures_lockdown_alt) {
  
  # Filter cases/deaths and policies datasets by country
  policies_eur_country <- policies_eur %>% filter(Country == country)
  data_eur_country <- data_eur %>% filter(Country == country)
  
  # Define date_max (i.e. last date for which data can be reasonably assumed complete)
  date_max <- data_eur_country %>% pull(Date) %>% max
  
  # Filter policies data by date_max
  policies_eur_country <- policies_eur_country %>% filter(Date <= date_max)
  
  # Create empty summary table
  summary <- tibble(Country = country,
                    Date_first_restriction = as.Date(NA),
                    Date_restrictions_eased = as.Date(NA),
                    Date_lockdown = as.Date(NA),
                    Date_lockdown_eased = as.Date(NA),
                    Date_lockdown_end = as.Date(NA),
                    Max_number_restrictions = as.numeric(NA))
  
  # Restrictions  --------
  
  # (either recommended or required)
  
  # Determine number of recommended or required measures values on each date
  data_any_restriction <- Summarise_N_Measures(data = policies_eur_country,
                                               measures = measures_any_restriction$measures,
                                               cutoffs = measures_any_restriction$cutoffs)
  
  # Determine whether any measures were recommended/required (i.e. threshold exceeded) for each date,
  # and whether number of measures decreases (i.e. diff is negative) for each date
  data_any_restriction <- data_any_restriction %>% 
    mutate(Threshold_exceeded = ifelse(N_measures >= measures_any_restriction$threshold, TRUE, FALSE),
           Diff_neg = ifelse(N_measures_diff < 0, TRUE, FALSE)) %>%
    relocate(-contains("diff"))
  
  # Create indicator for whether any restriction was recommended/required
  any_restriction_tf <- any(data_any_restriction$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Analyse if any restriction recommended/required
  if (any_restriction_tf == TRUE) {
    
    ## Max number of restrictions ##
    
    # Determine max number of recommended or required measures
    max_number_restrictions <- data_any_restriction %>% summarise(max(N_measures)) %>% pull()
    # Add max number of measures to summary table
    summary["Max_number_restrictions"] <- max_number_restrictions
    
    ## First restriction ##
    ## (first date where any restrictions were either recommended or required) ##
    
    # Filter restrictions data by dates where any measures were recommended
    data_any_restriction_filt <- data_any_restriction %>% filter(Threshold_exceeded == TRUE)
    # Define date of first restriction as first date where threshold was exceeded
    date_first_restriction <- data_any_restriction_filt %>% pull(Date) %>% min(na.rm = TRUE)
    # Add to summary table
    summary["Date_first_restriction"] <- date_first_restriction
    
    ## Restrictions eased ##
    ## (first date after date of first restriction where number of measures decreased) ##
    
    # Filter restrictions data by dates greater than date of first restriction
    data_any_restriction_filt <- data_any_restriction %>% filter(Date >= date_first_restriction)
    
    # Create indicator for whether restrictions were eased
    # (TRUE if difference is negative on any date, FALSE if difference always >= 0)
    restrictions_eased_tf <- any(data_any_restriction_filt$Diff_neg == TRUE, na.rm = TRUE)
    
    # Analyse countries whose restrictions were eased
    if (restrictions_eased_tf == TRUE) {
      # Re-filter data by dates where difference is negative
      data_any_restriction_filt <- data_any_restriction_filt %>% filter(Diff_neg == TRUE)
      # Define date of restrictions eased as first date with negative difference
      date_restrictions_eased <- data_any_restriction_filt %>% pull(Date) %>% min(na.rm = TRUE)
      # Add to summary table
      summary["Date_restrictions_eased"] <- date_restrictions_eased
    }  
    
  }  # (close loop - any restriction recommended/required)
  
  # Summarise lockdown --------
  
  # Determine number of the following required measures on each date
  # (1) lockdown (general or targeted)
  data_lockdown <- Summarise_N_Measures(data = policies_eur_country,
                                        measures = measures_lockdown$measures,
                                        cutoffs = measures_lockdown$cutoffs)
  # (2) alternate lockdown measures
  data_lockdown_alt <- Summarise_N_Measures(data = policies_eur_country, 
                                            measures = measures_lockdown_alt$measures, 
                                            cutoffs = measures_lockdown_alt$cutoffs)
  
  # Merge two dataframes together
  data_lockdown_all <- full_join(data_lockdown, data_lockdown_alt, by = "Date", suffix = c("_lockdown", "_lockdown_alt"))
  
  # Determine whether lockdown measures or alternate lockdown threshold is exceeded for each date,
  # and whether number of lockdown measures or alternate lockdown measures decreases (i.e. diff is negative) for each date
  data_lockdown_all <- data_lockdown_all %>% 
    mutate(Threshold_exceeded = ifelse(N_measures_lockdown >= measures_lockdown$threshold |
                                         N_measures_lockdown_alt >= measures_lockdown_alt$threshold,
                                       TRUE, FALSE),
           Diff_neg = ifelse(N_measures_diff_lockdown < 0 | N_measures_diff_lockdown_alt < 0, 
                             TRUE, FALSE)) %>%
    relocate(-contains("diff"))
  
  # Create indicator for whether lockdown was entered
  # (TRUE if threshold exceeded on any date, FALSE if threshold never exceeded)
  lockdown_tf <- any(data_lockdown_all$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Analyse if country entered lockdown
  if (lockdown_tf == TRUE) {
    
    ## Beginning of lockdown ##
    ## (first date where either lockdown or alternate lockdown measures were required) ##
    
    # Filter lockdown data by dates where lockdown threshold was exceeded
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Threshold_exceeded == TRUE)
    # Define lockdown date as first date where threshold was exceeded
    date_lockdown <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
    # Add to summary table
    summary["Date_lockdown"] <- date_lockdown
    
    ## Lockdown eased ##
    ## (first date after lockdown date where number of measures decreased) ##
    
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
      # Add to summary table
      summary["Date_lockdown_eased"] <- date_lockdown_eased
    }
    
    ## End of lockdown ##
    ## (first date AFTER lockdown date where neither lockdown measures no alt lockdown measures were required) ##
    
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
      # Add to summary table
      summary["Date_lockdown_end"] <- date_lockdown_end
    }
    
  }  # (close inner loop - entered lockdown)
  
  # Return summary table
  return(summary)
  
}

# Function to summarise the number of measures that exceed a cutoff value
# (each measure has its own specific cutoff value) for every day
# Arguments: 
# (1) data = full policies dataframe (i.e. contains all measures) for one country;
# (2) measures = a list of the measures to evaluate;
# (3) cutoffs = a list of the cutoff points corresponding to each individual measure;
# Returns: 
# Dataframe with 3 columns: (1) Date; (2) N_measures; (3) N_measures_diff (change from previous day)
Summarise_N_Measures <- function(data, measures, cutoffs) {
  
  # Print warning if each measure doesn't have its own cutoff point
  if (length(measures) != length(cutoffs)) {
    stop("Error: Each measure must have its own cutoff point.")
  }
  
  # Subset dataset to include only selected Measures
  data <- data %>% ungroup() %>% select(Date, unlist(measures))
  
  # For each measure, determine whether it is greater than or equal to its cutoff
  # If yes, assign 1; if no, assign 0
  for (i in 1:length(measures)) {
    measure <- measures[[i]]
    data[, measure] <- ifelse(data[, measure] >= cutoffs[[i]], 1, 0)
  }
  
  # Calculate number of measures above cutoff (i.e. row sums)
  data$N_measures <- apply(data[, unlist(measures)], 1, sum, na.rm = TRUE)
  
  # Retain Date, N_measures variables
  data <- data %>% select(Date, N_measures)
  
  # Determine change in number of restrictions from previous day
  # (negative value indicates restriction(s) lifted, ...
  # ...zero indicates no change, positive value indicates restriction(s) increased)
  data <- data %>%
    mutate(N_measures_diff = N_measures - lag(N_measures, n = 1, default = NA))
  
  # Return dataframe with Date, N_measures variables
  return(data)
  
}

# Function to calculate population-based threshold values for a particular country
# Arguments:
# (1) country = country
# (2) year = year from which to use population
# (3) thresholds = vector of threshold(s), representing proportion of total population
# Returns: dataframe with country, description of threshold (%), and value of threshold
Calculate_Pop_Threshold_Values <- function(country, year = 2019, thresholds) {
  
  # Filter World Bank data by specified country and most recent year
  worldbank_eur_country <- worldbank_eur %>% filter(Country == country, Year == year) %>%
    select(Country, Population)
  
  # Calculate value of threshold based on population
  threshold_values <- worldbank_eur_country %>%
    expand_grid(Threshold = thresholds) %>%
    mutate(Threshold_value = Population * Threshold,
           Threshold = percent(Threshold)) %>%
    select(-Population)
  
  # Return table of thresholds and values
  return(threshold_values)
  
}

## Summaries -------------------------------------------------------------------

# Calculate 0.0001% of population for each country
pct <- worldbank_eur %>% filter(Year == 2019) %>% 
  mutate(Pop_pct = 0.000001 * Population) %>% select(Country, Pop_pct)

# Create summary table containing dates of important case thresholds:
# date of first case (Date_1), 
# dates at which cases first exceeded 5 and 100 (Date_5, Date_100), 
# date at which cases first exceeded defined pct (Date_pop_pct)
# date for which data can be reasonably assumed complete (Date_max)
summary_eur_cases <- full_join(data_eur, pct, by = "Country") %>% 
  group_by(Country) %>%
  mutate(Date_1 = Date[which(Daily_cases >= 1)[1]],
         Date_5 = Date[which(Cumulative_cases_beg >= 5)[1]],
         Date_100 = Date[which(Cumulative_cases_beg >= 100)[1]],
         Date_pop_pct = Date[which(Cumulative_cases_beg >= Pop_pct)[1]],
         Date_max = max(Date)) %>%
  select(Country, Date_1:Date_max) %>%
  unique %>% ungroup
rm(pct)

# Define policy measures of interest, cutoff points for whether measures have been implemented
# (1 corresponds to measure recommended, 2-3 corresponds to measure required),
# and threshold values (i.e. how many measures together constitute a given restriction)
## (1) Any restrictions 
measures_any_restriction <- list("measures" = list("C1_School_closing", 
                                                   "C2_Workplace_closing", 
                                                   "C3_Cancel_public_events",
                                                   "C5_Close_public_transport", 
                                                   "C6_Stay_at_home_requirements",
                                                   "C7_Restrictions_on_internal_movement"),
                                 "cutoffs" = list(1, 1, 1, 1, 1, 1),
                                 "threshold" = 1)
## (2) Lockdown measure
measures_lockdown <- list("measures" = list("C6_Stay_at_home_requirements"),
                          "cutoffs" = list(2),
                          "threshold" = 1)
## (3) Alternate group of measures which together are broadly equivalent to lockdown
measures_lockdown_alt <- list("measures" = list("C1_School_closing", 
                                                "C2_Workplace_closing", 
                                                "C3_Cancel_public_events",
                                                "C5_Close_public_transport", 
                                                "C7_Restrictions_on_internal_movement"),
                              "cutoffs" = list(2, 2, 2, 2, 2),
                              "threshold" = 3)

# Create summary table containing dates of policy thresholds
summary_eur_policies <- foreach(i = countries_eur, .errorhandling = "pass") %do%
  Calculate_Policy_Dates(country = i,
                         measures_any_restriction = measures_any_restriction,
                         measures_lockdown = measures_lockdown,
                         measures_lockdown_alt = measures_lockdown_alt) %>% reduce(bind_rows)

# Combine summary dataframes into one
summary_eur <- full_join(summary_eur_cases, summary_eur_policies, by = "Country")
rm(summary_eur_cases, summary_eur_policies)

# Determine European countries which entered lockdown
# and save list to Results folder
countries_eur_lockdown <- summary_eur %>% filter(!is.na(Date_lockdown)) %>% 
  pull(Country) %>% as.character %>% as.list
save(countries_eur_lockdown, file = paste0(results_directory, "countries_eur_lockdown.RData"))
## print note about any countries which did not enter lockdown:
if (length(countries_eur_lockdown) != length(countries_eur)) {
  unavail <- setdiff(unlist(countries_eur), unlist(countries_eur_lockdown))
  cat(paste0("Note that the following countries did not enter lockdown:\n", 
             paste0(unavail, collapse = ", ")))
  rm(unavail)
} 

# Define first date from which to include data (Date_start)
summary_eur <- summary_eur %>% group_by(Country) %>% 
  mutate(Date_start = max(Date_5, Date_pop_pct)) %>% ungroup

# Manually replace start date for Denmark, Estonia, Norway, Slovenia, and Sweden
summary_eur <- summary_eur %>% 
  mutate(Date_start = if_else(Country == "Denmark", as.Date("2020-03-14") + 3, Date_start),
         Date_start = if_else(Country == "Estonia", as.Date("2020-03-17") + 3, Date_start),
         Date_start = if_else(Country == "Norway", as.Date("2020-03-14") + 3, Date_start),
         Date_start = if_else(Country == "Slovenia", as.Date("2020-03-18") + 3, Date_start),
         Date_start = if_else(Country == "Sweden", as.Date("2020-03-14") + 3, Date_start))

# Calculate:
# (1) date lockdown eased (or, if no lockdown, date restrictions eased)
# (2) date_T (last date to include data from) as date of easing + 28 days
# (3) length of lockdown as number of days from date of lockdown to date of easing
summary_eur <- summary_eur %>% 
  mutate(Date_eased = if_else(is.na(Date_lockdown), Date_restrictions_eased, Date_lockdown_eased),
         Date_T = Date_eased + 28,
         Length_lockdown = as.numeric(Date_lockdown_eased - Date_lockdown)) %>%
  relocate(Length_lockdown, .before = Max_number_restrictions)

# Add population and area size columns to summary table
summary_eur <- worldbank_eur %>% 
  group_by(Country) %>% 
  arrange(Country, desc(Year)) %>%
  select(Country, Area_sq_km, Population) %>%
  summarise(across(c(Area_sq_km, Population), ~first(na.omit(.))), .groups = "keep") %>%
  ungroup %>%
  right_join(., summary_eur, by = "Country")

# Export summary table
write_csv(summary_eur, file = paste0(results_directory, "summary_eur.csv"))

## Threshold values ------------------------------------------------------------

# Define population-based thresholds
thresholds <- c(0.00001, 0.00005, 0.0001)

# Calculate population-based threshold values
thresholds_eur <- foreach(i = countries_eur, .errorhandling = "pass") %do%
  Calculate_Pop_Threshold_Values(country = i,
                                 thresholds = thresholds) %>%
  reduce(bind_rows)

# Export tables of threshold values 
write_csv(thresholds_eur, file = paste0(results_directory, "thresholds_eur.csv"))

# ------------------------------------------------------------------------------
# Estimate knot points
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to calculate best knot dates for a given country according to a specified criteria
# Arguments:
# (1) country = country to calculate
# (2) criteria = criteria by which to select best knot dates (Poisson deviance of incident or cumulative cases)
# (3) n_best = number of best knots to select
# Returns list of dataframe containing best knots and associated parameters
Estimate_Best_Knots <- function(country, criteria = c("Pois_dev_inc", "Pois_dev_cum"),
                                n_best) {
  
  # Initialise progress bar
  progress_bar <- txtProgressBar(min = 0, max = 1, style = 3, char = paste(country, " "))
  
  # Filter cases/deaths, summary dataframes by country
  data_eur_country <- data_eur %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Record important dates
  date_start <- summary_eur_country %>% pull(Date_start)
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  date_T <- summary_eur_country %>% pull(Date_T)
  
  # Create copy of cases/deaths dataframe where 
  # cumulative cases >= starting threshold and up to date_T
  data_eur_country_in_range <- data_eur_country %>% filter(Date >= date_start & Date <= date_T)
  
  # Calculate potential knot dates
  knot_dates <- Calculate_Potential_Knots(country = country,
                                          date_first_restriction = date_first_restriction,
                                          date_lockdown = date_lockdown,
                                          date_start = date_start,
                                          date_T = date_T,
                                          window = c(2, 28))
  
  # Print warning and stop if there are no potential knot dates
  if (nrow(knot_dates) == 0) {
    stop(paste("Knot estimation is not possible for", country, 
               ". Restrictions were imposed long before threshold for modelling was reached."))
  }
  
  # Create dataframe to store summary statistics for all possible combinations of knot dates
  knot_summaries <- tibble(Knot_date_1 = as.Date(character()),
                           Knot_date_2 = as.Date(character()),
                           N_knots = as.numeric(),
                           Growth_factor_1 = as.numeric(),
                           Growth_factor_2 = as.numeric(),
                           Growth_factor_3 = as.numeric(),
                           Growth_factor_1_sd = as.numeric(),
                           Growth_factor_2_sd = as.numeric(),
                           Growth_factor_3_sd = as.numeric(),
                           Intercept_1 = as.numeric(),
                           Intercept_2 = as.numeric(),
                           Intercept_3 = as.numeric(),
                           Pois_dev_inc = as.numeric(),
                           Pois_dev_cum = as.numeric())
  
  # (1) Iterate through pairs of candidate knot dates
  # Estimate growth parameters, estimate growth using those parameters
  for (i in 1:nrow(knot_dates)) {
    
    # Set knot dates
    knot_date_1 <- knot_dates[[i, "Knot_date_1"]]
    knot_date_2 <- knot_dates[[i, "Knot_date_2"]]
    
    # Estimate growth parameters
    ## If first knot occurs at first date for which cases exceeded pop pct threshold (i.e. when we begin modelling),
    ## there may be either no knots (i.e. knot occured before or at date_start)
    ## OR 1 knot (occurring at knot_date_2).
    ## Otherwise, there may be either 1 knot (occurring at knot_date_1)
    ## OR 2 knots (occurring at knot_date_1 and knot_date_2)
    if (knot_date_1 == date_start) {
      
      if (is.na(knot_date_2)) {  # NO knot points
        
        # Set number of knot points
        n_knots <- 0
        
        # Set knot point
        knot_1 <- NA
        knot_2 <- NA
        
        # Define data to be used for fitting
        data_i <- data_eur_country_in_range %>% select(Daily_cases, Cumulative_cases_beg)
        covariates <- "Cumulative_cases_beg"
        
        # Estimate growth parameters
        # (Arima spline model with 1 knot and intercept)
        parameters <- Estimate_Growth_Parameters(n_knots = n_knots,
                                                 knot_1, knot_2,
                                                 intercept = TRUE,
                                                 data = data_i, 
                                                 covariates = covariates)
        
        # Skip to next iteration if error occurred in estimation
        if (parameters$Error == TRUE) { next }
        
      } else {  # ONE knot point (at knot_date_2)
        
        # Set number of knot points
        n_knots <- 1
        
        # Set knot points
        knot_1 <- data_eur_country_in_range %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        knot_2 <- NA
        
        # Create dataframe for fitting manual splines
        data_i <- data.frame(lspline(data_eur_country_in_range$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_i) <- covariates <- paste0("Cumulative_cases_beg_", 1:2)
        data_i <- bind_cols(Daily_cases = data_eur_country_in_range$Daily_cases, data_i)
        
        # Estimate growth parameters
        # (Arima spline model with 1 knot and intercept)
        parameters <- Estimate_Growth_Parameters(n_knots = n_knots,
                                                 knot_1, knot_2,
                                                 intercept = TRUE,
                                                 data = data_i, 
                                                 covariates = covariates)
        
        # Skip to next iteration if error occurred in estimation
        if (parameters$Error == TRUE) { next }
        
      }
      
    } else {
      
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        
        # Set number of knot points 
        n_knots <- 1
        
        # Set knot points
        knot_1 <- data_eur_country_in_range %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        knot_2 <- NA
        
        # Create dataframe for fitting manual splines
        data_i <- data.frame(lspline(data_eur_country_in_range$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_i) <- covariates <- paste0("Cumulative_cases_beg_", 1:2)
        data_i <- bind_cols(Daily_cases = data_eur_country_in_range$Daily_cases, data_i)
        
        # Estimate growth parameters
        # (Arima spline model with 1 knot and no intercept)
        parameters <- Estimate_Growth_Parameters(n_knots = n_knots,
                                                 knot_1, knot_2,
                                                 intercept = FALSE,
                                                 data = data_i, 
                                                 covariates = covariates)
        
        # Skip to next iteration if error occurred in estimation
        if (parameters$Error == TRUE) { next }
        
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        
        # Set number of knot points
        n_knots <- 2
        
        # Set knot points
        knot_1 <- data_eur_country_in_range %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        knot_2 <- data_eur_country_in_range %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_i <- data.frame(lspline(data_eur_country_in_range$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
        names(data_i) <- covariates <- paste0("Cumulative_cases_beg_", 1:3)
        data_i <- bind_cols(Daily_cases = data_eur_country_in_range$Daily_cases, data_i)
        
        # Estimate growth parameters
        # (Arima spline model with 2 knots and no intercept)
        parameters <- Estimate_Growth_Parameters(n_knots = n_knots,
                                                 knot_1, knot_2,
                                                 intercept = FALSE,
                                                 data = data_i, 
                                                 covariates = covariates)
        
        # Skip to next iteration if error occurred in estimation
        if (parameters$Error == TRUE) { next }
        
      }
      
    }  # (close section which estimates growth parameters)
    
    # Record incident and cumulative cases (MA7) on date_start - 1
    inc_startminus1 <- data_eur_country %>% filter(Date == (date_start - 1)) %>% pull(Daily_cases_MA7)
    cum_startminus1 <- data_eur_country %>% filter(Date == (date_start - 1)) %>% pull(Cumulative_cases_end_MA7)
    
    # Estimate incident cases over modelling period
    daily_cases_sim <- Estimate_Growth(date_start = date_start,
                                       date_end = date_T,
                                       start_value = inc_startminus1,
                                       n_knots = n_knots,
                                       knot_date_1 = knot_date_1,
                                       knot_date_2 = knot_date_2,
                                       parameters = parameters)
    
    # Calculate cumulative cases over modelling period
    daily_cases_sim_copy <- daily_cases_sim
    daily_cases_sim_copy[, 1] <- cum_startminus1
    cumulative_cases_end_sim <- apply(X = daily_cases_sim_copy, MARGIN = 1, FUN = cumsum) %>% t
    
    # Calculate and record Poisson deviance
    ## (1) For predicted vs true (7-day moving average) incident cases
    true_inc <- data_eur_country_in_range$Daily_cases_MA7
    pred_inc <- daily_cases_sim[1, -1]
    pois_dev_inc <- Calc_Pois_Dev(obs = true_inc, sim = pred_inc)
    ## (2) For predicted vs true (7-day moving average) cumulative cases
    true_cum <- data_eur_country_in_range$Cumulative_cases_end_MA7
    pred_cum <- cumulative_cases_end_sim[1, -1]
    pois_dev_cum <- Calc_Pois_Dev(obs = true_cum, sim = pred_cum)
    
    # Convert parameters list to dataframe, 
    # label with knot dates, number of knots, and Poisson deviances
    parameters <- parameters %>% bind_cols %>% 
      mutate(Knot_date_1 = knot_date_1, Knot_date_2 = knot_date_2, N_knots = n_knots,
             Pois_dev_inc = pois_dev_inc, Pois_dev_cum = pois_dev_cum) 
    
    # Bind model parameters to summary table
    knot_summaries <- bind_rows(knot_summaries, parameters)
    
    # Update progress bar
    setTxtProgressBar(progress_bar, i / (nrow(knot_dates) + 1))
    
  }  # (close loop 1 (i))
  
  # Remove from consideration knot date pairs for which ...
  # (1) growth factor 1 is less than 1 AND growth factor 3 exists 
  # (because where there are 3 segments, the first scenario must represent initial uncontrolled growth)
  # (2) any of growth factors are negative
  # (3) growth factor 1 is less than 2, or 2 is less than 3
  # (4) any of growth factor SDs are NaN
  remove_1 <- knot_summaries %>% filter(Growth_factor_1 < 1 & !is.na(Growth_factor_3)) 
  remove_2 <- knot_summaries %>% filter(Growth_factor_1 < 0 | Growth_factor_2 < 0 | Growth_factor_3 < 0)
  remove_3 <- knot_summaries %>% filter(Growth_factor_1 < Growth_factor_2 | Growth_factor_2 < Growth_factor_3)
  remove_4 <- knot_summaries %>% filter(is.nan(Growth_factor_1_sd) | is.nan(Growth_factor_2_sd) | is.nan(Growth_factor_3_sd))
  knot_summaries <- anti_join(knot_summaries, remove_1, by = names(knot_summaries)) %>% 
    anti_join(., remove_2, by = names(knot_summaries)) %>%
    anti_join(., remove_3, by = names(knot_summaries)) %>%
    anti_join(., remove_4, by = names(knot_summaries)) 
  
  # Find best knot points (by lowest value of specified criteria) and label with country
  knots_best <- knot_summaries %>% arrange(eval(parse(text = criteria))) %>% 
    head(n_best) %>% select(-Error_occurred) %>%
    mutate(Country = country) %>% relocate(Country) %>%
    arrange(Knot_date_1, Knot_date_2)
  
  # Update progress bar
  setTxtProgressBar(progress_bar, 1)
  close(progress_bar)
  
  # Return dataframe of best knots
  return(list(knots_best = knots_best))
  
}

# Function to calculate all potential knot dates for a given country
# Arguments:
# (1) country = country to calculate
# (2) date_first_restriction = date of first restriction in country
# (3) date_lockdown = date of lockdown in country
# (4) date_start = start date of modelling period
# (5) date_T = end date of modelling period
# (6) window = vector of two values representing the min and max number of days after a restriction
###### that its results might potentially be realised
# Returns: grid of of all possible combinations of knot dates
Calculate_Potential_Knots <- function(country, date_first_restriction, date_lockdown,
                                      date_start, date_T, window) {
  
  # Countries with only one distinct intervention
  if (is.na(date_lockdown) | date_first_restriction == date_lockdown) {
    
    # Calculate potential knot dates
    possible_knot_dates_1 <- seq(from = date_first_restriction + window[1], 
                                 to = date_first_restriction + window[2], by = 1)
    
    # Create grid of potential knot dates, with restriction that they fall within modelling period
    grid <- tibble("Knot_date_1" = possible_knot_dates_1) %>% 
      filter(Knot_date_1 >= date_start, Knot_date_1 < date_T) %>% 
      mutate("Knot_date_2" = as.Date(NA))
    
  } else { # Countries with potentially two distinct interventions
    
    # Calculate potential knot dates
    possible_knot_dates_1 <- seq(from = date_first_restriction + window[1], 
                                 to = date_first_restriction + window[2], by = 1)
    possible_knot_dates_2 <- seq(from = date_lockdown + window[1], 
                                 to = date_lockdown + window[2], by = 1)
    
    # Create grid of potential knot dates, with restriction that they fall within modelling period
    # and that first knot date is before/at the same time as second
    grid <- tibble(expand.grid(possible_knot_dates_2, possible_knot_dates_1))
    names(grid) <- c("Knot_date_2", "Knot_date_1")
    grid <- grid %>% select("Knot_date_1", "Knot_date_2") %>% 
      filter(Knot_date_1 <= Knot_date_2, Knot_date_1 >= date_start, Knot_date_2 < date_T)  
    
    # If first knot date equals second knot date, replace second with NA 
    # (i.e. effects of interventions realised on same day)
    for (g in 1:nrow(grid)) {
      k_1 <- grid[[g, "Knot_date_1"]]
      k_2 <- grid[[g, "Knot_date_2"]]
      if (k_1 == k_2) {grid[[g, "Knot_date_2"]] <- NA}
      
    }
    
  }
  
  # Return grid of potential knot dates
  return(grid)
  
}

# Function to estimate the growth parameters for given knot dates using an Arima (spline) model
# Arguments:
# (1) n_knots = number of knot dates
# (2) knot_1 = value of knot 1 (cumulative cases)
# (3) knot_2 = value of knot 2 (cumulative cases)
# (4) intercept = whether to fit an intercept
# (5) data = dataframe containing daily and cumulative cases
# (6) covariates = names of covariates in the model
# Returns: list of model parameters, and indicator for whether error occurred in model estimation
Estimate_Growth_Parameters <- function(n_knots = c(0, 1, 2), 
                                       knot_1, knot_2, 
                                       intercept = c(TRUE, FALSE),
                                       data, covariates) {
  
  # Create estimation error indicator
  error_occurred <- FALSE
  
  # Fit ARIMA spline model with input data
  model <- tryCatch(Arima(data$Daily_cases, order = c(2, 0, 0), 
                          seasonal = list(order = c(1, 0, 0), period = 7),
                          xreg = as.matrix(data[, covariates]), 
                          include.constant = intercept, method = "ML"), 
                    error = function(e) { error_occurred <<- TRUE } )
  
  # If error occurred in model estimation, 
  # return list of empty parameters with error
  if (error_occurred == TRUE) { return(list(Growth_factor_1 = as.numeric(NA),
                                            Growth_factor_2 = as.numeric(NA),
                                            Growth_factor_3 = as.numeric(NA),
                                            Growth_factor_1_sd = as.numeric(NA),
                                            Growth_factor_2_sd = as.numeric(NA),
                                            Growth_factor_3_sd = as.numeric(NA),
                                            Intercept_1 = as.numeric(NA),
                                            Intercept_2 = as.numeric(NA),
                                            Intercept_3 = as.numeric(NA),
                                            Error = error_occurred)) }
  
  # Record model parameters (intercept, slope, and SD of slope),
  # and calculate growth factor(s)
  if (n_knots == 0) {
    
    intercept_1 <- as.numeric(coef(model)["intercept"])
    slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg"])
    slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg"]]
    growth_factor_1 <- slope_1 + 1
    
  } else if (n_knots == 1) {
    
    intercept_1 <- 0
    slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
    slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
    slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]]
    slope_2_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]]
    intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
    growth_factor_1 <- slope_1 + 1
    growth_factor_2 <- slope_2 + 1
    
  } else {
    
    intercept_1 <- 0
    slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
    slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
    slope_3 <- as.numeric(coef(model)["Cumulative_cases_beg_3"])
    slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]]
    slope_2_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]]
    slope_3_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_3"]]
    intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
    intercept_3 <- (intercept_2 + slope_2*knot_2) - slope_3*knot_2
    growth_factor_1 <- slope_1 + 1
    growth_factor_2 <- slope_2 + 1
    growth_factor_3 <- slope_3 + 1
    
  }
  
  # Return list of model parameters
  return(list(Growth_factor_1 = ifelse(exists("growth_factor_1"), growth_factor_1, NA),
              Growth_factor_2 = ifelse(exists("growth_factor_2"), growth_factor_2, NA),
              Growth_factor_3 = ifelse(exists("growth_factor_3"), growth_factor_3, NA),
              Growth_factor_1_sd = ifelse(exists("slope_1_sd"), slope_1_sd, NA),
              Growth_factor_2_sd = ifelse(exists("slope_2_sd"), slope_2_sd, NA),
              Growth_factor_3_sd = ifelse(exists("slope_3_sd"), slope_3_sd, NA),
              Intercept_1 = ifelse(exists("intercept_1"), intercept_1, NA),
              Intercept_2 = ifelse(exists("intercept_2"), intercept_2, NA),
              Intercept_3 = ifelse(exists("intercept_3"), intercept_3, NA),
              Error_occurred = error_occurred))
  
}

# Function to estimate the growth of cases over a specified time period
# Arguments:
# (1) date_start = start date
# (2) date_end = end date
# (3) start value = value of daily cases on which to start simulation
# (4) n_knots = number of knots (where growth factor changes)
# (5) knot_date_1 = date of first knot
# (6) knot_date_2 = date of second knot
# (7) parameters = list containing growth parameters 
# Returns: matrix of simulated incident cases
Estimate_Growth <- function(date_start, date_end, start_value, 
                            n_knots = c(0, 1, 2),
                            knot_date_1, knot_date_2,
                            parameters) {
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_start, to = date_end, by = 1)
  
  # Create matrices for simulated incidence data 
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- 
    matrix(nrow = 1, ncol = length(dates) + 1,
           dimnames = list(1, as.character(seq.Date(from = date_start - 1, to = date_end, by = 1))))
  
  # Initialise matrix with data at date_start - 1
  daily_cases_sim[, 1] <- start_value
  
  # Iterate through dates
  for (t in as.list(dates)) {
    
    # Get daily cases from time t-1
    inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
    
    # Define growth parameters
    if (n_knots == 0) {  # NO knot points
      
      growth <- parameters$Growth_factor_1  
      
    } else if (n_knots == 1) {  # ONE knot point
      
      if (t <= knot_date_1) {
        growth <- parameters$Growth_factor_1
      } else {
        growth <- parameters$Growth_factor_2
      }
      
    } else {  # TWO knot points
      
      if (t <= knot_date_1) {
        growth <- parameters$Growth_factor_1
      } else if (t <= knot_date_2) {
        growth <- parameters$Growth_factor_2
      } else {
        growth <- parameters$Growth_factor_3
      }
      
    }
    
    # Calculate daily cases at time t and record
    inc_t <- growth*inc_tminus1
    daily_cases_sim[, as.character(t)] <- inc_t
    
  }
  
  # Return list of incident cases
  return(daily_cases_sim)
  
}

# Function to calculate Poisson deviance between two vectors
# (from: https://en.wikipedia.org/wiki/Deviance_(statistics))
# Arguments: 
# (1) obs = vector of observed values
# (2) sim = vector of simulated/predicted values
Calc_Pois_Dev <- function(obs, sim) {
  
  #D <- 2 * sum(obs * log(obs / sim) - (obs - sim))
  D <- 2 * sum(obs * log(obs / sim) - (obs - sim), na.rm = TRUE)
  return(D)
  
}

## Estimation ------------------------------------------------------------------

# Specify parameters
criteria <- "Pois_dev_inc"  # criteria for selecting best knot points
n_best <- 10  # number of best knot points to select per country

### Parallelised ---------------------------------------------------------------

# Set up parallelisation
n_cores <- detectCores()
cluster <- parallel::makeCluster(n_cores[1] - 1, setup_strategy = "sequential")
registerDoSNOW(cluster)

# Set up progress bar
iterations <- length(countries_eur)
progress_bar <- txtProgressBar(min = 1, max = iterations, style = 3)
progress <- function(n) { setTxtProgressBar(progress_bar, n) }
options <- list(progress = progress)

# Estimation
start <- Sys.time()
knots_best <- foreach(i = countries_eur, 
                      .errorhandling = "pass", 
                      .packages = c("tidyverse", "lspline", "forecast"), 
                      .options.snow = options) %dopar% 
  Estimate_Best_Knots(country = i, 
                      criteria = criteria,
                      n_best = n_best)
end <- Sys.time()
end - start  # ~ 1.8 mins

# Close progress bar and stop parallel processing
close(progress_bar)
stopCluster(cluster)

# Combine summary results for all countries, arrange by country
knots_best <- map(.x = knots_best,
                  .f = ~.x$knots_best) %>% reduce(bind_rows) %>% arrange(Country) 

### Sequential -----------------------------------------------------------------

## Estimation
#start <- Sys.time()
#knots_best <- foreach(i = countries_eur, 
#                      .errorhandling = "pass") %do% 
#  Estimate_Best_Knots(country = i, 
#                      criteria = criteria,
#                      n_best = n_best)
#end <- Sys.time()
#end - start  # ~ 11 mins
#
## Combine summary results for all countries, arrange by country
#knots_best <- map(.x = knots_best,
#                  .f = ~.x$knots_best) %>% reduce(bind_rows) %>% arrange(Country) 

## Calculate probabilities of best knot dates ----------------------------------

# Group best knots dataframe by country
knots_best <- knots_best %>% group_by(Country)

# Construct probability of each pair of knot points
## (1) Equal probability of each knot point pair
knots_best <- knots_best %>% 
  mutate(n_knots = n(), Prob_equal = 1 / n_knots) %>%
  select(-n_knots) 
## (2) Unequal probabilities of each knot point pair according to Pois_dev_cum
### Create inverse of Pois_dev_cum values so that lower values are ranked higher,
### calculate normaliser for rescaling Pois_dev_cum inverse values, and
### calculate probability by multiplying Pois_dev_cum inverse values by normaliser
knots_best <- knots_best %>% 
  mutate(Pois_dev_cum_inv = 1 / Pois_dev_cum,
         Norm = 1 / sum(Pois_dev_cum_inv), 
         Prob_unequal = Pois_dev_cum_inv * Norm) %>%
  select(-c(Pois_dev_cum_inv, Norm)) 

# Calculate minimum number of simulation runs per knot date required to retain relative probability of each
### (find the probability of the least likely knot date,
### calculate the multiplier required for this knot date to be simulated once, and
### multiply the probability of each knot date by multiplier)
knots_best <- knots_best %>% mutate(Prob_min = min(Prob_unequal),
                                    Mult = ceiling(1 / Prob_min),
                                    Min_n_unequal = round(Prob_unequal * Mult)) %>%
  select(-c(Prob_min, Mult))

# Calculate median growth factors for each country among best knots
median_growth_factors <- knots_best %>% summarise(Median_growth_factor_1 = median(Growth_factor_1, na.rm = TRUE),
                                                  Median_growth_factor_2 = median(Growth_factor_2, na.rm = TRUE),
                                                  Median_growth_factor_3 = median(Growth_factor_3, na.rm = TRUE),
                                                  .groups = "keep")

## SAVE all output -------------------------------------------------------------

# Export knots_best dataframe
write_csv(knots_best, file = paste0(results_directory, "knots_best.csv"))

# Create list of countries for which best knot points could be estimated
# (i.e. those which can be modelled) and save
countries_eur_modelled <- knots_best %>% pull(Country) %>% unique %>% as.list
save(countries_eur_modelled, file = paste0(results_directory, "countries_eur_modelled.RData"))

# Export median growth factors
write_csv(median_growth_factors, file = paste0(results_directory, "median_growth_factors.csv"))

# ------------------------------------------------------------------------------
# Calculate possible counterfactual conditions
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to calculate all possible combinations of the number of days earlier
# the first restriction and lockdown can be estimated, given a list of knot dates
# Arguments:
# (1) country = country to estimate
# (2) knots = dataframe of knot points
# Returns: dataframe containing possible combinations
Calculate_Possible_Counterfactual_Days <- function(country, knots) {
  
  # Filter datasets by country
  knots_country <- knots %>% filter(Country == country) 
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Record simulation start date
  date_start <- summary_eur_country %>% pull(Date_start)
  
  # Record dates of first restriction and lockdown
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  
  # Calculate maximum number of knots in knots dataframe
  max_n_knots <- knots_country %>% pull(N_knots) %>% max(na.rm = TRUE) %>% 
    suppressWarnings
  
  # Calculate possible counterfactuals
  if (max_n_knots == 2) {  # evidence of two unique intervention effects
    
    # Calculate maximum number of days earlier we can estimate first restriction
    # (minimum value of knot_date_1 greater than or equal to date_start)
    max_days_counterfactual_first_restriction <- knots_country %>% 
      mutate(Diff = Knot_date_1 - date_start) %>% pull(Diff) %>% min %>% as.numeric 
    
    # Calculate minimum date we can estimate first restriction
    min_date_first_restriction <- date_first_restriction - max_days_counterfactual_first_restriction
    
    # Calculate minimum date we can estimate lockdown (must be after date of first restriction)
    min_date_lockdown <- min_date_first_restriction + 1
    
    # Determine all possible combinations of dates for first restriction and lockdown
    # (first restriction must be before lockdown)
    possible_dates_counterfactual <- expand_grid(Date_first_restriction = seq.Date(min_date_first_restriction, date_first_restriction, 1),
                                                 Date_lockdown = seq.Date(min_date_lockdown, date_lockdown, 1)) %>%
      filter(Date_first_restriction < Date_lockdown)
    
    # Calculate all possible combinations of counterfactual days for first restriction and lockdown, label
    possible_days_counterfactual <- possible_dates_counterfactual %>%
      mutate(N_days_first_restriction = as.numeric(date_first_restriction - Date_first_restriction),
             N_days_lockdown = as.numeric(date_lockdown - Date_lockdown)) 
    
  } else if (max_n_knots == 1) {  # evidence of only one unique intervention effect
    
    # Calculate maximum number of days earlier we can estimate first restriction
    # (minimum value of knot_date_1 greater than or equal to date_start)
    max_days_counterfactual_first_restriction <- knots_country %>% 
      mutate(Diff = Knot_date_1 - date_start) %>% pull(Diff) %>% min %>% as.numeric 
    
    # Calculate minimum date we can estimate first restriction
    min_date_first_restriction <- date_first_restriction - max_days_counterfactual_first_restriction
    
    # Determine possible combinations of dates for first restriction and lockdown
    if (is.na(date_lockdown)) {  ## (no lockdown implemented)
      # Determine all possible dates for first restriction 
      # (lockdown date is NA)
      possible_dates_counterfactual <- tibble(Date_first_restriction = seq.Date(min_date_first_restriction, date_first_restriction, 1),
                                              Date_lockdown = as.Date(NA))
    } else if (date_first_restriction == date_lockdown) {  ## (lockdown implemented same day as first restriction)
      # Determine all possible dates for first restriction 
      # (lockdown date is equal to date of first restriction)
      possible_dates_counterfactual <- tibble(Date_first_restriction = seq.Date(min_date_first_restriction, date_first_restriction, 1),
                                              Date_lockdown = Date_first_restriction)
    } else {  ## (lockdown implemented, but didn't have unique effect on growth)
      # Calculate minimum date we can estimate lockdown (must be after date of first restriction)
      min_date_lockdown <- min_date_first_restriction + 1
      # Determine all possible combinations of dates for first restriction and lockdown
      # (first restriction must be before lockdown)
      possible_dates_counterfactual <- expand_grid(Date_first_restriction = seq.Date(min_date_first_restriction, date_first_restriction, 1),
                                                   Date_lockdown = seq.Date(min_date_lockdown, date_lockdown, 1)) %>%
        filter(Date_first_restriction < Date_lockdown)
    }
    
    # Determine all possible counterfactual days for first restriction
    possible_days_counterfactual <- possible_dates_counterfactual %>%
      mutate(N_days_first_restriction = as.numeric(date_first_restriction - Date_first_restriction),
             N_days_lockdown = as.numeric(date_lockdown - Date_lockdown)) 
    
  } else {  # none
    
    # Specify no combinations of counterfactual days are possible
    possible_days_counterfactual <- tibble(Date_first_restriction = as.Date(NA),
                                           Date_lockdown = as.Date(NA),
                                           N_days_first_restriction = as.numeric(NA),
                                           N_days_lockdown = as.numeric(NA)) 
    
  }
  
  # Label dataframe with country, max knots
  possible_days_counterfactual <- possible_days_counterfactual %>% 
    mutate(Country = country, Max_n_knots = max_n_knots) %>%
    arrange(N_days_first_restriction, N_days_lockdown) %>%
    relocate(Country, Max_n_knots, N_days_first_restriction, N_days_lockdown)
  
  # Return dataframe containing possible counterfactual days
  return(possible_days_counterfactual)
  
}

## Calculation -----------------------------------------------------------------

# Calculate possible counterfactuals
possible_days_counterfactual <- foreach(i = countries_eur_modelled,
                                        .errorhandling = "pass") %do%
  Calculate_Possible_Counterfactual_Days(country = i,
                                         knots = knots_best) %>%
  bind_rows %>% arrange(Country)

# Export dataframe containing possible counterfactual days
write_csv(possible_days_counterfactual, file = paste0(results_directory, "possible_days_counterfactual.csv"))

