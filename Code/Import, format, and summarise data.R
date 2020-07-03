# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script imports/formats COVID-19 cases, deaths, and policies data 
# for countries around the world, and selects a subset of European countries to be analysed 
# (those with >=1000 cases by 1 June, and for which there is policies data available).

# It then produces a table which summarises the state of the pandemic in each country:
# (1) First date at which each country exceeded 100 cases (and cases/deaths on this day);
# (2) First date at which any restriction was imposed (and cases/deaths on this day);
# (3) Date at which the country entered lockdown (and cases/deaths on this day).
# This summary table is exported to the project folder


# Notes:
# May have to impute some values (look at Peter's code) - find NA's
# Need to take into account whether restrictions are targeted or general?
# don't need beginning and end vars in summaries df?

# will need to deal with countries with no important dates
# OR with only 1 (because both criteria met on same day, or only 1 criteria met)

# find countries which didn't implement restrictions or lockdowns
# Create data_eur_100 dataset?


# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(readr); library(dplyr); library(ggplot2)

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

# Function to find the first row (index) for which a group of measures
# (each with their own cutoffs) exceeds a particular threshold
# Arguments: DATA = full policies dataframe (i.e. contains all measures) for ONE country;
# MEASURES = a list of the measures to evaluate;
# CUTOFFS = a list of the cutoff points corresponding to each individual measure;
# THRESHOLD = the total number of measures that must be met/exceeded
# Returns row number (index) of first instance where threshold is met/exceeded
Find_Beginning_Index <- function(DATA, MEASURES, CUTOFFS, THRESHOLD) {
  
  # Print warning of each measure doesn't have its own cutoff point
  if (length(MEASURES) != length(CUTOFFS)) {
    print("Error: Each measure must have its own cutoff point.")
  }
  
  # Subset dataset to include only selected Measures
  data <- DATA %>% ungroup() %>% select(unlist(MEASURES))
  
  # For each measure, determine whether it is greater than or equal to its cutoff
  # If yes, assign 1; if no, assign 0
  for (i in 1:length(MEASURES)) {
    measure <- MEASURES[[i]]
    data[, measure] <- ifelse(data[, measure] >= CUTOFFS[[i]], 1, 0)
  }
  
  # Calculate row sums & find first instance where sum >= THRESHOLD
  sum <- apply(data[, unlist(MEASURES)], 1, sum)
  index <- suppressWarnings(min(which(sum >= THRESHOLD), na.rm = TRUE))
  
  # Return row index
  return(index)
  
}

# ------------------------------------------------------------------------------
# Import and format data
# ------------------------------------------------------------------------------

# Load data
## (1) CSSE data (cumulative cases and deaths)
cases <- read_csv("./Data/CSSE data/time_series_covid19_confirmed_global.csv")
deaths <- read_csv("./Data/CSSE data/time_series_covid19_deaths_global.csv")
## (2) OxCGRT data (government policies)
policies <- read_csv("./Data/OxCGRT data/OxCGRT_latest.csv") %>% 
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
policies <- policies %>% rename(c(Country = CountryName)) %>% 
  select(Country:C8_International_travel_controls) %>% #select(-contains("Flag")) %>%
  mutate_if(is.character, as.factor) %>%
  group_by(Country) %>% arrange(Country, Date)

# Calculate daily cases and deaths
cases <- cases %>% mutate(Cumulative_cases_beg = lag(Cumulative_cases_end, n = 1, default = 0),
                          Daily_cases = Cumulative_cases_end - Cumulative_cases_beg) %>%
  relocate(c(Cumulative_cases_beg, Daily_cases), .before = Cumulative_cases_end)
deaths <- deaths %>% mutate(Cumulative_deaths_beg = lag(Cumulative_deaths_end, n = 1, default = 0), 
                            Daily_deaths = Cumulative_deaths_end - Cumulative_deaths_beg) %>%
  relocate(c(Cumulative_deaths_beg, Daily_deaths), .before = Cumulative_deaths_end)

# Merge cases and deaths datasets into single dataframe
data_all <- full_join(cases, deaths) 
rm(cases, deaths)  # (remove separate datasets)

# Create variables for: date of first case (Date_0), 
# date at which cases first exceeded 100 (Date_100), and 
# number of days since 100 cases (Days_since_100)
data_all <- data_all %>% mutate(Date_0 = Date[which(Daily_cases >= 1)[1]]) %>% 
  mutate(Date_100 = Date[which(Cumulative_cases_beg >= 100)[1]]) %>% 
  mutate(Days_since_100 = as.numeric(Date - Date_100)) 

# Rename Czech Republic and Slovak Republic in policies dataset
policies <- policies %>% mutate(Country = recode(Country, "Czech Republic" = "Czechia"),
                                Country = recode(Country, "Slovak Republic" = "Slovakia"))

# Retain data for countries in Europe only
# and remove dependencies (of the Netherlands, UK, France, and Denmark)
# and drop unused levels
data_eur <- data_all %>% filter(Country %in% countries) %>% ungroup(Province_State) %>%
  filter(is.na(Province_State)) %>% select(-Province_State) %>% droplevels
policies_eur <- policies %>% filter(Country %in% countries) %>% droplevels

# ------------------------------------------------------------------------------
# Determine countries to be analysed
# ------------------------------------------------------------------------------

# Define end date for inclusion
date_T <- as.Date("2020-06-01")

# Define European countries with >=1000 total cases by date_T
countries_eur <- filter(data_eur, Date == date_T, 
                        Cumulative_cases_end >= 1000)$Country %>% as.character %>% as.list

# Retain only data from European countries with >=1000 total cases by 1 June
data_eur <- data_eur %>% filter(Country %in% countries_eur) %>% droplevels
policies_eur <- policies_eur %>% filter(Country %in% countries_eur) %>% droplevels

# Create list of final European countries for analysis:
# Countries with >=1000 total cases for which we have policy data
countries_eur_final <- as.list(intersect(levels(data_eur$Country), levels(policies_eur$Country)))
if (length(countries_eur_final) != length(countries_eur)) {
  unavail <- setdiff(levels(data_eur$Country), levels(policies_eur$Country))
  cat(paste0("Note that the following countries satisfied the inclusion criteria
but will be excluded from subsequent analyses because there is 
no policy data available:\n", 
              unavail))
  rm(unavail)
}  # (print note about excluded countries)

# Create dataframes for final list of countries to be analysed
data_eur_final <- data_eur %>% filter(Country %in% countries_eur_final) %>% droplevels
policies_eur_final <- policies_eur %>% filter(Country %in% countries_eur_final) %>% droplevels

# Create copy of dataframe where cumulative cases >= 100 and up to date_T
#data_eur_final_100 <- data_eur_final %>% filter(Date >= Date_100 & Date <= date_T)

# Remove all non-final dataframes and lists
rm(countries, countries_eur)
rm(data_all, data_eur, policies, policies_eur)

# ------------------------------------------------------------------------------
# Summarise countries 
# ------------------------------------------------------------------------------

# Create summary table which defines first date at which cases first exceeded 100 (Date_100)
summary_eur_final <- data_eur_final %>% filter(Date == Date_100) %>% 
  select(-c(Date_100, Days_since_100, Cumulative_cases_end, Cumulative_deaths_end)) %>%
  rename_at(vars(-c(Country, Date_0)), .funs = list(~ paste0(., "_100"))) %>%
  relocate(Date_0, .before = Date_100)

# Create empty summary tables to store:
# (1) Date of first restriction (including cases and deaths)
summary_first_restriction <- data_eur_final %>% select(Country, Date, contains(c("Cumulative", "Daily"))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_first_restriction"))) 
summary_first_restriction <- summary_first_restriction[0, ]
# (2) Date of lockdown (including cases and deaths)
summary_lockdown <- data_eur_final %>% select(Country, Date, contains(c("Cumulative", "Daily"))) %>%
  rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown"))) 
summary_lockdown <- summary_lockdown[0, ]


# Define requirements of interest and cutoff points for whether measures have been implemented
# [1 corresponds to measure recommended, 2-3 corresponds to measure required]
## (1) Lockdown measure
measures_lockdown <- list("C6_Stay_at_home_requirements")
cutoffs_lockdown <- list(2)
length(measures_lockdown) == length(cutoffs_lockdown)
## (2) Alternate group of measures which together are broadly equivalent to lockdown
measures_lockdown_alt <- list("C1_School_closing", "C2_Workplace_closing", "C3_Cancel_public_events",
                              "C5_Close_public_transport", "C7_Restrictions_on_internal_movement")
cutoffs_lockdown_alt <- list(2, 2, 2, 2, 2)
length(measures_lockdown_alt) == length(cutoffs_lockdown_alt)
## (3) Full group of measures (i.e. any restrictions)
measures_any_restriction <- as.list(sort(unlist(c(measures_lockdown, measures_lockdown_alt))))
cutoffs_any_restriction <- list(1, 1, 1, 1, 1, 1)
length(measures_any_restriction) == length(cutoffs_any_restriction)


# Record country summary data
for (i in 1:nrow(summary_eur_final)) {
  
  # Define country
  country <- summary_eur_final[[i, "Country"]] %>% as.character()
  
  # Filter cases/deaths and policies datasets by country
  policies_eur_i <- policies_eur_final %>% filter(Country == country)
  data_eur_i <- data_eur_final %>% filter(Country == country)
  
  # Summarise first restriction --------
  
  # Find first instance where any measures were recommended
  index <- Find_Beginning_Index(DATA = policies_eur_i,
                                MEASURES = measures_any_restriction,
                                CUTOFFS = cutoffs_any_restriction,
                                THRESHOLD = 1)
  
  # Analyse countries for which any measures were recommended
  if (index != Inf) {
    # Find date of first restriction, cumulative cases and deaths at that date
    date_first_restriction <- policies_eur_i[[index, "Date"]]
    # Create summary of cases/deaths on date of first restriction
    summary_first_restriction_i <- data_eur_i %>% filter(Date == date_first_restriction) %>% 
      select(Country, Date, contains(c("Cumulative", "Daily"))) %>%
      rename_at(vars(-Country), .funs = list(~ paste0(., "_first_restriction"))) 
    # Merge with full first restriction summary dataset
    summary_first_restriction <- bind_rows(summary_first_restriction, summary_first_restriction_i)
  }
  
  # Summarise lockdown --------
  
  # Find first instance where either:
  # (1) lockdown (general or targeted) was required; or
  # (2) >= 2 alternate lockdown measures were required
  index <- min(Find_Beginning_Index(DATA = policies_eur_i,
                                    MEASURES = measures_lockdown,
                                    CUTOFFS = cutoffs_lockdown,
                                    THRESHOLD = 1),
               Find_Beginning_Index(DATA = policies_eur_i,
                                    MEASURES = measures_lockdown_alt,
                                    CUTOFFS = cutoffs_lockdown_alt,
                                    THRESHOLD = 2),
               na.rm = TRUE)
  
  # Analyse countries for which any measures were recommended
  if (index != Inf) {
    # Find date of lockdown, cumulative cases and deaths at beginning of lockdown
    date_lockdown <- policies_eur_i[[index, "Date"]]
    # Create summary of cases/deaths on date of lockdown
    summary_lockdown_i <- data_eur_i %>% filter(Date == date_lockdown) %>% 
      select(Country, Date, contains(c("Cumulative", "Daily"))) %>%
      rename_at(vars(-Country), .funs = list(~ paste0(., "_lockdown"))) 
    # Merge with full lockdown summary dataset
    summary_lockdown <- bind_rows(summary_lockdown, summary_lockdown_i)
  }
  
}
# Remove measures/cutoffs, loop objects
rm(measures_lockdown, measures_lockdown_alt, measures_any_restriction,
   cutoffs_lockdown, cutoffs_lockdown_alt, cutoffs_any_restriction)
rm(i, data_eur_i, policies_eur_i, summary_first_restriction_i, summary_lockdown_i, 
   index, country, date_lockdown, date_first_restriction)

# Combine all summary datasets; remove separate dataframes
summary_eur_final <- full_join(summary_eur_final, summary_first_restriction, by = "Country") %>%
  full_join(., summary_lockdown, by = "Country") %>% ungroup
rm(summary_first_restriction, summary_lockdown)

# Export summary table
#write_csv(summary_eur_final, path = paste0(out, "Country summaries.csv"))

## Figures ---------------------------------------------------------------------

# Import data and set output storage directory if required
# summary_eur_final <- read_csv("./Results/Country summaries.csv") %>% mutate_if(is.character, as.factor)
# out <- paste0("./Results/")

# Order countries by date of first restriction
countries_ordered <- summary_eur_final %>% arrange(Date_first_restriction) %>% 
  pull(Country) %>% as.character

## (1) Figure - with dates of first restriction and lockdown -------------------

# Get min and max dates from summary table
date_min <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = min(Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull
date_max <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = max(Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull

# Define colours and shapes (for first restriction and lockdown)
cols <- c("col1" = "navyblue", "col2" = "darkorange")
shapes <- c("sh1" = 15, "sh2" = 18)

plot_1 <- ggplot(data = summary_eur_final, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Date when: first restriction imposed, lockdown imposed",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_first_restriction, color = "col1", shape = "sh1"), size = 3) +
  geom_point(aes(x = Date_lockdown, color = "col2", shape = "sh2"), size = 3) +
  scale_color_manual(name = "Date of...",
                     breaks = c("col1", "col2"),
                     values = cols,
                     labels = c("first restriction", "lockdown")) +
  scale_shape_manual(name = "Date of...",
                     breaks = c("sh1", "sh2"),
                     values = shapes,
                     labels = c("first restriction", "lockdown")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 4, date_max + 4), 
               date_breaks = "2 days", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_1

# Save plot
#ggsave(paste0(out, "Figure - Important dates.png"),
#       plot = plot_1, width = 9, height = 8)

## (2) Figure - including first case and cases >= 100 --------------------------

# Get min and max dates
date_min <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = min(Date_0, Date_100, Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull
date_max <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = max(Date_0, Date_100, Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull

# Define colours and shapes 
# (for first restriction, lockdown, first case, and cases >= 100)
cols <- c("col1" = "navyblue", "col2" = "darkorange", "col3" = "grey80", "col4" = "grey50")
shapes <- c("sh1" = 15, "sh2" = 18, "sh3" = 1, "sh4" = 1)

plot_2 <- ggplot(data = summary_eur_final, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Date when: first restriction imposed, lockdown imposed, first case recorded, 100th case recorded",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)
       and Johns Hopkins COVID-19 Data Repository (https://github.com/CSSEGISandData/COVID-19).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_first_restriction, color = "col1", shape = "sh1"), size = 3) +
  geom_point(aes(x = Date_lockdown, color = "col2", shape = "sh2"), size = 3) +
  geom_point(aes(x = Date_0, color = "col3", shape = "sh3"), size = 3) +
  geom_point(aes(x = Date_100 - 1, color = "col4", shape = "sh4"), size = 3) +
  scale_color_manual(name = "Date of...",
                     breaks = c("col1", "col2", "col3", "col4"),
                     values = cols,
                     labels = c("first restriction", "lockdown", "first confirmed case", "100th confirmed case")) +
  scale_shape_manual(name = "Date of...",
                     breaks = c("sh1", "sh2", "sh3", "sh4"),
                     values = shapes,
                     labels = c("first restriction", "lockdown", "first confirmed case", "100th confirmed case")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 4, date_max + 4), 
               date_breaks = "2 days", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_2

# Save plot
#ggsave(paste0(out, "Figure - Important dates (including first and 100th cases).png"),
#       plot = plot_2, width = 12, height = 8)

# Remove plotting objects from environment
rm(countries_ordered, date_min, date_max, cols, shapes, plot_1, plot_2)
