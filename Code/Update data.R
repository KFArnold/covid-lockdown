# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script downloads most recent data from:

# (1) COVID-19 Data Repository by the Center for Systems Science and Engineering (CSSE) at Johns Hopkins University
# (2) Oxford Covid-19 Government Response Tracker

# Data is then saved to project repository.

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(readr)

# Define project directory where data is located
data_directory <- paste0("./Data/")

# ------------------------------------------------------------------------------
# Import data
# ------------------------------------------------------------------------------

## Cases and deaths data -------------------------------------------------------

# Set storage directory for saved data
out <- paste0(data_directory, "CSSE data/")

# Define web repository where CSSE data is located
source <- paste0("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/")

# Define data files to download
filenames <- list("time_series_covid19_confirmed_global.csv",
                  "time_series_covid19_deaths_global.csv")  # also data on recovered cases, if necessary

# Download data files and save to repository Data folder
for (i in filenames) {
  filename <- i
  write_csv(x = read_csv(url(paste0(source, filename))),
            path = paste0(out, filename))
}

## Policy data -----------------------------------------------------------------

# Set storage directory for saved data
out <- paste0(data_directory, "OxCGRT data/")

# Define web repository where OxCGRT data is located
source <- paste0("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/")

# Define data files to download
filenames <- list("OxCGRT_latest.csv")  

# Download data files and save to repository Data folder
for (i in filenames) {
  filename <- i
  write_csv(x = read_csv(url(paste0(source, filename)),
                         col_types = cols(RegionName = col_character(),
                                          RegionCode = col_character())),
            path = paste0(out, filename))
}

# Remove variables from environment
rm(data_directory, out, source, filenames)
