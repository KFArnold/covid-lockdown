#' Format all source data.
#' 
#' The following data files are formatted:
#' (1) 'time_series_covid19_confirmed_global.csv', which contains time series 
#' data pertaining to confirmed cases of COVID-19 for countries worldwide;
#' (2) 'time_series_covid19_deaths_global.csv', which contains time series 
#' data pertaining to confirmed deaths from COVID-19 for countries worldwide;
#' (3) 'OxCGRT_latest.csv', which contains time series data pertaining to 
#' policy responses to COVID-19 for countries worldwide; and
#' (4) 'Worldbank_data.csv', which contains basic demographic statistics 
#' for a number of years for countries worldwide.
#'
#' @param countries List of countries to retain source data for
#' @param source_folder Folder where (unformatted) source data are located
#' @param out_folder Folder where formatted data should be saved
#'
#' @return Named list of 4 items:
#' (1) 'countries_eur' = list of \code{countries} for which there exists both
#' cases/deaths data and policy data;
#' (2) 'Cases_deaths_data_europe' = dataframe containing formatted cases/deaths 
#' data for all countries in \code{countries_eur};
#' (3) 'Policy_data_europe' = dataframe containing formatted policy data for all
#' countries in \code{countries_eur}; and
#' (4) 'Worldbank_data_europe' = dataframe containing formatted World Bank data
#' for all countries in \code{countries_eur}.
#' Note that the list 'countries_eur' is saved as an .RData file to the project's
#' Output folder (i.e. "./Output/"), and the three formatted dataframes are
#' saved as .csv files to the specified \code{out_folder}.
#'
#' @examples
#' Format_Source_Data(countries = list("Germany", "United Kingdom"),
#' source_folder = "./Data/Unformatted/", out_folder = "./Data/Formatted/")
Format_Source_Data <- function(countries, source_folder, out_folder) {
  
  # Create designated folder for saving formatted source data, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Load unformatted source data
  ## (1) CSSE data (cumulative cases and deaths)
  cases <- read_csv(paste0(source_folder, "CSSE data/time_series_covid19_confirmed_global.csv"))
  deaths <- read_csv(paste0(source_folder, "CSSE data/time_series_covid19_deaths_global.csv"))
  ## (2) OxCGRT data (government policies)
  policies <- read_csv(paste0(source_folder, "OxCGRT data/OxCGRT_latest.csv"),
                       col_types = cols(RegionName = col_character(),
                                        RegionCode = col_character())) %>% 
    mutate(Date = as.Date(as.character(Date), format = "%Y%m%d"))
  ## World Bank data 
  worldbank <- read_csv(paste0(source_folder, "World Bank data/Worldbank_data.csv"))
  
  # Convert datasets to long form, select relevant variables, 
  # rename country variable, convert characters to factors,
  # and order / group by Province and Country
  cases <- cases %>% 
    gather(Date, Cumulative_cases_end, -(1:4)) %>% 
    mutate(Date = as.Date(Date, format = "%m_%d_%y")) %>% 
    select(-c(Lat, Long)) %>% rename(c(Country = Country_Region)) %>% 
    mutate_if(is.character, as.factor) %>%
    group_by(Province_State, Country) %>% arrange(Country, Date)
  deaths <- deaths %>% 
    gather(Date, Cumulative_deaths_end, -(1:4)) %>% 
    mutate(Date = as.Date(Date, format = "%m_%d_%y")) %>% 
    select(-c(Lat, Long)) %>% rename(c(Country = Country_Region)) %>% 
    mutate_if(is.character, as.factor) %>%
    group_by(Province_State, Country) %>% arrange(Country, Date)
  policies <- policies %>% 
    rename(c(Country = CountryName,
             Province_State = RegionName)) %>% 
    select(c(Country, Province_State, Date, C1_School_closing:C8_International_travel_controls)) %>%
    mutate_if(is.character, as.factor) %>%
    group_by(Province_State, Country) %>% arrange(Country, Date)
  worldbank <- worldbank %>% 
    select(-contains("Iso")) %>%
    mutate_if(is.character, as.factor) %>% 
    group_by(Country) %>% arrange(Country)
  
  # Calculate daily cases and deaths
  cases <- cases %>% 
    mutate(Cumulative_cases_beg = lag(Cumulative_cases_end, n = 1, default = 0),
           Daily_cases = Cumulative_cases_end - Cumulative_cases_beg) %>%
    relocate(c(Cumulative_cases_beg, Daily_cases), .before = Cumulative_cases_end)
  deaths <- deaths %>% 
    mutate(Cumulative_deaths_beg = lag(Cumulative_deaths_end, n = 1, default = 0), 
           Daily_deaths = Cumulative_deaths_end - Cumulative_deaths_beg) %>%
    relocate(c(Cumulative_deaths_beg, Daily_deaths), .before = Cumulative_deaths_end)
  
  # Calculate 7-day moving averages of daily and cumulative cases and deaths
  cases <- cases %>% 
    mutate(Daily_cases_MA7 = round(runmean(Daily_cases, k = 7, alg = "C", endrule = "mean"), 3),
           Cumulative_cases_end_MA7 = cumsum(Daily_cases_MA7),
           Cumulative_cases_beg_MA7 = lag(Cumulative_cases_end_MA7, n = 1, default = 0)) %>%
    relocate(Cumulative_cases_beg_MA7, .before = Daily_cases_MA7)
  deaths <- deaths %>% 
    mutate(Daily_deaths_MA7 = round(runmean(Daily_deaths, k = 7, alg = "C", endrule = "mean"), 3),
           Cumulative_deaths_end_MA7 = cumsum(Daily_deaths_MA7),
           Cumulative_deaths_beg_MA7 = lag(Cumulative_deaths_end_MA7, n = 1, default = 0)) %>%
    relocate(Cumulative_deaths_beg_MA7, .before = Daily_deaths_MA7)
  
  # Merge cases and deaths datasets into single dataframe
  data_all <- full_join(cases, deaths, by = c("Province_State", "Country", "Date")) 
  
  # Remove data which cannot be reasonable assumed complete
  data_all <- data_all %>% 
    filter(Date <= max(Date) - 7)
  
  # Rename Czechia and Slovakia in cases/deaths dataset,
  # and Russian Federation in Worldbank data
  data_all <- data_all %>% 
    mutate(Country = recode(Country, "Czechia" = "Czech Republic"),
           Country = recode(Country, "Slovakia" = "Slovak Republic"))
  worldbank <- worldbank %>% 
    mutate(Country = recode(Country, "Russian Federation" = "Russia"))
  
  # Retain data for countries in Europe only
  # and remove dependencies (of the Netherlands, UK, France, and Denmark)
  # and drop unused levels
  data_eur <- data_all %>% 
    filter(Country %in% countries) %>% 
    ungroup(Province_State) %>%
    filter(is.na(Province_State)) %>% 
    select(-Province_State) %>% droplevels
  policies_eur <- policies %>% 
    filter(Country %in% countries) %>% 
    ungroup(Province_State) %>%
    filter(is.na(Province_State)) %>% 
    select(-Province_State) %>% droplevels
  
  # Create list of European countries for which we have both cases/deaths data and policy data,
  # and record list of unavailable countries
  countries_eur <- as.list(intersect(levels(data_eur$Country), levels(policies_eur$Country)))
  unavail <- setdiff(levels(data_eur$Country), levels(policies_eur$Country))
  
  # Retain data for European countries for which we have both cases/deaths data and policy data
  data_eur <- data_eur %>% 
    filter(Country %in% countries_eur) %>% droplevels
  policies_eur <- policies_eur %>% 
    filter(Country %in% countries_eur) %>% droplevels
  worldbank_eur <- worldbank %>% 
    filter(Country %in% countries_eur) %>% droplevels
  
  # Save formatted data to project directory
  write_csv(x = data_eur, file = paste0(out_folder, "Cases_deaths_data_europe.csv"))
  write_csv(x = policies_eur, file = paste0(out_folder, "Policy_data_europe.csv"))
  write_csv(x = worldbank_eur, file = paste0(out_folder, "Worldbank_data_europe.csv"))
  
  # Save list of European countries for which we have both cases/deaths data and policy data
  # to Output folder
  save(countries_eur, file = "./Output/countries_eur.RData")
  
  # Print messages re: (1) unavailable countries, and 
  # (2) formatted files saved to designated out folder
  cat("The following countries do not have both cases/deaths data and policy data available",
      "and have been discarded:", 
      paste(unavail, collapse = ", "),
      sep = "\n")
  cat(paste0("The following formatted files have been saved to ", out_folder, ":"),
      paste(c("Cases_deaths_data_europe.csv", "Policy_data_europe.csv", "Worldbank_data_europe.csv"), collapse = ", "),
      sep = "\n")
  
  # Return list of formatted files and list of countries for which we have
  # both cases/deaths data and policy data
  return(list(countries_eur = countries_eur,
              Cases_deaths_data_europe = data_eur,
              Policy_data_europe = policies_eur,
              Worldbank_data_europe = worldbank_eur))
  
}
