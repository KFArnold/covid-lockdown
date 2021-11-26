#' Calculate within-country effects for all specified countries.
#' 
#' This function is largely a wrapper for the function 
#' 'Analyse_Effects_Within_Country', which calculates within-country effects for
#' a single country. 
#'
#' @param countries List of countries
#' @param out_folder Where to save within-country effects
#'
#' @return Named list of two dataframes:
#' (1) 'effects_within_countries', which contains all within-country effects 
#' for all \code{countries} individually; and
#' (2) 'effects_within_countries_summary', which contains summary information 
#' for all within-country effects (i.e. Median, IQR, and N_countries).
#'
#' @examples
#' Execute_Within_Country_Analysis(countries = list("United Kingdom", "Germany),
#' out_folder = "./Output/Between-country analysis/")
Execute_Within_Country_Analysis <- function(countries, out_folder) {
  
  # Create specified folder to save model fit statistics in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Calculate within-country effects for all designated countries,
  # and combine into single dataframe
  effects_within_countries <- foreach(j = countries,
          .errorhandling = "pass") %do%
    Analyse_Effects_Within_Country(country = j) %>%
    bind_rows %>%
    arrange(Country)
  
  # Calculate median percent change and quartiles across all countries
  # for each outcome in each simulation
  effects_within_countries_summary <- effects_within_countries %>%
    select(-Value) %>%
    group_by(History, Simulation, Outcome, Threshold) %>%
    summarise(Median_pct_change = median(Pct_change, na.rm = TRUE),
              QR1_pct_change = quantile(Pct_change, 0.25, na.rm = TRUE),
              QR3_pct_change = quantile(Pct_change, 0.75, na.rm = TRUE),
              N_countries = sum(!is.na(Pct_change)),
              .groups = "keep") %>% 
    ungroup
  
  # Save country-level and summary dataframes of within-country effects
  write_csv(effects_within_countries, 
            paste0(out_folder, "effects_within_countries.csv"))
  write_csv(effects_within_countries_summary, 
            paste0(out_folder, "effects_within_countries_summary.csv"))
  
  # Return country-level and summary dataframes of within-country effects
  return(list(effects_within_countries = effects_within_countries,
              effects_within_countries_summary = effects_within_countries_summary))
  
}
