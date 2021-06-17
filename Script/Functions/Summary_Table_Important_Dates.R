#' Create table summarising important dates for a group of countries.
#'
#' @param countries List of countries to include
#' @param dates Vector of dates to include (from 'summary_eur.csv')
#' @param out_folder Folder to save summary table
#'
#' @return Summary table with one row per \code{countries} and 1 column per
#' specified \code{dates}.
#'
#' @examples
#' Summary_Table_Important_Dates(countries = c("United Kingdom", "Germany"),
#' out_folder = "./Output/Tables/")
Summary_Table_Important_Dates <- function(countries,
                                          dates = c("Date_1", 
                                                    "Date_first_restriction", 
                                                    "Date_lockdown", 
                                                    "Date_lockdown_eased",
                                                    "Date_start", 
                                                    "Date_T"),
                                          out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import file containing important dates
  Import_Unloaded_CSV_Files(filenames = "summary_eur",
                            silent = TRUE)
  
  # Filter summary table by specified countries and dates,
  # and convert dates to characters
  important_dates <- summary_eur %>%
    filter(Country %in% countries) %>%
    select(Country, all_of(dates)) %>%
    mutate(across(where(is.Date), ~strftime(., "%b %d %Y")))
  
  # Save formatted table to specified folder
  write_csv(important_dates, paste0(out_folder, "important_dates.csv"))
  
  # Return summary table
  return(important_dates)
  
}
