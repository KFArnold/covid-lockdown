#' Create table summarising estimated within-country effects.
#'
#' @param outcomes Vector of outcomes to include in table (from table
#' 'effects_within_countries_summary.csv')
#' @param n_decimals Number of decimals to include in output
#' @param out_folder Folder to save table in
#'
#' @return Formatted table of within-country effects for specified \code{outcomes}.
#'
#' @examples
#' Summary_Table_Effects_Within_Country(outcomes = c("Length_lockdown", "Total_cases"),
#' out_folder = "./Output/Tables/")
Summary_Table_Effects_Within_Country <- function(outcomes = c("Length_lockdown",
                                                              "Time_to_threshold",
                                                              "Total_cases"), 
                                                 n_decimals = 2,
                                                 out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import formatted file containing within-country effects
  data_formatted <- Format_Data_For_Plotting(filenames = c("effects_within_countries_summary"),
                                             silent = TRUE)
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Filter summary table containing within-country effects by specified outcomes, 
  # and with specified number of decimals
  effects_formatted <- effects_within_countries_summary_formatted %>%
    mutate(across(contains("pct"), ~100*.x), 
           across(contains("pct"), 
                  ~formatC(round(., digits = n_decimals), format = "f", digits = n_decimals))) %>%
    arrange(Outcome, History, Simulation, Threshold) %>%
    filter(Outcome %in% outcomes) %>%
    select(where(~sum(!is.na(.x)) > 0))
  
  # Combine median, QR1, and QR3 values into single column
  effects_formatted <- effects_formatted %>%
    unite(col = "QR1_QR3", c(QR1_pct_change, QR3_pct_change), sep = ", ") %>%
    mutate(QR1_QR3 = paste0("(", QR1_QR3, ")")) %>%
    unite(col = "Median_pct_change_QR1_QR3", c(Median_pct_change, QR1_QR3), sep = " ")
  
  # Arrange table
  effects_formatted <- effects_formatted %>% 
    relocate(N_countries, .after = last_col()) %>%
    arrange(Outcome, Simulation)
  
  # Save formatted table to specified folder
  write_csv(effects_formatted, 
            paste0(out_folder, "effects_within_countries_summary_formatted.csv"))
  
  # Return formatted table
  return(effects_formatted)
  
}
