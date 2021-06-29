#' Create table summarising estimated between-country effects.
#'
#' @param outcomes Vector of outcomes to include in table (from table
#' 'effects_between_countries.csv')
#' @param leverage_points Whether to present models with leverage points included
#' or excluded
#' @param n_decimals Number of decimals to include in output
#' @param out_folder Folder to save table in
#'
#' @return Formatted table of between-country effects for specified \code{outcomes}.
#'
#' @examples
#' Summary_Table_Effects_Between_Countries(outcomes = "Length_lockdown",
#' leverage_points = "Included", out_folder = "./Output/Tables/")
Summary_Table_Effects_Between_Country <- function(outcomes = c("Length_lockdown",
                                                               "Median_growth_factor_lockdown"),
                                                  leverage_points = c("Included",
                                                                      "Excluded"),
                                                  n_decimals = 2,
                                                  out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import formatted file containg between-country effects
  data_formatted <- Format_Data_For_Plotting(filenames = "effects_between_countries",
                                             silent = TRUE)
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Filter summary table containing between-country effects from best-fitting models
  # by specified outcomes, leverage points, and with specified number of decimals
  effects_formatted <- effects_between_countries_formatted %>%
    mutate(across(c(Effect:R_squared), 
                  ~formatC(round(., digits = n_decimals), format = "f", digits = n_decimals)),
           BIC = round(BIC, digits = 0)) %>%
    filter(Outcome %in% outcomes,
           Leverage_points %in% leverage_points) %>%
    arrange(Leverage_points, Outcome, Analysis, Exposure) %>%
    select(where(~sum(!is.na(.x)) > 0))
  
  # Combine effect, CI_lower, and CI_upper values into single column
  effects_formatted <- effects_formatted %>%
    unite(col = "CI", c(CI_lower, CI_upper), sep = ", ") %>%
    mutate(CI = paste0("(", CI, ")")) %>%
    unite(col = "Effect_CI", c(Effect, CI), sep = " ")
  
  # Save formatted table to specified folder
  write_csv(effects_formatted, 
            paste0(out_folder, "effects_between_countries_formatted.csv"))
  
  # Return formatted table
  return(effects_formatted)
  
}
