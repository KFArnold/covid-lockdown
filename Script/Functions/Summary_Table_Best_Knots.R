#' Create table summarising best knots and associated growth parameters for
#' a group of countries.
#'
#' @param countries List of countries
#' @param n_decimals Number of decimals to include in output
#' @param out_folder Folder to save table in
#'
#' @return Summary table of best knots for all \code{countries}.
#'
#' @examples
#' Summary_Table_Best_Knots(countries = c("United Kingdom", "Germany"),
#' out_folder = "./Output/Tables/")
Summary_Table_Best_Knots <- function(countries, 
                                     n_decimals = 3, 
                                     out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import file containing best knots
  Import_Unloaded_CSV_Files(filenames = "knots_best",
                            silent = TRUE)
  
  # Filter best knots dataframe by specified countries
  knots <- knots_best %>% 
    filter(Country %in% countries) %>%
    arrange(match(Country, all_of(countries))) %>%
    select(Country, Date_first_restriction, Date_lockdown, 
           Knot_date_1, Knot_date_2, contains("Growth"), Prob_unequal) %>%
    mutate(across(where(is.numeric), ~round(., digits = n_decimals)))
  
  # Collapse combine growth factors and associated SDs into same cell
  knots <- knots %>% 
    mutate(Growth_factor_1 = paste0(Growth_factor_1, " (", Growth_factor_1_sd, ")"),
           Growth_factor_2 = paste0(Growth_factor_2, " (", Growth_factor_2_sd, ")"),
           Growth_factor_3 = paste0(Growth_factor_3, " (", Growth_factor_3_sd, ")")) %>%
    select(-contains("sd"))
  
  # Save formatted table and density/QQ plots to specified folder
  write_csv(knots, paste0(out_folder, "best_knots.csv"))
  
  # Return summary table
  return(knots)
  
}
