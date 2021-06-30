#' Create tables summarising within-country model fit statistics.
#'
#' @param measures Vector of model fit statistics to include (from 'model_fit.csv')
#' @param n_decimals Number of decimals to include in output
#' @param out_folder Folder to save table in
#'
#' @return List of two tables"
#' (1) 'model_fit_formatted' = formatted version of 'model_fit', which contains
#' specified model fit statistics for all countries individually; and
#' (2) 'model_fit_summary_formatted' = formatted version of 'model_fit_summary',
#' which contains summary of specified model fit statistics across all countries.
#'
#' @examples
#' Summary_Tables_Model_Fit(out_folder = "./Output/Tables/")
Summary_Tables_Model_Fit <- function(measures = c("Pois_dev_inc",
                                                  "Pois_dev_cum",
                                                  "Diff_total_cases",
                                                  "Diff_time_to_threshold"),
                                     n_decimals = 2, 
                                     out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import formatted files containing model fit statistics
  data_formatted <- Format_Data_For_Plotting(filenames = c("model_fit",
                                                           "model_fit_summary"),
                                             silent = TRUE)
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # (1) Country-specific model fit statistics:
  # Filter table of country-specific model fit statistics by specified measures,
  # and with specified number of decimals.
  # Star values which are outliers
  model_fit_filt <- model_fit_formatted %>%
    filter(Measure %in% measures) %>%
    mutate(across(Value, 
                  ~formatC(round(., digits = n_decimals), 
                           format = "f", big.mark = ",", digits = n_decimals))) %>%
    mutate(Value = ifelse(Outlier, paste0("*", Value), Value)) %>%
    arrange(Country, Threshold, Measure, Type) %>%
    select(-c(Pct_Rank, Outlier)) 
  # Pivot wider and combine number and percent values into single column (pct in parentheses)
  model_fit_filt <- model_fit_filt %>%
    pivot_wider(names_from = Type, values_from = Value) %>%
    mutate(across(c(Measure, Threshold), as.character),
           Threshold = str_replace_all(Threshold, c("," = "", " " = "_"))) %>%
    unite(col = "Measure", c(Measure, Threshold), na.rm = TRUE, sep = "_") %>%
    pivot_wider(names_from = Measure, names_glue = "{Measure}_{.value}",
                values_from = c(Number, Pct)) %>%
    select(Country, contains(measures)) %>%
    purrr::discard(~all(is.na(.))) %>%
    arrange(Country)
  
  # (2) Summary of model fit statistics:
  # Filter summary table of model fit statistics by specified measures,
  # and with specified number of decimals
  model_fit_summary_filt <- model_fit_summary_formatted %>%
    filter(Measure %in% measures) %>%
    mutate(across(c(Median, IQR), 
                  ~formatC(round(., digits = n_decimals), 
                           format = "f", big.mark = ",", digits = n_decimals))) %>%
    arrange(Type, Measure, Threshold) %>%
    select(where(~sum(!is.na(.x)) > 0))
  # Combine median and IQR values into single column and pivot wider
  model_fit_summary_filt <- model_fit_summary_filt %>%
    mutate(IQR = paste0("(", IQR, ")")) %>%
    unite(col = "Median_IQR", c(Median, IQR), sep = " ") %>%
    pivot_wider(names_from = Type, 
                values_from = Median_IQR,
                names_prefix = "Median_IQR_") %>%
    relocate(N_countries, .after = last_col())
  
  # Save formatted tables to specified folder
  write_csv(model_fit_filt, paste0(out_folder, "model_fit_formatted.csv"))
  write_csv(model_fit_summary_filt, paste0(out_folder, "model_fit_summary_formatted.csv"))
  
  # Return formatted table
  return(list(model_fit_formatted = model_fit_filt,
              model_fit_summary_formatted = model_fit_summary_filt))
  
}
