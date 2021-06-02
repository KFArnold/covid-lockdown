#' Calculate model fit assessement for all specified countries.
#' 
#' This code is largely a wrapper for the function 'Assess_Model_Fit', which 
#' calculates the values of various model fit statistics for a single country. 
#' It also calculates countries that are outliers with respect to individual model
#' fit metrics.
#'
#' @param countries List of countries
#' @param out_folder Where to save model fit statistics
#'
#' @return Named list of two dataframes:
#' (1) 'model_fit', which contains all model fit statistics for all \code{countries}; and
#' (2) 'model_fit_summary', which contains summary information for all model fit 
#' statistics (i.e. Median, IQR, and N_countries).
#'
#' @examples
#' Execute_Model_Fit_Assessment_All_Countries(countries = list("United Kingdom", "Germany),
#' out_folder = "./Output/Between-country analysis/")
Execute_Model_Fit_Assessment_All_Countries <- function(countries, out_folder) {
  
  # Create specified folder to save model fit statistics in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Calculate model fit statitsics for all countries,
  # and combine into single dataframe
  model_fit <- foreach(j = countries,
                           .errorhandling = "pass") %do%
    Assess_Model_Fit_All(country = j) %>%
    bind_rows %>%
    arrange(Country)
  
  # Calculate percentage rank for all model fit metrics
  model_fit <- model_fit %>%
    group_by(Measure, Type, Threshold) %>%
    mutate(Pct_Rank = percent_rank(abs(Value)))
  
  # Calculate outliers for all model fit statistics and label
  outliers <- model_fit %>% 
    group_split %>%
    map(., .f = ~boxplot(.x$Value)$out)
  model_fit <- model_fit %>%
    group_split %>%
    map2(., .y = outliers,
         .f = ~mutate(.x, Outlier = ifelse(.x$Value %in% .y, TRUE, FALSE))) %>%
    bind_rows %>%
    arrange(Country, Measure, Threshold)
  
  # Calculate summary statistics for all model fit statistics
  model_fit_summary <- model_fit %>% 
    group_by(Measure, Type, Threshold) %>%
    summarise(Median = median(Value, na.rm = TRUE),
              IQR = IQR(Value, na.rm = TRUE),
              N_countries = sum(!is.na(Value)),
              .groups = "keep") %>%
    relocate(Threshold, .after = Measure) %>%
    arrange(Measure, Threshold)
  
  # Save country-level and summary dataframes of model fit statistics
  write_csv(model_fit, paste0(out_folder, "model_fit.csv"))
  write_csv(model_fit_summary, paste0(out_folder, "model_fit_summary.csv"))
  
  # Return country-level and summary dataframes of model fit statistics
  return(list(model_fit = model_fit,
              model_fit_summary = model_fit_summary))
  
}
