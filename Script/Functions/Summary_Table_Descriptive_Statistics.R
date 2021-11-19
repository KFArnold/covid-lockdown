#' Create table of summary statistics (min, Q1, median, Q3, max, N) for 
#' outcomes, cases, and covariates used in analysis.
#' 
#' Note that this function also produces density and QQ-plots of specified 
#' untransformed and log-transformed variables. 
#'
#' @param countries List of countries to include
#' @param outcomes Vector of outcomes to summarise (default = "Length_lockdown")
#' @param dates_cases List containing pairs of dates and case types, describing
#' important dates and the types of cases to summarise on these dates
#' @param covariates Vector of covariates to summarise
#' @param n_decimals Number of decimals to include in statistics
#' @param out_folder Folder to save summary table, density and QQ-plots
#'
#' @return Named list of 3 items:
#' (1) 'density' = density plots of untransformed and log-transformed values
#' of specified \code{outcomes, dates_cases, covariates} for specified 
#' \code{countries}; 
#' (2) 'qq' = QQ-plots of untransformed and log-transformed values
#' of specified \code{outcomes, dates_cases, covariates} for specified 
#' \code{countries}; and
#' (3) 'summary' = summary table containing descriptive statistics for all
#' specified \code{outcomes, dates_cases, covariates} across all specified 
#' \code{countries}.
#'
#' @examples
#' Summary_Table_Descriptive_Statistics(countries = countries_eur,
#' out_folder = "./Output/Tables/")
Summary_Table_Descriptive_Statistics <- function(countries, 
                                                 outcomes = "Length_lockdown",
                                                 dates_cases = list(c("Date_lockdown", "Daily_cases_MA7"),
                                                                    c("Date_lockdown", "Cumulative_cases_beg"),
                                                                    c("Date_T", "Daily_cases_MA7"),
                                                                    c("Date_T", "Cumulative_cases_end")),
                                                 covariates = c("Area_sq_km", "Population",
                                                                "Gdp_usd", "Health_expend_usd",
                                                                "Population_0_14", "Population_15_64",
                                                                "Population_65_up", "Population_urb"),
                                                 n_decimals = 0,
                                                 out_folder) {
  
  # Create folder for saving output, if it doens't already exist
  Create_Folder_If_None_Exists(folder = out_folder, 
                               silent = TRUE)
  
  # Import files containing observed data and important dates
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "summary_eur"),
                            silent = TRUE)
  
  # Get data for all outcomes, exposures, and covariates for designated countries
  ## Exposures: Cases on dates of interest 
  data_cases <- dates_cases %>% 
    map(., .f = ~select(summary_eur, c(Country, .x[1]))) %>%
    map(., .f = ~full_join(.x, Cases_deaths_data_europe, by = "Country")) %>%
    map2(., .y = dates_cases, .f = ~select(., c(Country, contains("Date"), .y[2]))) %>%
    map2(., .y = dates_cases, .f = ~filter(., Date == eval(parse(text = .y[1])))) %>%
    map(., .f = ~select(., -contains("Date"))) %>% 
    map2(., .y = dates_cases, .f = ~mutate(., Date = .y[1])) %>%
    map(., .f = ~pivot_longer(.x, cols = contains("cases"), names_to = "Case_type", values_to = "Value")) %>%
    map(., .f = ~mutate(., Case_type = paste0(Case_type, "_", Date))) %>%
    map(., .f = ~select(., -Date)) %>%
    bind_rows %>%
    pivot_wider(., names_from = Case_type, values_from = Value) %>%
    filter(Country %in% countries)
  ## Covariates: Country stats
  data_covariates <- summary_eur %>% filter(Country %in% countries) %>% 
    select(Country, all_of(covariates)) 
  ## Outcomes
  data_outcomes <- summary_eur %>% filter(Country %in% countries) %>% 
    select(Country, all_of(outcomes))
  
  # Combine all data into single dataframe
  data_all <- data_cases %>% 
    full_join(., data_covariates, by = "Country") %>%
    full_join(., data_outcomes, by = "Country") 
  
  # Create density plots of raw and log-transformed variables
  density_raw <- data_all %>% keep(is.numeric) %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(Value)) +
    labs(title = "Untransformed values") +
    facet_wrap(~ Variable, scales = "free") + 
    geom_density()
  density_log <- data_all %>% keep(is.numeric) %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(log(Value))) +
    labs(title = "Log-transformed values") +
    facet_wrap(~ Variable, scales = "free") + 
    geom_density()
  density <- ggarrange(plotlist = list(density_raw, density_log), ncol = 2) %>%
    annotate_figure(top = text_grob("Density plots", size = 15))
  
  # Produce QQ-plots of raw and log-transformed variables
  qq_raw <- data_all %>% keep(is.numeric) %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(sample = Value)) +
    labs(title = "Untransformed values") +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ Variable, scales = "free") 
  qq_log <- data_all %>% keep(is.numeric) %>% 
    pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
    ggplot(aes(sample = log(Value))) +
    labs(title = "Log-transformed values") +
    stat_qq() +
    stat_qq_line() +
    facet_wrap(~ Variable, scales = "free") 
  qq <- ggarrange(plotlist = list(qq_raw, qq_log), ncol = 2) %>%
    annotate_figure(top = text_grob("QQ plots", size = 15))
  
  # Create summary table with specicied numver of decimals
  summary <- data_all %>% 
    pivot_longer(cols = -Country, names_to = "Variable", values_to = "Value") %>%
    group_by(Variable) %>% 
    summarise(across(Value, list(Min = ~min(., na.rm = TRUE),
                                 Q1 = ~quantile(., 0.25, na.rm = TRUE),
                                 Median = ~median(., na.rm = TRUE),
                                 Q3 = ~quantile(., 0.75, na.rm = TRUE),
                                 Max = ~max(., na.rm = TRUE),
                                 N = ~sum(!is.na(Value))),
                     .names = "{fn}"),
              .groups = "keep") %>%
    mutate(across(-N, ~formatC(round(., digits = n_decimals), 
                               format = "f", big.mark = ",", digits = n_decimals)))
  
  # Save formatted table and density/QQ plots to specified folder
  write_csv(summary, paste0(out_folder, "descriptive_statistics.csv"))
  ggsave(paste0(out_folder, "descriptive_statistics_density.png"),
         plot = density, width = 24, height = 11, limitsize = FALSE)
  ggsave(paste0(out_folder, "descriptive_statistics_qq.png"),
         plot = qq, width = 24, height = 11, limitsize = FALSE)
  
  # Return density plots, QQ plots, and summary table
  return(list(density = density,
              qq = qq,
              summary = summary))
  
}
