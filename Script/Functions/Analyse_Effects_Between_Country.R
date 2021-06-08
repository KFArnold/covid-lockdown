#' Analyse between-country effects for a list of countries.
#' 
#' The effects of interest are the impact of cases at lockdown on the length of
#' time spent under full lockdown and the median growth factor under lockdown.
#' Both unadjusted and adjusted effects are considered, and exposures/covariates
#' are considered in both untransformed and transformed (e.g. logged) values.
#' Models are run with and without points of high leverage.
#' The function selects the best-fitting (adjusted) models for each combination 
#' of specified exposure and outcome by lowest BIC.
#'
#' @param countries List of countries to include in analysis
#' @param outcomes Vector of outcomes to consider (possible values = 
#' c("Length_lockdown", "Median_growth_factor_lockdown"), which is the default)
#' @param exposures_trans Multilevel list containing exposures to consider, and
#' all potential transformations of each exposure
#' @param covariates_trans Miltilevel list containing covariates to consider, and
#' all potential transformations of each covariate
#' @param out_folder Where to save between-country effects
#'
#' @return Table containing estimated effects and model fit statistics 
#' (BIC, R-squared) from best-fitting adjusted models, in addition to 
#' corresponding unadjusted models and models with points of high leverage excluded.
#'
#' @examples
#' Analyse_Effects_Between_Country(countries = countries_eur_lockdown,
#' out_folder = "./Output/")
Analyse_Effects_Between_Country <- 
  function(countries, 
           outcomes = c("Length_lockdown", "Median_growth_factor_lockdown"),
           exposures_trans = list(list("Exposure" = "Daily_cases_MA7",
                                       "Transformation" = c("log(Daily_cases_MA7)")),
                                  list("Exposure"= "Cumulative_cases_beg",
                                       "Transformation" = c("log(Cumulative_cases_beg)"))),
           covariates_trans = list(list("Covariate" = "Area_sq_km",
                                        "Transformation" = c("Area_sq_km", "log(Area_sq_km)", "I(1/Area_sq_km)")),
                                   list("Covariate"= "Population",
                                        "Transformation" = c("Population", "log(Population)"))),
           out_folder) {
    
    # Import unloaded files into the global environment
    Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                            "summary_eur",
                                            "median_growth_factors"),
                              silent = TRUE)
    
    # Define exposures and covariates
    exposures <- exposures_trans %>% map(., .f = ~.x$Exposure) %>% unlist
    covariates <- covariates_trans %>% map(., .f = ~.x$Covariate) %>% unlist
    
    # Get data for all outcomes, exposures, and covariates for designated countries
    ## Exposures: Cases on lockdown dates
    data_cases <- Cases_deaths_data_europe %>% 
      filter(Country %in% countries) %>% 
      left_join(., select(summary_eur, c(Country, Date_lockdown)), by = "Country") %>% 
      filter(Date == Date_lockdown) %>%
      select(Country, Date_lockdown, all_of(exposures))
    ## Covariates: Country stats
    data_covariates <- summary_eur %>% 
      filter(Country %in% countries) %>% 
      select(Country, all_of(covariates)) 
    ## Outcomes
    data_length_lockdown <- summary_eur %>% 
      filter(Country %in% countries) %>% 
      select(Country, Length_lockdown)
    data_growth_factors <- median_growth_factors %>% 
      filter(Country %in% countries) %>% 
      group_by(Country) %>% 
      summarise(Median_growth_factor_lockdown = Median_growth_factor_3,
                .groups = "keep") %>% ungroup
    
    # Combine all data for modelling into single dataframe, remove individual datasets
    data_model <- data_cases %>% 
      full_join(., data_covariates, by = "Country") %>%
      full_join(., data_length_lockdown, by = "Country") %>%
      full_join(., data_growth_factors, by = "Country") %>%
      mutate(ID = rownames(.))
    
    # Determine all possible combinations of covariate transformations
    covariate_combinations <- covariates_trans %>% 
      map(., .f = ~.x$Transformation) %>%
      expand.grid %>%
      unite(., col = "Combination", sep = ", ", remove = TRUE) %>%
      pull(Combination)
    
    # Create grid with all combinations of exposure, outcome, and covariates,
    # and define formula
    grid <- expand_grid(Outcome = outcomes, 
                        Exposure = map(.x = exposures_trans, .f = ~.x$Transformation) %>% unlist, 
                        Covariates = c(NA, covariate_combinations)) %>%
      mutate(Independent_vars = ifelse(!is.na(Covariates), 
                                       paste0(Exposure, ", ", Covariates),
                                       Exposure),
             Formula = paste(Outcome, " ~ ", gsub(", ", " + ", Independent_vars))) %>%
      select(-Independent_vars)
    
    # Evaluate each formula
    models <- map(.x = grid$Formula, .f = ~as.formula(.x)) %>%
      map(~lm(.x, data = data_model))
    
    # Pull estimated effects and CI bounds from each formula
    effects <- map(.x = models, 
                   .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                                "CI_lower" = confint(.x)[2, 1],
                                "CI_upper" = confint(.x)[2, 2],
                                "R_squared" = summary(.x)$r.squared,
                                "BIC" = BIC(.x),
                                "N_countries" = length(summary(.x)$residuals))) %>% 
      reduce(bind_rows) %>%
      mutate(Leverage_points = "Included") %>%
      bind_cols(grid, .) %>% 
      select(-Formula)
    
    # Identify data points with high leverage
    leverage <- map(.x = models, .f = ~car::influencePlot(.x)) %>%
      map(.f = ~tibble(ID = (rownames(.x))))
    
    # Create vector of countries with high leverage
    leverage_countries <- leverage %>%
      map(., .f = ~left_join(.x, data_model, by = "ID")) %>%
      map(., .f = ~pull(.x, Country)) %>%
      map(., .f = ~paste(.x, collapse = ", "))
    
    # Re-run models without points of high leverage
    models_no_leverage <- leverage %>%
      map(., .f = ~anti_join(data_model, .x, by = "ID")) %>%
      map2(.y = grid$Formula, .f = ~lm(.y, data = .x)) 
    
    # Pull estimated effects and CI bounds from each formula
    effects_no_leverage <- map(.x = models_no_leverage, 
                               .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                                            "CI_lower" = confint(.x)[2, 1],
                                            "CI_upper" = confint(.x)[2, 2],
                                            "R_squared" = summary(.x)$r.squared,
                                            "BIC" = BIC(.x),
                                            "N_countries" = length(summary(.x)$residuals))) %>% 
      map2(.y = leverage_countries, 
           .f = ~mutate(.x, Leverage_points_excluded = .y)) %>%
      reduce(bind_rows) %>%
      mutate(Leverage_points = "Excluded") %>%
      relocate(Leverage_points, .before = Leverage_points_excluded) %>%
      bind_cols(grid, .) %>% select(-Formula)
    
    # Bind all estimated effects together
    all_effects <- bind_rows(effects, effects_no_leverage) %>%
      arrange(Outcome, Exposure, Covariates)
    
    # Find best-fitting (adjusted) models for each combination of exposure and outcome,
    # and bind with corresponding unadjusted models and models without leverage points
    best_effects <- map(.x = as.list(exposures),
                        .f = ~filter(effects, str_detect(Exposure, .x))) %>%
      map(., .f = ~filter(., !is.na(Covariates))) %>%
      map(., .f = ~group_by(., Outcome)) %>%
      map(., .f = ~filter(., BIC == min(BIC))) %>%
      reduce(bind_rows) %>%
      split(., seq(nrow(.))) %>%
      map(., .f = ~select(., c(Outcome, Exposure, Covariates))) %>%
      map(., .f = ~bind_rows(., tibble(Outcome = .x$Outcome,
                                       Exposure = .x$Exposure,
                                       Covariates = NA))) %>%
      map(., .f = ~left_join(., all_effects,
                             by = c("Outcome", "Exposure", "Covariates"))) %>%
      reduce(bind_rows) %>%
      arrange(Outcome)
    
    # Save table of best estimated effects to designated folder
    write_csv(best_effects, 
              file = paste0(out_folder, "effects_between_countries.csv"))
    
    # Return table of best estimated effects
    return(best_effects)
    
  }
