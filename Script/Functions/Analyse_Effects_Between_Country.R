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
#' @param primary_covariates Vector containing covariates to use in primary analysis
#' @param secondary_covariates Vector containing additional covariates to use
#' in secondary analysis
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
                                        "Transformation" = c("Population", "log(Population)", "I(1/Population)")),
                                   list("Covariate" = "Gdp_usd",
                                        "Transformation" = c("Gdp_usd", "log(Gdp_usd)", "I(Gdp_usd/Population)")),
                                   list("Covariate" = "Health_expend_usd",
                                        "Transformation" = c("Health_expend_usd", "log(Health_expend_usd)", "I(Health_expend_usd/Population)")),
                                   list("Covariate" = "Population_urb",
                                        "Transformation" = c("Population_urb", "log(Population_urb)", "I(Population_urb/Population)")),
                                   list("Covariate" = "Population_0_14",
                                        "Transformation" = c("Population_0_14", "log(Population_0_14)", "I(Population_0_14/Population)")),
                                   list("Covariate" = "Population_15_64",
                                        "Transformation" = c("Population_15_64", "log(Population_15_64)", "I(Population_15_64/Population)")),
                                   list("Covariate" = "Population_65_up",
                                        "Transformation" = c("Population_65_up", "log(Population_65_up)", "I(Population_65_up/Population)"))),
           primary_covariates = c("Area_sq_km", "Population", "Gdp_usd"),
           secondary_covariates = c("Health_expend_usd", "Population_urb", 
                                    "Population_0_14", "Population_15_64", "Population_65_up"),
           out_folder) {
    
    # Define exposures and covariates
    exposures <- exposures_trans %>% map(., .f = ~.x$Exposure) %>% unlist
    covariates <- covariates_trans %>% map(., .f = ~.x$Covariate) %>% unlist
    
    # Print warning and stop if transformations haven't been specified for all primary 
    # and secondary convariates
    if (all(!covariates %in% c(primary_covariates, secondary_covariates))) {
      stop(cat("Transformations must be specified for ALL primary and secondary covariates",
               "via the parameter covariate_trans.",
               sep = "\n"))
    }
    
    # Import unloaded files into the global environment
    Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                            "summary_eur",
                                            "median_simulation_parameters"),
                              silent = TRUE)
    
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
    data_growth_factors <- median_simulation_parameters %>% 
      filter(Country %in% countries) %>% 
      group_by(Country) %>% 
      summarise(Median_growth_factor_lockdown = Median_growth_factor_3,
                .groups = "drop")
    
    # Combine all data for modelling into single dataframe, remove individual datasets
    data_model <- data_cases %>% 
      full_join(., data_covariates, by = "Country") %>%
      full_join(., data_length_lockdown, by = "Country") %>%
      full_join(., data_growth_factors, by = "Country") %>%
      mutate(ID = as.integer(rownames(.)))
    
    # Create empty list for storing effects from best-fitting models 
    # (both primary and secondary analyses)
    effects_all_analyses <- list()
    
    # Estimate effects from best-fitting models in both primary and secondary analyses
    foreach(analysis = c("Unadjusted", "Primary", "Secondary")) %do% {
      
      # Specify covariates to use in analysis, 
      # and determine all possible combinations of covariate transformations
      if (analysis == "Unadjusted") {
        
        # Covariates and covariate combinations
        analysis_covariates <- NA
        covariate_combinations <- NA
        
      } else {
        
        # Covariates
        if (analysis == "Primary") {
          
          analysis_covariates <- primary_covariates
          
        } else {  # (analysis == "Secondary")
          
          analysis_covariates <- c(primary_covariates, secondary_covariates)
          # (if both total population and population subset by age are specified, 
          # remove total population from list of all covariates)
          if ("Population" %in% primary_covariates &
              length(setdiff(c("Population_0_14", "Population_15_64", "Population_65_up"), 
                             secondary_covariates)) == 0) {
            analysis_covariates <- analysis_covariates[analysis_covariates != "Population"]
          } 
          
        }
        
        # Covariate combinations
        covariate_combinations <- covariates_trans %>%
          keep(., .p = ~all(.x$Covariate %in% analysis_covariates)) %>%
          map(., .f = ~.x$Transformation) %>%
          expand.grid %>%
          unite(., col = "Combination", sep = ", ", remove = TRUE) %>%
          pull(Combination)
        
      }
      
      # Create grid with all combinations of exposure, outcome, and covariates,
      # and define formula
      grid <- expand_grid(Outcome = outcomes, 
                          Exposure = map(.x = exposures_trans, .f = ~.x$Transformation) %>% unlist, 
                          Covariates = covariate_combinations) %>%
        mutate(Independent_vars = ifelse(!is.na(Covariates), 
                                         paste0(Exposure, ", ", Covariates),
                                         Exposure),
               Formula = paste(Outcome, " ~ ", gsub(", ", " + ", Independent_vars))) %>%
        select(-Independent_vars)
      
      # Evaluate each formula
      models <- map(.x = grid$Formula, .f = ~as.formula(.x)) %>%
        map(., .f = ~lm(.x, data = data_model))
      
      # Pull estimated effects and CI bounds from each formula
      effects <- map_dfr(.x = models, 
                         .f = ~tibble("Effect" = summary(.x)$coefficients[2, "Estimate"],
                                      "CI_lower" = confint(.x)[2, 1],
                                      "CI_upper" = confint(.x)[2, 2],
                                      "R_squared" = summary(.x)$r.squared,
                                      "BIC" = BIC(.x),
                                      "N_countries" = length(summary(.x)$residuals))) %>% 
        mutate(Leverage_points = "Included") %>%
        bind_cols(grid, .) %>% 
        select(-Formula)
      
      # Identify data points with high leverage
      leverage <- map(.x = models, .f = ~tibble(ID = High_Leverage_Points(model = .x)))
      
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
        map2_dfr(., .y = leverage_countries, 
                 .f = ~mutate(.x, Leverage_points_excluded = .y)) %>%
        mutate(Leverage_points = "Excluded") %>%
        relocate(Leverage_points, .before = Leverage_points_excluded) %>%
        bind_cols(grid, .) %>% 
        select(-Formula)
      
      # Bind all estimated effects together
      all_effects <- bind_rows(effects, effects_no_leverage) %>%
        arrange(Outcome, Exposure, Covariates)
      
      # Find best-fitting models for each combination of exposure and outcome,
      # and bind with corresponding models without leverage points
      best_effects <- map(.x = as.list(exposures),
                          .f = ~filter(effects, str_detect(Exposure, .x))) %>%
        map(., .f = ~group_by(., Outcome)) %>%
        map_dfr(., .f = ~filter(., BIC == min(BIC))) %>%
        split(., seq(nrow(.))) %>%
        map(., .f = ~select(., c(Outcome, Exposure, Covariates))) %>%
        map_dfr(., .f = ~left_join(., all_effects,
                                   by = c("Outcome", "Exposure", "Covariates"))) %>%
        ungroup %>%
        arrange(Outcome) 
      
      # Save table of estimated effects to list
      effects_all_analyses[[analysis]] <- best_effects
      
    }
    
    # Bind effects from all analyses into single dataframe and label
    effects_all_analyses <- bind_rows(effects_all_analyses, .id = "Analysis") %>%
      arrange(Outcome)
     
    # Save table of all estimated effects to designated folder
    write_csv(effects_all_analyses, 
              file = paste0(out_folder, "effects_between_countries.csv"))
    
    # Return table of best estimated effects
    return(effects_all_analyses)
    
  }
