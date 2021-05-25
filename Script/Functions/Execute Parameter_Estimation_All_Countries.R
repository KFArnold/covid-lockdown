#' Execute parameter estimation for all specified countries.
#' 
#' This code is largely a wrapper for the function 'Determine_Best_Knots',
#' which determines the best pairs of knot dates and associated growth parameters 
#' for a given country. 
#'
#' @param countries List of countries
#' @param criteria Criteria by which to select best knot dates; one of
#' c("Pois_dev_inc", "Pois_dev_cum")
#' @param n_best Number of best knots to select 
#' @param parallel Whether the simulations should be run in parallel (T/F)
#'
#' @return Dataframe containing best knot date pairs and associated growth
#' parameters for all \code{countries}.
#'
#' @examples
#' Execute_Parameter_Estimation_All_Countries(countries = list("Germany", "United Kingdom"),
#' criteria = "Pois_dev_inc", n_best = 10, parallel = TRUE)
Execute_Parameter_Estimation_All_Countries <- function(countries, criteria, n_best,
                                                       parallel) {
  
  # Set up parallelisation, if specified
  if (parallel == TRUE) {
    n_cores <- parallel::detectCores()
    cluster <- parallel::makeCluster(n_cores[1] - 1, setup_strategy = "sequential")
    registerDoSNOW(cluster)
    parallel::clusterExport(cl = cluster, varlist = ls(.GlobalEnv), envir = .GlobalEnv)
  }
  
  # Set up progress bar
  iterations <- length(countries)
  progress_bar <- txtProgressBar(min = 1, max = iterations, style = 3)
  progress <- function(n) { setTxtProgressBar(progress_bar, n) }
  options <- list(progress = progress)
  
  # Determine best knot date pairs and associated growth parameters for all countries
  knots_best <- foreach(j = countries, 
                        .errorhandling = "pass", 
                        .packages = c("tidyverse", "lspline", "forecast"), 
                        .options.snow = options) %dopar% 
    Determine_Best_Knots(country = j, 
                         criteria = criteria,
                         n_best = n_best)
  
  # Stop parallel processing
  if (parallel == TRUE) {
    stopCluster(cluster) 
  }
  
  # Combine summary results for all countries, arrange by country
  knots_best <- knots_best %>%
    map(., .f = ~.x$knots_best) %>% reduce(bind_rows) %>% arrange(Country) 
  
  # Return summary dataframe
  return(knots_best)
  
}
