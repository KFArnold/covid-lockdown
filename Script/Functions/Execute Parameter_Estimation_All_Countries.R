#' Execute parameter estimation for all specified countries.
#' 
#' This code is largely a wrapper for the functions 'Determine_Best_Knots'
#' (which determines the best pairs of knot dates and associated growth parameters 
#' for a given country) and 'Calculate_Likelihood_Best_Knots' (which calculates
#' the relative likelihood of the best pairs according to a specified criteria). 
#'
#' @param countries List of countries
#' @param criteria_selection Criteria by which to select best knot dates; one of
#' c("Pois_dev_inc", "Pois_dev_cum")
#' @param criteria_likelihood Criteria by which to calculate the relative
#' likelihood of each of the best knot dates; one of c("Pois_dev_inc", "Pois_dev_cum")
#' @param n_best Number of best knots to select 
#' @param parallel Whether the simulations should be run in parallel (T/F)
#' @param out_folder Where to save resulting 'best_knots' dataframe 
#'
#' @return Dataframe containing \code{n_best} knot date pairs and associated growth 
#' parameters (as evaluated by \code{criteria_selection}) for all \code{countries}, 
#' including the relative likelihood of each pair (as evaluated by
#' \code{criteria_likelihood}).
#'
#' @examples
#' Execute_Parameter_Estimation_All_Countries(countries = list("Germany", "United Kingdom"),
#' criteria_selection = "Pois_dev_inc", criteria_likelihood = "Pois_dev_cum",
#' n_best = 10, parallel = TRUE)
Execute_Parameter_Estimation_All_Countries <- function(countries, 
                                                       criteria_selection, 
                                                       criteria_likelihood,
                                                       n_best,
                                                       parallel,
                                                       out_folder) {
  
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
                         criteria_selection = criteria_selection,
                         n_best = n_best)
  
  # Stop parallel processing
  if (parallel == TRUE) {
    stopCluster(cluster) 
  }
  
  # Combine summary results for all countries, arrange by country
  knots_best <- knots_best %>%
    map(., .f = ~.x$knots_best) %>% reduce(bind_rows) %>% arrange(Country)
  
  # Calculate both equal and unequal probabilities of each pair of knot points
  # based on specified criteria
  knots_best <- Calculate_Likelihood_Best_Knots(knots = knots_best,
                                                criteria_likelihood = criteria_likelihood,
                                                likelihood = c("Prob_equal", "Prob_unequal"))
  
  # Export knots_best dataframe to specified folder
  write_csv(knots_best, file = paste0(out_folder, "knots_best.csv"))
  
  # Return summary dataframe
  return(knots_best)
  
}
