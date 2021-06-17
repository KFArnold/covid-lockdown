#' Function to draw a vector of random growth factors
#'
#' @param n Number of growth factors to draw
#' @param mean Arithmetic mean of growth factors
#' @param sd Arithmetic sd of growth factors
#' @param variation Whether growth factor(s) should be drawn from a distribution
#' with variation; if FALSE, all growth factor(s) returned are equal to the mean
#' @param log Whether growth factors should be drawn from a lognormal 
#' distribution (T/F); if FALSE, normal distribution is used
#'
#' @return Vector of growth factors drawn from specified distribution
#'
#' @examples
#' Draw_Random_Growth_Factors(n = 100, mean = 1, sd = 0, variation = TRUE, log = TRUE)
#' Draw_Random_Growth_Factors(n = 1, mean = 1, sd = 0, variation = FALSE, log = FALSE)
Draw_Random_Growth_Factors <- function(n, mean, sd, variation, log) {
  
  # If variation specified, draw n random growth factors from either a normal
  # or lognormal distribution; else, draw n growth factors equal to mean
  if (variation == TRUE) {
    
    if (log == TRUE) {
      
      # Calculate parameters of lognormal distribution
      log_parameters <- Calculate_Parameters_Log(mean = mean, sd = sd) %>%
        setNames(., paste0(names(.), "_log"))
      list2env(log_parameters, envir = environment())
      
      # Draw n random growth factors from a lognormal distribution
      growth_factors <- rlnorm(n = n, meanlog = mean_log, sdlog = sd_log)
      
    } else {
      
      # Draw n random growth factors from a normal distribution
      growth_factors <- rnorm(n = n, mean = mean, sd = sd)
      
    }
    
  } else {  # variation == FALSE
    
    # Draw n growth factors equal to mean (i.e. no variation)
    growth_factors <- rep(mean, n)
    
  }
  
  # Return vector of growth factors
  return(growth_factors)
  
  
}
