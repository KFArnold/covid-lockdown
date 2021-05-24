#' Calculate mean and SD of a distrubtion on the log scale.
#' 
#' Derivation described in: 
#' https://en.wikipedia.org/wiki/Log-normal_distribution#Arithmetic_moments.
#'
#' @param mean Arithmetic mean of distribution
#' @param sd Arithmetic standard deviation of distribution
#'
#' @return List with two items: (1) mean, and (2) standard deviation of 
#' distribution on the log scale
#'
#' @examples
#' Calculate_Parameters_Log(mean = 0, sd = 1)
Calculate_Parameters_Log <- function(mean, sd) {
  
  # Calculate mean and SD of distribution on log scale
  mean_log <- log(mean^2 / sqrt(sd^2 + mean^2))
  sd_log <- sqrt(log(1 + (sd^2 / mean^2)))
  
  # Return list of mean and SD on log scale
  return(list(mean = mean_log, 
              sd = sd_log))
  
}
