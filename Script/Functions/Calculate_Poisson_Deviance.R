#' Calculate Poisson deviance between observed and predicted values
#' 
#' Formula obtained from https://data.princeton.edu/wws509/notes/c4.pdf.
#'
#' @param obs Vector of observed values
#' @param pred Vector of predicted/simulated values
#'
#' @return Value of Poisson deviance
#'
#' @examples
#' Calculate_Poisson_Deviance(obs = c(0, 4, 6, 3, 4), pred = c(2, 1, 5, 3, 5))
Calculate_Poisson_Deviance <- function(obs, pred) {
  
  # Calculate Poisson deviance
  D <- 2 * sum(obs * log(obs / pred) - (obs - pred), na.rm = TRUE)
  
  # Return value of Poisson deviance
  return(D)
  
}
