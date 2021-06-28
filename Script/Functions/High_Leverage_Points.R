#' Identify points of high leverage in a fitted model.
#'
#' @param model Fitted model
#' @param threshold Threshold specifying the number of times greater an 
#' observation's hat value can be compared to the average hat value before it is
#' considered a point of high leverage (default = 2)
#'
#' @return Numeric vector containg observation numbers deemed to have high leverage
#' (defined as observations with hat values greater than \code{threshold} times
#' the average hat value).
#'
#' @examples
High_Leverage_Points <- function(model, threshold = 2) {
  
  # Compute hat values from specified model
  hat_values <- hatvalues(model)
  
  # Compute mean hat value from specified model
  mean_hat_value <- mean(hat_values)
  
  # Determine which observations have hat values X times greater than average
  high_leverage_points <- subset(hat_values, hat_values > threshold*mean_hat_value) %>% 
    names %>% as.integer
  
  # Return vector of high leverage observations
  return(high_leverage_points)
  
}
