#' Summarise mean and upper/lower centile values for a numeric vector
#'
#' @param x Vector of values
#' @param lower Value of lower centile
#' @param upper Value of upper centile
#' @param digits Number of digits to round output to
#'
#' @return List containing 3 values: (1) Mean; (2) C_lower; and (3) C_upper
#'
#' @examples
#' Summarise_Centiles(x = c(4, 3, 19, 4, 12, 11, 9))
Summarise_Centiles <- function(x, lower = 0.025, upper = 0.975, digits = 2) {
  
  # Remove values equal to NA or INF
  x <- x[!is.na(x) & !is.infinite(x)]
  
  # Calculate mean, lower and upper centiles
  mean <- mean(x) %>% round(digits = digits)
  c_lower <- quantile(x, lower, names = FALSE) %>% round(digits = digits)
  c_upper <- quantile(x, upper, names = FALSE) %>% round(digits = digits)
  
  # Return list of values
  return(list(Mean = mean,
              C_lower = c_lower,
              C_upper = c_upper))
  
}
