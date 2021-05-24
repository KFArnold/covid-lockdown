#' Calculate cumulative cases from an input matrix of incidence cases
#'
#' @param daily_cases Matrix of incidence data, with 1 row per simulation run
#' and 1 column per date
#' @param start_value Value of cumulative cases on first date
#'
#' @return Matrix of cumulative data, with 1 row per simulation run and 1
#' column per date
#'
#' @examples
Calculate_Cumulative_Cases <- function(daily_cases, start_value) {
  
  # Replace first column of daily_cases matrix with start_value
  daily_cases[, 1] <- start_value
  
  # Calculate cumulative sum
  cumulative_cases <- apply(X = daily_cases, MARGIN = 1, FUN = cumsum) %>% t
  
  # Return matrix of cumulative cases
  return(cumulative_cases)
  
}
