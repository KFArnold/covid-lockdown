#' Estimate growth parameters using an Arima spline model.
#' 
#' The model is fit to Daily_cases ~ Cumulative_cases_beg 
#' (i.e. Daily_cases(t) ~ Cumulative_cases(t-1)), and is specified with
#' second-order autocorrelation and weekly seasonality.
#'
#' @param n_knots Number of knots in the full model (i.e. number of unique interventions)
#' @param n_knots_in_range Number of knots within the modelling range
#' @param knot_1 Value of cumulative cases on the date of the first knot
#' @param knot_2 Value of cumulative cases on the date of the second knot
#' @param intercept Whether spline model should include intercept term (T/F)
#' @param data Data to feed into spline model
#' @param covariates Vector of all covariate names in spline model
#'
#' @return Named list containing growth parameters. If parameter estimation fails,
#' the list item 'Error_occurred' = TRUE is returned.
#'
#' @examples
Estimate_Growth_Parameters <- function(n_knots, n_knots_in_range, knot_1, knot_2, 
                                       intercept, data, covariates) {
  
  # Create estimation error indicator
  error_occurred <- FALSE
  
  # Fit ARIMA spline model with input data
  model <- tryCatch(Arima(data$Daily_cases, order = c(2, 0, 0), 
                          seasonal = list(order = c(1, 0, 0), period = 7),
                          xreg = as.matrix(data[, covariates]), 
                          include.constant = intercept, method = "ML"), 
                    error = function(e) { error_occurred <<- TRUE } )
  
  # If error occurred in model estimation, 
  # return list of empty parameters with error
  if (error_occurred == TRUE) { return(list(Growth_factor_1 = as.numeric(NA),
                                            Growth_factor_2 = as.numeric(NA),
                                            Growth_factor_3 = as.numeric(NA),
                                            Growth_factor_1_sd = as.numeric(NA),
                                            Growth_factor_2_sd = as.numeric(NA),
                                            Growth_factor_3_sd = as.numeric(NA),
                                            Intercept_1 = as.numeric(NA),
                                            Intercept_2 = as.numeric(NA),
                                            Intercept_3 = as.numeric(NA),
                                            Error_occurred = error_occurred)) }
  
  # Record all model parameters from spline model
  slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
  slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
  slope_3 <- as.numeric(coef(model)["Cumulative_cases_beg_3"])
  slope_1_sd <- ifelse(!is.na(slope_1), 
                       sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]], NA)
  slope_2_sd <- ifelse(!is.na(slope_2),
                       sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]], NA)
  slope_3_sd <- ifelse(!is.na(slope_3),
                       sqrt(diag(model$var.coef))[["Cumulative_cases_beg_3"]], NA)
  growth_factor_1 <- slope_1 + 1
  growth_factor_2 <- slope_2 + 1
  growth_factor_3 <- slope_3 + 1
  
  # Calculate intercepts for all spline segments
  intercept_1 <- 0  # always zero, even if unobserved
  
  # Depending on how many knots are in full model and how many of those are actually
  # observed within the modelling period,
  # the intercept estimated within the model will correspond to different spline segments
  if (n_knots == 1) {
    
    if (n_knots_in_range == 0) {
      
      # If there are no knots within the modelling range, then
      # the intercept estimated in spline model corresponds to segment 2
      intercept_2 <- as.numeric(coef(model)["intercept"])
      
    } else {  # (n_knots_in_range == 1)
      
      # If there is one knot within the modelling period, then
      # no intercept is calculated in the model and intercept corresponding to
      # segment 2 must be calcualted
      intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
      
    }
    
    # The intercept corresponding to segment 3 does not exist 
    intercept_3 <- as.numeric(NA)
    
  } else {
    
    if (n_knots_in_range == 0) {
      
      # If there are no knots within the modelling range, then
      # the intercept estimated in spline model corresponds to segment 3
      # (and the intercept corresponding to segment 2 cannot be calculated 
      # because the entire segment is unobserved)
      intercept_2 <- as.numeric(NA)
      intercept_3 <- as.numeric(coef(model)["intercept"])
      
    } else if (n_knots_in_range == 1) {
      
      # If there is only one knot within the modelling period, then
      # the intercept estimated in the spline model corresponds to segment 2
      intercept_2 <- as.numeric(coef(model)["intercept"])
      intercept_3 <- (intercept_2 + slope_2*knot_2) - slope_3*knot_2
      
    } else {  # (n_knots_in_range == 2)
      
      # If there are two knots within the modelling period, 
      # no intercept is calculated in the model
      
      if (is.na(knot_1)) {
        
        # If knots are not distinct (i.e. knot_date_1 = knot_date_2), 
        # segment 2 does not exist so only the intercept coresponding to segment 3
        # must be calculated
        intercept_2 <- as.numeric(NA)
        intercept_3 <- (intercept_1 + slope_1*knot_2) - slope_3*knot_2
        
      } else {
        
        # If there are two distinct knots within the modelling period, then
        # intercepts corresponding to segments 2 and 3 must be calcualted
        intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
        intercept_3 <- (intercept_2 + slope_2*knot_2) - slope_3*knot_2
        
      }
      
    }
    
  }
  
  # Return list of model parameters
  return(list(Growth_factor_1 = ifelse(exists("growth_factor_1"), growth_factor_1, NA),
              Growth_factor_2 = ifelse(exists("growth_factor_2"), growth_factor_2, NA),
              Growth_factor_3 = ifelse(exists("growth_factor_3"), growth_factor_3, NA),
              Growth_factor_1_sd = ifelse(exists("slope_1_sd"), slope_1_sd, NA),
              Growth_factor_2_sd = ifelse(exists("slope_2_sd"), slope_2_sd, NA),
              Growth_factor_3_sd = ifelse(exists("slope_3_sd"), slope_3_sd, NA),
              Intercept_1 = ifelse(exists("intercept_1"), intercept_1, NA),
              Intercept_2 = ifelse(exists("intercept_2"), intercept_2, NA),
              Intercept_3 = ifelse(exists("intercept_3"), intercept_3, NA),
              Error_occurred = error_occurred))
  
}
