#' Calculate the relative likelihood of a set of knot date pairs.
#'
#' @param knots Dataframe containing knot date pairs
#' @param criteria_likelihood Criteria by which to calculate the relative
#' likelihood of each of the best knot dates; one of c("Pois_dev_inc", "Pois_dev_cum")
#' @param likelihood Equal and/or unequal likelihood to calculate;
#' at least one of c("Prob_equal", "Prob_unequal") (default is both)
#'
#' @return Modified \code{knots} dataframe containing column(s) for specified
#' \code{likelihood} parameters. If \code{"Prob_unequal" in likelihood},
#' dataframe contains additional "Min_n_unequal" column, which specifies 
#' the minimum number of simulation runs per knot date required 
#' to retain the relative probability of each (i.e. a whole-number weight).
#'
#' @examples
#' Calculate_Likelihood_Best_Knots(knots = knots_best, 
#' criteria_likelihood = "Pois_dev_cum")
Calculate_Likelihood_Best_Knots <- function(knots, 
                                            criteria_likelihood,
                                            likelihood = c("Prob_equal", "Prob_unequal")) {
  
  # Group dataframe of knots by country
  knots_grouped <- knots %>%
    group_by(Country)
  
  # If "Prob_equal" specified, construct equal probability of each knot date pair
  if ("Prob_equal" %in% likelihood) {
    
    # Divide
    knots_grouped <- knots_grouped %>% 
      mutate(n_knots = n(), Prob_equal = 1 / n_knots) %>%
      select(-n_knots) 
    
  }
  
  # If "Prob_unequal" specified, construct unequal probability of each knot date pair
  # and calculate minimum number of simulation runs per knot date required 
  # to retain relative probability of each
  if ("Prob_unequal" %in% likelihood) {
    
    # Create inverse of criteria_likelihood values so that lower values are ranked higher,
    # calculate normaliser for rescaling criteria_likelihood inverse values, and
    # calculate probability by multiplying criteria_likelihood inverse values by normaliser
    knots_grouped <- knots_grouped %>% 
      mutate(Criteria_inv = 1 / eval(parse(text = criteria_likelihood)),
             Norm = 1 / sum(Criteria_inv), 
             Prob_unequal = Criteria_inv * Norm) %>%
      select(-c(Criteria_inv, Norm)) 
    
    # Find the probability of the least likely knot date,
    # calculate the multiplier required for this knot date to be simulated once, and
    # multiply the probability of each knot date by multiplier
    knots_grouped <- knots_grouped %>% 
      mutate(Prob_min = min(Prob_unequal),
             Mult = ceiling(1 / Prob_min),
             Min_n_unequal = round(Prob_unequal * Mult)) %>%
      select(-c(Prob_min, Mult))
    
  }
  
  # Ungroup dataframe
  knots_ungrouped <- knots_grouped %>% ungroup
  
  # Return modified dataframe 
  return(knots_ungrouped)
  
}
