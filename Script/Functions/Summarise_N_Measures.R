#' Summarise the number of measures that exceed a cutoff value on each day.
#' 
#' Note that each measure is allowed to have its own specific cutoff value.
#'
#' @param data Dataframe containing all policy measures for one country.
#' @param measures A vector of the measures to evaluate (i.e. specific column names
#' in \code{data})
#' @param cutoffs A vector of cutoff points corresponding to all individual 
#' \code{measures}
#' @param scope A vector which defines all possible geographic scope(s) that are 
#' of interest for the specified \code{measures} 
#' (possible values = 0 for local/targeted scope, 1 for national/general scope)
#'
#' @return Dataframe containing 3 columns:
#' (1) Date = All dates in \code{data}; 
#' (2) N_measures = Number of \code{measures} with specified \code{scope} which 
#' exceed their designated \code{cutoffs} for each 'Date'; and
#' (3) N_measures_diff = Difference in the value of 'N_measures' from the 
#' previous 'Date' (i.e. negative value indicates restriction(s) lifted,
# zero indicates no change, positive value indicates restriction(s) increased).
#'
#' @examples
#' Summarise_N_Measures(data = filter(Policy_data_europe, Country == country),
#' measures = c("C1_School_closing", "C2_Workplace_closing", 
#' "C3_Cancel_public_events", "C5_Close_public_transport", 
#' "C6_Stay_at_home_requirements", "C7_Restrictions_on_internal_movement"),
#' cutoffs = c(1, 1, 1, 1, 1, 1),
#' scope = c(0, 1))
#' Summarise_N_Measures(data = filter(Policy_data_europe, Country == country),
#' measures = "C6_Stay_at_home_requirements",
#' cutoffs = 2,
#' scope = 1)
Summarise_N_Measures <- function(data, measures, cutoffs, scope) {
  
  # Print warning if each measure doesn't have its own cutoff point
  if (length(measures) != length(cutoffs)) {
    stop("Error: Each measure must have its own cutoff point.")
  }
  
  # Define flags associated with specified measures
  flags <- measures %>% 
    map(., .f = ~str_extract(.x, "[^_]+")) %>%
    map(., .f = ~paste0(.x, "_Flag"))
  
  # Subset dataset to include only selected Measures and associated Flags
  data <- data %>% ungroup %>% 
    select(Date, unlist(measures), unlist(flags)) %>%
    select(sort(names(.))) %>%
    relocate(Date)
  
  # For each measure, determine whether it is greater than or equal to its cutoff
  # AND whether it is within the specified geographic scope
  # If yes, assign 1; if no, assign 0
  data <- map2(.x = measures, .y = flags, 
               .f = ~select(data, Date, Measure_value = all_of(.x), Flag = all_of(.y))) %>%
    map2(., .y = cutoffs, .f = ~mutate(., Cutoff = .y)) %>% 
    map2(., .y = measures, .f = ~mutate(., Measure = .y)) %>%
    map(., .f = ~mutate(., Conditions_met = ifelse(Measure_value >= Cutoff & 
                                                     Flag %in% unlist(scope),
                                                   1, 0))) %>%
    map(., .f = ~select(., Date, Measure, Conditions_met)) %>%
    reduce(bind_rows) %>%
    pivot_wider(., names_from = Measure, values_from = Conditions_met)
  
  # Calculate number of measures above cutoff (i.e. row sums)
  data$N_measures <- apply(data[, unlist(measures)], 1, sum, na.rm = TRUE)
  
  # Retain Date, N_measures variables
  data <- data %>% select(Date, N_measures)
  
  # Determine change in number of restrictions from previous day
  # (negative value indicates restriction(s) lifted, ...
  # ...zero indicates no change, positive value indicates restriction(s) increased)
  data <- data %>%
    mutate(N_measures_diff = N_measures - lag(N_measures, n = 1, default = NA))
  
  # Return dataframe with Date, N_measures variables
  return(data)
  
}
