#' Calculate the dates for which a specific country implements and ends 
#' (a group of) specified measures(s).
#'
#' @param country Country
#' @param measures_any_restriction A list containing 4 items: 
#' (1) measures = vector of measures constituting any restriction, 
#' (2) cutoffs = vector of cutoffs corresponding to the given measures, 
#' (3) threshold = number representing the minimum number of measures constituting
#' any restriction, and
#' (4) scope = vector of numbers which all possible geographic scope(s) that are 
#' of interest for the specified measures (possible values = 0 for local/targeted 
#' scope, 1 for national/general scope)
#' @param measures_lockdown A list containing 4 items:
#' (1) measures = vector of measures constituting lockdown, 
#' (2) cutoffs = vector of cutoffs corresponding to the given measures, 
#' (3) threshold = number representing the minimum number of measures constituting
#' lockdown, and
#' (4) scope = vector of numbers which all possible geographic scope(s) that are 
#' of interest for the specified measures (possible values = 0 for local/targeted 
#' scope, 1 for national/general scope)
#' @param measures_lockdown_alt A list containing 4 items:
#' (1) measures = vector of alternate measures constituting lockdown, 
#' (2) cutoffs = vector of cutoffs corresponding to the given measures, 
#' (3) threshold = number representing the minimum number of alternate measures 
#' constituting lockdown, and
#' (4) scope = vector of numbers which all possible geographic scope(s) that are 
#' of interest for the specified measures (possible values = 0 for local/targeted 
#' scope, 1 for national/general scope)
#'
#' @return A named list containing 6 items: (1) Country, 
#' (2) Date_first_restriction, (3) Date_restrictions_eased, (4) Date_lockdown, 
#' (5) Date_lockdown_eased, and (6) Date_lockdown_end
#'
#' @examples
#' Calculate_Policy_Dates(country = "United Kingdom,
#' measures_any_restriction = list("measures" = c("C1_School_closing", 
#' "C2_Workplace_closing", "C3_Cancel_public_events", "C5_Close_public_transport", 
#' "C6_Stay_at_home_requirements", "C7_Restrictions_on_internal_movement"),
#' "cutoffs" = c(1, 1, 1, 1, 1, 1), "scope" = c(0, 1), "threshold" = 1),
#' measures_lockdown = list("measures" = "C6_Stay_at_home_requirements",
#' "cutoffs" = 2, "scope" = 1, "threshold" = 1),
#' measures_lockdown_alt = list("measures" = c("C1_School_closing", 
#' "C2_Workplace_closing", "C3_Cancel_public_events", "C5_Close_public_transport", 
#' "C7_Restrictions_on_internal_movement"), "cutoffs" = c(2, 2, 2, 2, 2), 
#' "scope" = 1, "threshold" = 3))
Calculate_Policy_Dates <- function(country, measures_any_restriction, 
                                   measures_lockdown, measures_lockdown_alt) {
  
  # Import required dataframes if not already loaded in the global environment
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "Policy_data_europe"), 
                            silent = TRUE)
  
  # Define date_max (i.e. last date for which case data can be reasonably assumed 
  # complete) for the specified country
  date_max <- Cases_deaths_data_europe %>% 
    filter(Country == country) %>% 
    pull(Date) %>% max
  
  # Filter policies data by country and date_max
  policies_country <- Policy_data_europe %>% 
    filter(Country == country, Date <= date_max)
  
  # Summarise any restrictions  --------
  
  # (either recommended or required)
  
  # Determine number of recommended or required measures values on each date
  data_any_restriction <- Summarise_N_Measures(data = policies_country,
                                               measures = measures_any_restriction$measures,
                                               cutoffs = measures_any_restriction$cutoffs,
                                               scope = measures_any_restriction$scope)
  
  # Determine whether any measures were recommended/required (i.e. threshold exceeded) for each date,
  # and whether number of measures decreases (i.e. diff is negative) for each date
  data_any_restriction <- data_any_restriction %>% 
    mutate(Threshold_exceeded = ifelse(N_measures >= measures_any_restriction$threshold, 
                                       TRUE, FALSE),
           Diff_neg = ifelse(N_measures_diff < 0, 
                             TRUE, FALSE)) %>%
    relocate(-contains("diff"))
  
  # Create indicator for whether any restriction was recommended/required
  any_restriction_tf <- any(data_any_restriction$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Analyse if any restriction recommended/required
  if (any_restriction_tf == TRUE) {
    
    ## First restriction ##
    ## (first date where any restrictions were either recommended or required) ##
    
    # Filter restrictions data by dates where any measures were recommended
    data_any_restriction_filt <- data_any_restriction %>% filter(Threshold_exceeded == TRUE)
    # Define date of first restriction as first date where threshold was exceeded
    date_first_restriction <- data_any_restriction_filt %>% pull(Date) %>% min(na.rm = TRUE)
    
    ## Restrictions eased ##
    ## (first date after date of first restriction where number of measures decreased) ##
    
    # Filter restrictions data by dates greater than date of first restriction
    data_any_restriction_filt <- data_any_restriction %>% filter(Date >= date_first_restriction)
    
    # Create indicator for whether restrictions were eased
    # (TRUE if difference is negative on any date, FALSE if difference always >= 0)
    restrictions_eased_tf <- any(data_any_restriction_filt$Diff_neg == TRUE, na.rm = TRUE)
    
    # Analyse countries whose restrictions were eased
    if (restrictions_eased_tf == TRUE) {
      # Re-filter data by dates where difference is negative
      data_any_restriction_filt <- data_any_restriction_filt %>% filter(Diff_neg == TRUE)
      # Define date of restrictions eased as first date with negative difference
      date_restrictions_eased <- data_any_restriction_filt %>% pull(Date) %>% min(na.rm = TRUE)
      # Add to summary table
      #summary["Date_restrictions_eased"] <- date_restrictions_eased
    }  
    
  } else {   # (close loop - any restriction recommended/required)
    
    # Record dates of first restriction and restrictions eased as NA
    date_first_restriction <- as.Date(NA)
    date_restrictions_eased <- as.Date(NA)
    
  }
  
  # Summarise lockdown --------
  
  # Determine number of the following required measures on each date
  # (1) lockdown (general or targeted)
  data_lockdown <- Summarise_N_Measures(data = policies_country,
                                        measures = measures_lockdown$measures,
                                        cutoffs = measures_lockdown$cutoffs,
                                        scope = measures_lockdown$scope)
  # (2) alternate lockdown measures
  data_lockdown_alt <- Summarise_N_Measures(data = policies_country, 
                                            measures = measures_lockdown_alt$measures, 
                                            cutoffs = measures_lockdown_alt$cutoffs,
                                            scope = measures_lockdown_alt$scope)
  
  # Merge two dataframes together
  data_lockdown_all <- full_join(data_lockdown, data_lockdown_alt, by = "Date", suffix = c("_lockdown", "_lockdown_alt"))
  
  # Determine whether lockdown measures or alternate lockdown threshold is exceeded for each date,
  # and whether number of lockdown measures or alternate lockdown measures decreases (i.e. diff is negative) for each date
  data_lockdown_all <- data_lockdown_all %>% 
    mutate(Threshold_exceeded = ifelse(N_measures_lockdown >= measures_lockdown$threshold |
                                         N_measures_lockdown_alt >= measures_lockdown_alt$threshold,
                                       TRUE, FALSE),
           Diff_neg = ifelse(N_measures_diff_lockdown < 0 | N_measures_diff_lockdown_alt < 0, 
                             TRUE, FALSE)) %>%
    relocate(-contains("diff"))
  
  # Create indicator for whether lockdown was entered
  # (TRUE if threshold exceeded on any date, FALSE if threshold never exceeded)
  lockdown_tf <- any(data_lockdown_all$Threshold_exceeded == TRUE, na.rm = TRUE)
  
  # Analyse if country entered lockdown
  if (lockdown_tf == TRUE) {
    
    ## Beginning of lockdown ##
    ## (first date where either lockdown or alternate lockdown measures were required) ##
    
    # Filter lockdown data by dates where lockdown threshold was exceeded
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Threshold_exceeded == TRUE)
    # Define lockdown date as first date where threshold was exceeded
    date_lockdown <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)

    ## Lockdown eased ##
    ## (first date after lockdown date where number of measures decreased) ##
    
    # Filter lockdown data by dates greater than lockdown date
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Date >= date_lockdown)
    
    # Create indicator for whether lockdown was eased
    # (TRUE if difference is negative on any date, FALSE if difference always >= 0)
    lockdown_eased_tf <- any(data_lockdown_all_filt$Diff_neg == TRUE, na.rm = TRUE)
    
    # Analyse countries which eased lockdown
    if (lockdown_eased_tf == TRUE) {
      # Re-filter data by dates where difference is negative
      data_lockdown_all_filt <- data_lockdown_all_filt %>% filter(Diff_neg == TRUE)
      # Define lockdown easing date as first date with negative difference
      date_lockdown_eased <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
    } else {
      # Record date of lockdown easing as NA
      date_lockdown_eased <- as.Date(NA)
    }
    
    ## End of lockdown ##
    ## (first date AFTER lockdown date where neither lockdown measures no alt lockdown measures were required) ##
    
    # Filter lockdown data by dates greater than lockdown date
    data_lockdown_all_filt <- data_lockdown_all %>% filter(Date >= date_lockdown)
    
    # Create indicator for whether lockdown was ended
    lockdown_end_tf <- any(data_lockdown_all_filt$Threshold_exceeded == FALSE, na.rm = TRUE)
    
    # Analyse countries which ended lockdown
    if (lockdown_end_tf == TRUE) {
      # Re-filter data by dates where lockdown threshold wasn't exceeded
      data_lockdown_all_filt <- data_lockdown_all_filt %>% filter(Threshold_exceeded == FALSE) 
      # Define lockdown end date
      date_lockdown_end <- data_lockdown_all_filt %>% pull(Date) %>% min(na.rm = TRUE)
    } else {
      # Record lockdown end date as NA
      date_lockdown_end <- as.Date(NA)
    }
    
  } else {  # (close inner loop - entered lockdown)
    
    # Record dates of lockdown, lockdown easing, and lockdown end as NA
    date_lockdown <- as.Date(NA)
    date_lockdown_eased <- as.Date(NA)
    date_lockdown_end <- as.Date(NA)
    
  }
  
  # Return list of important policy dates
  return(list(Country = country,
              Date_first_restriction = date_first_restriction,
              Date_restrictions_eased = date_restrictions_eased,
              Date_lockdown = date_lockdown,
              Date_lockdown_eased = date_lockdown_eased,
              Date_lockdown_end = date_lockdown_end))
  
}
