# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script finds the 'best' pairs of dates where the growth rate of COVID-19 cases changes,
# from a list of candidate pairs of dates. This is done for all European countries.

# For each candidate pair, an ARIMA spline model is fit with the pair of dates as knot points.
# The growth factor for each of the spline segments is estimated from this model,
# and the growth of cases is simulated using the estimated growth factors.

# The 'best' pairs of dates for each country are selected according to how well their estimated growth factors
# fit the observed growth of COVID-19 cases.


# Notes:
# Lithuania, Portugal, Spain, and UK have negative incidence 
# (negative incidence creates problems for calculating Poisson deviance - have removed NA's from function for now)
# Test whether making stricter criteria as equivalent to lockdown changes best knots identified

# Russia slowing appears to occur nearly 2 months after first restrictions/lockdown,
# so current code isn't performing well for this country

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(lspline); library(forecast)

# Run source code to import and format data
source("./Code/Import, format, and summarise data.R")

## Functions -------------------------------------------------------------------

# Function to calculate Poisson deviance between two vectors
# from: https://en.wikipedia.org/wiki/Deviance_(statistics)
# Arguments: obs = vector of observed values, sim = vector of simulated/predicted values
Calc_Pois_Dev <- function(obs, sim) {
  
  #D <- 2 * sum(obs * log(obs / sim) - (obs - sim))
  D <- 2 * sum(obs * log(obs / sim) - (obs - sim), na.rm = TRUE)
  return(D)
  
}

# ------------------------------------------------------------------------------
# Estimate when exponential growth changed 
# ------------------------------------------------------------------------------

# Create list to store best knots for each country
knots_best <- list()

# (1) Iterate through countries
start <- Sys.time()
for (i in countries_eur_lockdown) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, summary dataframes by country
  data_eur_i <- data_eur %>% filter(Country == country)
  summary_eur_i <- summary_eur %>% filter(Country == country)
  
  # Record important dates
  date_50 <- summary_eur_i %>% pull(Date_50)
  date_first_restriction <- summary_eur_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)
  date_T <- summary_eur_i %>% pull(Date_T)
  
  # Create copy of cases/deaths dataframe where cumulative cases >= 50 and up to date_T
  data_eur_50_i <- data_eur_i %>% filter(Date >= date_50 & Date <= date_T)
  
  # Define potential knot dates (from dates of first restriction and lockdown to 28 days subsequent),
  # And create grid of all possible combinations of knot dates, with restrictions that...
  # (a) first knot date must be before or at the same time as second knot date, and
  # (b) knot dates must fall within modelling period 
  # (i.e. after the first date at which cumulative cases >= 50 and less than date_T)
  if (is.na(date_lockdown) | date_first_restriction == date_lockdown) {
    possible_knot_dates_1 <- seq(from = date_first_restriction, to = date_first_restriction + 28, by = 1)
    grid <- tibble("Knot_date_1" = possible_knot_dates_1) %>% filter(Knot_date_1 >= date_50)
  } else {
    possible_knot_dates_1 <- seq(from = date_first_restriction, to = date_first_restriction + 28, by = 1)
    possible_knot_dates_2 <- seq(from = date_lockdown, to = date_lockdown + 28, by = 1)
    grid <- tibble(expand.grid(possible_knot_dates_2, possible_knot_dates_1))
    names(grid) <- c("Knot_date_2", "Knot_date_1")
    grid <- grid %>% select("Knot_date_1", "Knot_date_2") %>% 
      filter(Knot_date_1 <= Knot_date_2, Knot_date_1 >= date_50, Knot_date_2 < date_T)  
    # If first knot date equals second knot date, replace second with NA
    for (g in 1:nrow(grid)) {
      k_1 <- grid[[g, "Knot_date_1"]]
      k_2 <- grid[[g, "Knot_date_2"]]
      if (k_1 == k_2) {grid[[g, "Knot_date_2"]] <- NA}
    }
  }
  
  # Create dataframe to store summary statistics for all possible combinations of knot dates
  knots <- bind_rows(tibble(Knot_date_1 = as.Date(character()),
                            Knot_date_2 = as.Date(character()),
                            N_knots = as.numeric(),
                            Growth_factor_1 = as.numeric(),
                            Growth_factor_2 = as.numeric(),
                            Growth_factor_3 = as.numeric(),
                            Growth_factor_1_sd = as.numeric(),
                            Growth_factor_2_sd = as.numeric(),
                            Growth_factor_3_sd = as.numeric(),
                            Intercept_1 = as.numeric(),
                            Intercept_2 = as.numeric(),
                            Intercept_3 = as.numeric(),
                            Pois_dev_inc = as.numeric(),
                            Pois_dev_cum = as.numeric(),
                            Diff_cum_end = as.numeric()),
                     grid)
  rm(grid)
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_50, to = date_T, by = 1)
  
  # Create matrices for simulated data (daily and cumulative cases)
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- cumulative_cases_end_sim <- 
    matrix(nrow = 1, ncol = length(dates) + 1,
           dimnames = list(1, as.character(seq.Date(from = date_50 - 1, to = date_T, by = 1))))
  # Initialise matrices with data at date_50 - 1
  daily_cases_sim[, 1] <- data_eur_i %>% 
    filter(Date == (date_50 - 1)) %>% pull(Daily_cases)
  cumulative_cases_end_sim[, 1] <- data_eur_i %>% 
    filter(Date == (date_50 - 1)) %>% pull(Cumulative_cases_end)
  
  # (2) Iterate through pairs of candidate knot points
  for (j in 1:nrow(knots)) {
    
    # Set knot dates
    knot_date_1 <- knots[[j, "Knot_date_1"]]
    knot_date_2 <- knots[[j, "Knot_date_2"]]
    
    skip_to_next <- FALSE
    
    # Estimate growth parameters
    ## If first knot occurs at first date for which cases exceeded 50 (i.e. when we begin modelling),
    ## there may be either no knots (i.e. knot occured before or at date_50)
    ## OR 1 knot (occurring at knot_date_2).
    ## Otherwise, there may be either 1 knot (occurring at knot_date_1 (= knot_date_2, if it exists))
    ## OR 2 knots (occurring at knot_date_1 and knot_date_2)
    if (knot_date_1 == date_50) {
      
      if (is.na(knot_date_2)) {  # NO knot points
        
        # Set number of knot points and record
        n_knots <- 0
        knots[[j, "N_knots"]] <- n_knots
        
        # Fit regular Arima model (with intercept, since this is not technically first segment)
        model <- tryCatch(Arima(data_eur_50_i$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_eur_50_i[, "Cumulative_cases_beg"]), 
                                include.constant = TRUE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }
        
        # Record model parameters (intercept, slope, and SD of slope)
        intercept_1 <- as.numeric(coef(model)["intercept"])
        slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg"])
        slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg"]]
        
        # Calculate and record growth factor, record model parameters
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- slope_1 + 1
        knots[[j, "Growth_factor_1_sd"]] <- slope_1_sd
        knots[[j, "Intercept_1"]] <- intercept_1
        
      } else {  # ONE knot point (at knot_date_2)
        
        # Set number of knot points and record
        n_knots <- 1
        knots[[j, "N_knots"]] <- n_knots
        
        # Set knot point
        knot_1 <- data_eur_50_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_50_i$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:2)
        data_j <- bind_cols(Daily_cases = data_eur_50_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot point (with intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = TRUE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }
        
        # Record model parameters (intercept, slope, and SD of slope)
        intercept_1 <- as.numeric(coef(model)["intercept"])
        slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
        slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
        slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]]
        slope_2_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]]
        intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
        
        # Calculate and record growth factor, record model parameters
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- slope_1 + 1
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- slope_2 + 1
        knots[[j, "Growth_factor_1_sd"]] <- slope_1_sd
        knots[[j, "Growth_factor_2_sd"]] <- slope_2_sd
        knots[[j, "Intercept_1"]] <- intercept_1
        knots[[j, "Intercept_2"]] <- intercept_2
        
      }
      
    } else {
      
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        
        # Set number of knot points and record
        n_knots <- 1
        knots[[j, "N_knots"]] <- n_knots
        
        # Set knot point
        knot_1 <- data_eur_50_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_50_i$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:2)
        data_j <- bind_cols(Daily_cases = data_eur_50_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot point (with intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = TRUE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }

        # Record model parameters (intercept, slope, and SD of slope)
        intercept_1 <- as.numeric(coef(model)["intercept"])
        slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
        slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
        slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]]
        slope_2_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]]
        intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
        
        # Calculate and record growth factor, record model parameters
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- slope_1 + 1
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- slope_2 + 1
        knots[[j, "Growth_factor_1_sd"]] <- slope_1_sd
        knots[[j, "Growth_factor_2_sd"]] <- slope_2_sd
        knots[[j, "Intercept_1"]] <- intercept_1
        knots[[j, "Intercept_2"]] <- intercept_2
        
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        
        # Set number of knot points and record
        n_knots <- 2
        knots[[j, "N_knots"]] <- n_knots
        
        # Set knot points
        knot_1 <- data_eur_50_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        knot_2 <- data_eur_50_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_50_i$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:3)
        data_j <- bind_cols(Daily_cases = data_eur_50_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot points (no intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = FALSE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }
        
        # Record model parameters (intercept, slope, and SD of slope)
        intercept_1 <- 0
        slope_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"])
        slope_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"])
        slope_3 <- as.numeric(coef(model)["Cumulative_cases_beg_3"])
        slope_1_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_1"]]
        slope_2_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_2"]]
        slope_3_sd <- sqrt(diag(model$var.coef))[["Cumulative_cases_beg_3"]]
        intercept_2 <- (intercept_1 + slope_1*knot_1) - slope_2*knot_1
        intercept_3 <- (intercept_2 + slope_2*knot_2) - slope_3*knot_2
        
        # Calculate and record growth factor, record model parameters
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- slope_1 + 1
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- slope_2 + 1
        knots[[j, "Growth_factor_3"]] <- growth_factor_3 <- slope_3 + 1
        knots[[j, "Growth_factor_1_sd"]] <- slope_1_sd
        knots[[j, "Growth_factor_2_sd"]] <- slope_2_sd
        knots[[j, "Growth_factor_3_sd"]] <- slope_3_sd
        knots[[j, "Intercept_1"]] <- intercept_1
        knots[[j, "Intercept_2"]] <- intercept_2
        knots[[j, "Intercept_3"]] <- intercept_3
        
      }
      
    }  # (close if-else section)
    
    # Record model summaries
    knots[[j, "BIC"]] <- BIC(model)
    
    # (3) Estimate growth of cases using knot point(s)
    for (t in as.list(dates)) {
      
      # Get daily and cumulative cases from time t-1
      inc_tminus1 <- daily_cases_sim[, as.character(t-1)]
      cum_tminus1 <- cumulative_cases_end_sim[, as.character(t-1)]
      
      # Define growth parameters
      if (n_knots == 0) {  # NO knot points
        growth <- growth_factor_1  
      } else if (n_knots == 1) {  # ONE knot point
        if (t <= knot_date_1) {
          growth <- growth_factor_1
        } else {
          growth <- growth_factor_2
        }
      } else {  # TWO knot points
        if (t <= knot_date_1) {
          growth <- growth_factor_1
        } else if (t <= knot_date_2) {
          growth <- growth_factor_2
        } else {
          growth <- growth_factor_3
        }
      }
      
      # Calculate daily cases at time t and record
      inc_t <- growth*inc_tminus1
      daily_cases_sim[, as.character(t)] <- inc_t
      
      # Calculate cumulative cases at end of time t and record
      cum_t <- cum_tminus1 + inc_t
      cumulative_cases_end_sim[, as.character(t)] <- cum_t
      
    }  # (close loop 3)
    
    # Calculate and record Poisson deviance
    ## (1) For predicted vs true (7-day moving average) incident cases
    true_inc <- data_eur_50_i$Daily_cases_MA7
    pred_inc <- daily_cases_sim[1, -1]
    knots[[j, "Pois_dev_inc"]] <- Calc_Pois_Dev(obs = true_inc, sim = pred_inc)
    ## (2) For true vs predicted cumulative cases
    true_cum <- data_eur_50_i$Cumulative_cases_end
    pred_cum <- cumulative_cases_end_sim[1, -1]
    knots[[j, "Pois_dev_cum"]] <- Calc_Pois_Dev(obs = true_cum, sim = pred_cum)
    
    # Calculate absolute difference between cumulative cases at end of simulation vs true
    true_cum_end <- data_eur_50_i %>% filter(Date == date_T) %>% pull(Cumulative_cases_end)
    pred_cum_end <- cumulative_cases_end_sim[1, ncol(cumulative_cases_end_sim)]
    knots[[j, "Diff_cum_end"]] <- true_cum_end - pred_cum_end
    
    # Display progress 
    cat('\r', paste(round((j / nrow(knots) * 100), 0), 
                    "% done of country", grep(country, unlist(countries_eur_lockdown)), "of", 
                    length(countries_eur_lockdown), "          ", sep = " "))
    
  }  # (close loop 2)
  
  # Remove from consideration knot date pairs for which ...
  # (1) growth factor 1 is less than 1 AND growth factor 3 exists 
  # (because where there are 3 segments, the first scenario must represent initial uncontrolled growth)
  # (2) any of growth factors are negative
  remove_1 <- knots %>% filter(Growth_factor_1 < 1 & !is.na(Growth_factor_3)) 
  remove_2 <- knots %>% filter(Growth_factor_1 < 0 | Growth_factor_2 < 0 | Growth_factor_3 < 0)
  knots <- anti_join(knots, remove_1, by = names(knots)) %>% 
    anti_join(., remove_2, by = names(knots))
  
  # Find best knot points (by lowest Pois_dev_inc) for each country and label 
  knots_best_i <- knots %>% arrange(Pois_dev_inc) %>% head(10) %>% 
    mutate(Country = country) %>% relocate(Country)
  
  # Add best knots for country i to list of best knots
  knots_best[[i]] <- knots_best_i
  
}  # (close loop 1)
end <- Sys.time()
end - start  # ~9 mins

# Remove loop variables
rm(i, j, t, g, country, data_eur_i, summary_eur_i, data_eur_50_i, 
   date_50, date_first_restriction, date_lockdown, date_T,
   possible_knot_dates_1, possible_knot_dates_2, k_1, k_2, knots, dates, 
   daily_cases_sim, cumulative_cases_end_sim, knot_date_1, knot_date_2,
   skip_to_next, names, n_knots, knot_1, knot_2, data_j, model, 
   intercept_1, intercept_2, intercept_3,
   slope_1, slope_2, slope_3, slope_1_sd, slope_2_sd, slope_3_sd,
   growth_factor_1, growth_factor_2, growth_factor_3,
   inc_tminus1, cum_tminus1, inc_t, cum_t, growth,
   true_inc, pred_inc, true_cum, pred_cum, true_cum_end, pred_cum_end,
   knots_best_i, remove_1, remove_2, start, end)

# Combine best knots from all countries into single dataframe
knots_best <- bind_rows(knots_best)

# Group dataframe of best knots by country, arrange by knot dates
knots_best <- knots_best %>% group_by(Country) %>% arrange(Country, Knot_date_1, Knot_date_2)

# Construct probability of each pair of knot points
## (1) Equal probability of each knot point pair
knots_best <- knots_best %>% mutate(n_knots = n(), Prob_equal = 1 / n_knots) %>%
  select(-n_knots)
## (2) Unequal probabilities of each knot point pair according to Pois_dev_cum
### Create inverse of Pois_dev_cum values so that lower values are ranked higher,
### calculate normaliser for rescaling Pois_dev_cum inverse values, and
### calculate probability by multiplying Pois_dev_cum inverse values by normaliser
knots_best <- knots_best %>% mutate(Pois_dev_cum_inv = 1 / Pois_dev_cum,
                                    Norm = 1 / sum(Pois_dev_cum_inv), 
                                    Prob_unequal = Pois_dev_cum_inv * Norm) %>%
  select(-c(Pois_dev_cum_inv, Norm))

# Find median growth factors for each country among best knots
median_growth_factors <- knots_best %>% summarise(Median_growth_factor_1 = median(Growth_factor_1, na.rm = TRUE),
                                                  Median_growth_factor_2 = median(Growth_factor_2, na.rm = TRUE),
                                                  Median_growth_factor_3 = median(Growth_factor_3, na.rm = TRUE),
                                                  .groups = "keep")
knots_best <- full_join(knots_best, median_growth_factors)
rm(median_growth_factors)

# Export knots_best dataframe
write_csv(knots_best, path = paste0(out, "Best knot points.csv"))

