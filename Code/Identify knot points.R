# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script...


# Lithuania, Portugal, Spain, and UK have negative incidence

# Currently, date_T is global parameter, meaning some countries have few dates for simulating

# Test whether making stricter criteria as equivalent to lockdown changes best knots identified

# Russia slowing appears to occur nearly 2 months after first restrictions/lockdown,
# so current code isn't performing well for this country

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(lspline); library(forecast); library(ggpubr); library(ggrepel); library(scales)
library(Metrics)

# Import country summary data
#summary_eur_final <- read_csv("./Results/Country summaries.csv") 

# Run source code to import and format data
source("./Code/Import, format, and summarise data.R")

#date_T <- as.Date("2020-06-01")  # currently GLOBAL var but can be adapted for each country

# ------------------------------------------------------------------------------
# Plot exponential growth
# ------------------------------------------------------------------------------

# Each country on separate grid ------------------------------------------------

## Normal scale ----------------------------------------------------------------

plot_exp_growth_cases <- list()

for (i in 1:nrow(summary_eur_final)) {
  
  # Define country
  country <- summary_eur_final[[i, "Country"]]
  
  # Filter cases/deaths dataframe by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  
  # Define dates of first restriction, lockdown, and 100 cases
  date_first_restriction <- summary_eur_final[[i, "Date_first_restriction"]]
  date_lockdown <- summary_eur_final[[i, "Date_lockdown"]]
  date_100 <- summary_eur_final[[i, "Date_100"]]
  
  # Plot
  p <- ggplot(data = filter(data_eur_final_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, 
                  y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_final_i, Date == date_100),
               size = 3, color = "grey", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_100), color = "grey",
                    label = paste0(as.character(date_100, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_final_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_final_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 0, size = 3) +
    scale_x_continuous(name = "Cumulative number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1))
  
  # Add plot to list
  plot_exp_growth_cases[[i]] <- p
  
}

# Save plots
dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = 6, ncol = 6)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 16))
ggsave(paste0(out, "Figure - Cumulative vs incident cases.png"),
       plot = g, width = 6*6, height = 6*6, limitsize = FALSE)
dev.off()

## Log scale -------------------------------------------------------------------

plot_exp_growth_cases <- list()

for (i in 1:nrow(summary_eur_final)) {
  
  # Define country
  country <- summary_eur_final[[i, "Country"]]
  
  # Filter cases/deaths dataframe by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  
  # Define dates of first restriction and lockdown
  date_first_restriction <- summary_eur_final[[i, "Date_first_restriction"]]
  date_lockdown <- summary_eur_final[[i, "Date_lockdown"]]
  
  # Plot
  p <- ggplot(data = filter(data_eur_final_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, 
                  y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_final_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_final_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 0, size = 3) +
    scale_x_continuous(name = "Cumulative number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1),
                       trans = log10_trans()) + 
    scale_y_continuous(name = "New daily number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1),
                       trans = log10_trans())
  
  # Add plot to list
  plot_exp_growth_cases[[i]] <- p
  
}

# Save plots
dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = 6, ncol = 6)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 16))
ggsave(paste0(out, "Figure - Cumulative vs incident cases (log scale).png"),
       plot = g, width = 6*6, height = 6*6, limitsize = FALSE)
dev.off()

# All countries on same grid ---------------------------------------------------

plot_exp_growth_cases <- ggplot(data = filter(data_eur_final, Date <= date_T),
                                aes(x = Cumulative_cases_beg, 
                                    y = Daily_cases)) +
  theme_minimal() +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 6),
        legend.spacing.y = unit(0.05, "cm")) + 
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "Exponential growth of Covid-19 cases",
       subtitle = "Cumulative versus incident cases") +
  geom_path(aes(color = Country), alpha = 0.7) +
  scale_x_continuous(name = "Cumulative number of lab-confirmed cases",
                     labels = comma_format(accuracy = 1)) + 
  scale_y_continuous(name = "New daily number of lab-confirmed cases",
                     limits = c(NA, 30000),
                     labels = comma_format(accuracy = 1))
#plot_exp_growth_cases

# Save plot
ggsave(paste0(out, "Figure - Cumulative vs incident cases - ALL.png"),
       plot = plot_exp_growth_cases, width = 12, height = 8)

# ------------------------------------------------------------------------------
# Estimate when exponential growth changed 
# ------------------------------------------------------------------------------

# Set seed
set.seed(72)

# (1) Iterate through countries
start <- Sys.time()
for (i in 1:nrow(summary_eur_final)) {
  
  # Define country
  country <- summary_eur_final[[i, "Country"]] %>% as.character
  
  # Filter cases/deaths dataframe by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  
  # Create copy of dataframe where cumulative cases >= 100 and up to date_T
  data_eur_final_100_i <- data_eur_final_i %>% filter(Date >= Date_100 & Date <= date_T)
  
  # Record important dates
  date_100 <- summary_eur_final[[i, "Date_100"]]  # (first date where cumulative cases >= 100)
  date_first_restriction <- summary_eur_final[[i, "Date_first_restriction"]]  # (date of first restriction)
  date_lockdown <- summary_eur_final[[i, "Date_lockdown"]]  # (date of lockdown)
  
  # Define potential knot dates (from dates of first restriction and lockdown to 28 days subsequent),
  # And create grid of all possible combinations of knot dates, with restrictions that...
  # (a) first knot date must be before or at the same time as second knot date, and
  # (b) knot dates must fall within modelling period (i.e. after the first date at which cumulative cases >= 100)
  if (!is.na(date_lockdown)) {
    possible_knot_dates_1 <- seq(from = date_first_restriction, to = date_first_restriction + 28, by = 1)
    possible_knot_dates_2 <- seq(from = date_lockdown, to = date_lockdown + 28, by = 1)
    grid <- tibble(expand.grid(possible_knot_dates_2, possible_knot_dates_1))
    names(grid) <- c("Knot_date_2", "Knot_date_1")
    grid <- grid %>% select("Knot_date_1", "Knot_date_2") %>% 
      filter(Knot_date_1 <= Knot_date_2, Knot_date_1 >= date_100)  
  } else {
    possible_knot_dates_1 <- seq(from = date_first_restriction, to = date_first_restriction + 28, by = 1)
    grid <- tibble("Knot_date_1" = possible_knot_dates_1)
  }
  
  # Create dataframe to store summary statistics for all possible combinations of knot dates
  knots <- bind_rows(tibble(Knot_date_1 = as.Date(character()),
                            Knot_date_2 = as.Date(character()),
                            Growth_factor_1 = as.numeric(),
                            Growth_factor_2 = as.numeric(),
                            Growth_factor_3 = as.numeric(),
                            BIC = as.numeric(),
                            RMSE_inc = as.numeric(),
                            RMSE_cum = as.numeric(),
                            Diff_cum_end = as.numeric()),
                     grid)
  rm(grid)
  
  # Set dates over which to simulate growth
  dates <- seq.Date(from = date_100, to = date_T, by = 1)
  
  # Create matrices for simulated data (daily and cumulative cases)
  # (1 row per simulation run, 1 col per date)
  daily_cases_sim <- cumulative_cases_end_sim <- 
    matrix(nrow = 1, ncol = length(dates) + 1,
           dimnames = list(1, as.character(seq.Date(from = date_100 - 1, to = date_T, by = 1))))
  # Initialise matrices with data at date_100 - 1
  daily_cases_sim[, 1] <- data_eur_final_i %>% 
    filter(Date == (date_100 - 1)) %>% pull(Daily_cases)
  cumulative_cases_end_sim[, 1] <- data_eur_final_i %>% 
    filter(Date == (date_100 - 1)) %>% pull(Cumulative_cases_end)
  
  # (2) Iterate through pairs of candidate knot points
  for (j in 1:nrow(knots)) {
    
    # Set knot dates
    knot_date_1 <- knots[[j, "Knot_date_1"]]
    knot_date_2 <- knots[[j, "Knot_date_2"]]
    
    skip_to_next <- FALSE
    
    # Estimate growth parameters
    ## If first knot occurs at first date for which cases exceeded 100 (i.e. when we begin modelling),
    ## there may be either no knots (i.e. knot occured before or at date_100)
    ## OR 1 knot (occurring at knot_date_2).
    ## Otherwise, there may be either 1 knot (occurring at knot_date_1 (= knot_date_2, if it exists))
    ## OR 2 knots (occurring at knot_date_1 and knot_date_2)
    if (knot_date_1 == date_100) {
      
      if (is.na(knot_date_2) | knot_date_1 == knot_date_2) {  # NO knot points
        
        # Set number of knot points
        n_knots <- 0
        
        # Fit regular Arima model (no intercept)
        model <- tryCatch(Arima(data_eur_final_100_i$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_eur_final_100_i[, "Cumulative_cases_beg"]), 
                                include.constant = FALSE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }
        
        # Calculate and record growth factor (model slope + 1)
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- as.numeric(coef(model)["Cumulative_cases_beg"] + 1)
        
        
      } else {  # ONE knot point (at knot_date_2)
        
        # Set number of knot points
        n_knots <- 1
        
        # Set knot point
        knot_1 <- data_eur_final_100_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_final_100_i$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:2)
        data_j <- bind_cols(Daily_cases = data_eur_final_100_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot point (no intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = FALSE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }
        
        # Calculate and record growth factors (model slopes + 1)
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"] + 1)
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"] + 1)
        
      }
      
    } else {
      
      if (is.na(knot_date_2) | knot_date_1 == knot_date_2) {  # ONE knot point (at knot_date_1 = knot_date_2)
        
        # Set number of knot points
        n_knots <- 1
        
        # Set knot point
        knot_1 <- data_eur_final_100_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_final_100_i$Cumulative_cases_beg, knots = c(knot_1)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:2)
        data_j <- bind_cols(Daily_cases = data_eur_final_100_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot point (no intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = FALSE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }

        # Calculate and record growth factors (model slopes + 1)
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"] + 1)
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"] + 1)
        
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        
        # Set number of knot points
        n_knots <- 2
        
        # Set knot points
        knot_1 <- data_eur_final_100_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
        knot_2 <- data_eur_final_100_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
        
        # Create dataframe for fitting manual splines
        data_j <- data.frame(lspline(data_eur_final_100_i$Cumulative_cases_beg, knots = c(knot_1, knot_2)))
        names(data_j) <- names <- paste0("Cumulative_cases_beg_", 1:3)
        data_j <- bind_cols(Daily_cases = data_eur_final_100_i$Daily_cases, data_j)
        
        # Fit ARIMA spline model w/ specified knot points (no intercept)
        model <- tryCatch(Arima(data_j$Daily_cases, order = c(2, 0, 0), 
                                seasonal = list(order = c(1, 0, 0), period = 7),
                                xreg = as.matrix(data_j[, names]), 
                                include.constant = FALSE, method = "ML"), 
                          error = function(e) { skip_to_next <<- TRUE } )
        if (skip_to_next) { next }

        # Calculate and record growth factors (model slopes + 1)
        knots[[j, "Growth_factor_1"]] <- growth_factor_1 <- as.numeric(coef(model)["Cumulative_cases_beg_1"] + 1)
        knots[[j, "Growth_factor_2"]] <- growth_factor_2 <- as.numeric(coef(model)["Cumulative_cases_beg_2"] + 1)
        knots[[j, "Growth_factor_3"]] <- growth_factor_3 <- as.numeric(coef(model)["Cumulative_cases_beg_3"] + 1)
        
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
      inc_t <- round(growth*inc_tminus1)
      daily_cases_sim[, as.character(t)] <- inc_t
      
      # Calculate cumulative cases at end of time t and record
      cum_t <- cum_tminus1 + inc_t
      cumulative_cases_end_sim[, as.character(t)] <- cum_t
      
    }  # (close loop 3)
    
    # Calculate and record RMSE 
    ## (1) For true vs predicted incident cases
    true_inc <- data_eur_final_100_i$Daily_cases
    pred_inc <- daily_cases_sim[1, -1]
    knots[[j, "RMSE_inc"]] <- rmse(true_inc, pred_inc)
    ## (2) For true vs predicted cumulative cases
    true_cum <- data_eur_final_100_i$Cumulative_cases_end
    pred_cum <- cumulative_cases_end_sim[1, -1]
    knots[[j, "RMSE_cum"]] <- rmse(true_cum, pred_cum)
    
    # Calculate absolute difference between cumulative cases at end of simulation vs true
    true_cum_end <- data_eur_final_100_i %>% filter(Date == date_T) %>% pull(Cumulative_cases_end)
    pred_cum_end <- cumulative_cases_end_sim[1, ncol(cumulative_cases_end_sim)]
    knots[[j, "Diff_cum_end"]] <- true_cum_end - pred_cum_end
    
    # Display progress 
    cat('\r', paste(round((j / nrow(knots) * 100), 0), 
                    "% done of country", i, "of", length(countries_eur_final), "     ", sep = " "))
    
  }  # (close loop 2)
  
  # Find best knot points for each country
  knots_1 <- knots %>% arrange(RMSE_inc) %>% head(10)  # lowest RMSE_inc
  knots_2 <- knots %>% arrange(RMSE_cum) %>% head(10)  # lowest RMSE_cum
  #knots_3 <- knots %>% arrange(abs(Diff_cum_end)) %>% head(10)  # lowest absolute difference in cumulative cases at end
  
  # Keep matches between TWO datsets and label with country
  knots_best_i <- knots_1[(knots_1$Knot_date_1 %in% knots_2$Knot_date_1) & 
                            (knots_1$Knot_date_2 %in% knots_2$Knot_date_2), ]
  #knots_best_i <- knots_1[(knots_1$Knot_date_1 %in% knots_2$Knot_date_1 & knots_1$Knot_date_1 %in% knots_3$Knot_date_1) & 
  #                          (knots_1$Knot_date_2 %in% knots_2$Knot_date_2 & knots_1$Knot_date_2 %in% knots_3$Knot_date_2), ]
  knots_best_i <- knots_best_i %>% mutate(Country = country) %>% relocate(Country)
  
  # Add best knots for country i to full dataframe
  if (i == 1) {
    knots_best <- knots_best_i
  } else {
    knots_best <- bind_rows(knots_best, knots_best_i)
  }
  
}  # (close loop 1)
end <- Sys.time()
end - start  # ~9 mins

# Remove loop variables
rm(i, j, t, country, data_eur_final_i, data_eur_final_100_i, 
   date_100, date_first_restriction, date_lockdown, 
   possible_knot_dates_1, possible_knot_dates_2, knots, dates, 
   daily_cases_sim, cumulative_cases_end_sim, knot_date_1, knot_date_2,
   skip_to_next, names, n_knots, knot_1, knot_2, data_j, model, 
   growth_factor_1, growth_factor_2, growth_factor_3,
   inc_tminus1, cum_tminus1, inc_t, cum_t, growth,
   true_inc, pred_inc, true_cum, pred_cum, true_cum_end, pred_cum_end,
   knots_1, knots_2, knots_best_i, start, end)

# Group dataframe of best knots by country and view
knots_best <- knots_best %>% group_by(Country); knots_best

# Find median growth factors for each country among best knots
knots_best <- knots_best %>% mutate(Median_growth_factor_1 = median(Growth_factor_1, na.rm = TRUE),
                                    Median_growth_factor_2 = median(Growth_factor_2, na.rm = TRUE),
                                    Median_growth_factor_3 = median(Growth_factor_3, na.rm = TRUE))

# Select ONE best (set of) knot point(s) by both RMSE_inc and RMSE_cum, and bind into single dataframe
knots_best_final_inc <- knots_best %>% filter(RMSE_inc == min(RMSE_inc)) %>% 
  mutate(Criteria = "min RMSE_inc") %>% select(-contains("Median"))
knots_best_final_cum <- knots_best %>% filter(RMSE_cum == min(RMSE_cum)) %>% 
  mutate(Criteria = "min RMSE_cum") %>% select(-contains("Median"))
knots_best_final <- bind_rows(knots_best_final_inc, knots_best_final_cum) %>% 
  relocate(Criteria, .after = Country) %>% arrange(Country)
rm(knots_best_final_inc, knots_best_final_cum)
knots_best_final

# Export knots_best and knots_best_final dataframes
write_csv(knots_best, path = paste0(out, "Best knot points - all.csv"))
write_csv(knots_best_final, path = paste0(out, "Best knot points - final.csv"))

# ------------------------------------------------------------------------------
# Plot exponential growth
# ------------------------------------------------------------------------------

# don't think necessary to convert to character 
##summary_eur_final <- summary_eur_final %>% mutate(Country = as.character(Country))

# Each country on separate grid ------------------------------------------------

#plot_exp_growth_cases <- list()
plot_exp_growth_cases_inc <- list()


for (i in 1:nrow(summary_eur_final)) {
  
  # Define country
  country <- summary_eur_final[[i, "Country"]]
  
  # Filter cases/deaths dataframe by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  
  # Define dates of first restriction and lockdown
  date_first_restriction <- summary_eur_final[[i, "Date_first_restriction"]]
  date_lockdown <- summary_eur_final[[i, "Date_lockdown"]]
  
  # Knot dates, values at knot dates, slopes and intercepts
  

  # Plot
  p <- ggplot(data = filter(data_eur_final_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, 
                  y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_final_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_final_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 0, size = 3) +
    scale_x_continuous(name = "Cumulative number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of lab-confirmed cases",
                       labels = comma_format(accuracy = 1))
  
  # Add plot to list
  plot_exp_growth_cases[[i]] <- p
  
}

# Save plots
dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = 6, ncol = 6)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 16))
ggsave(paste0(out, "Figure - Cumulative vs incident cases.png"),
       plot = g, width = 6*6, height = 6*6, limitsize = FALSE)
dev.off()


