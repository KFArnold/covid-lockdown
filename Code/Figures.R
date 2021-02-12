# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates various country-specific figures related to COVID-19:
# (1) Important dates 
# (2) Exponential growth (i.e. cumulative vs incident cases), with fitted splines
# (3) Growth factors under lockdown
# (4) Length of time to reach important thresholds
# (5) Simulated incident and cumulative cases (natural and counterfactual), and model residuals
# (6) Estimated between-country and within-country effect sizes

# All figures are saved to the project directory (./Results/).

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Restore package library to last snapshot
packrat::restore()

# Load required packages
library(tidyverse); library(ggrepel); library(scales)
library(ggpubr); library(foreach); library(RColorBrewer); library(ggrepel)
library(ggh4x)

# Define storage directory for formatted data
data_directory_f <- paste0("./Data/Formatted/")

# Define storage directory for results
results_directory <- paste0("./Results/")

# Load formatted data
data_eur <- read_csv(paste0(data_directory_f, "Cases_deaths_data_europe.csv"))
worldbank_eur <- read_csv(paste0(data_directory_f, "Worldbank_data_europe.csv"))

# Import files containing best knot date pairs, median growth factors, and country summaries
knots_best <- read_csv(paste0(results_directory, "knots_best.csv"))
median_growth_factors <- read_csv(paste0(results_directory, "median_growth_factors.csv"))
summary_eur <- read_csv(paste0(results_directory, "summary_eur.csv"))

# Load list of European countries for which we have both cases/deaths data and policy data,
# those which entered lockdown, and those which can be modelled 
load(paste0(results_directory, "countries_eur.RData"))
load(paste0(results_directory, "countries_eur_lockdown.RData"))
load(paste0(results_directory, "countries_eur_modelled.RData"))

# Load lists of countries excluded from analyses
load(paste0(results_directory, "countries_excluded_all.RData"))
load(paste0(results_directory, "countries_excluded_time_to_threshold.RData"))
load(paste0(results_directory, "countries_excluded_length_lockdown.RData"))
load(paste0(results_directory, "countries_excluded_total_cases.RData"))

## Import simulated data -------------------------------------------------------

# Define filenames which contain simulated data
files <- c("summary_daily_cases_sim", "summary_cumulative_cases_end_sim", "summary_thresholds_sim")

# Import all simulation files (i.e. natural and counterfactual histories) from Results subfolders
sim_data_all <- list()
for (i in files) {
  
  # Get names of files in subfolders which contain designated simualtion results
  sim_files <- list.files(path = "./Results",
                          recursive = TRUE, 
                          pattern = paste0(i, ".csv"),
                          full.names = TRUE)
  
  # Read in all files and bind together
  sim_data_all[[i]] <- lapply(sim_files, read_csv, col_types = cols(Simulation = col_character(),
                                                                    N_days_first_restriction = col_double(),
                                                                    N_days_lockdown = col_double(),
                                                                    Date_first_restriction = col_date(),
                                                                    Date_lockdown = col_date())) %>% 
    reduce(bind_rows) %>% 
    mutate(across(where(is.character), as.factor)) %>%
    arrange(N_days_first_restriction, N_days_lockdown)
  
}

# Append file names with '_all' and save simulation results as separate objects in global environment
names(sim_data_all) <- paste0(names(sim_data_all), "_all")
list2env(sim_data_all, globalenv()); rm(sim_data_all)

# Create dataframe for simulated cumulative cases at beginning of time t
summary_cumulative_cases_beg_sim_all <- summary_cumulative_cases_end_sim_all %>% 
  group_by(Country, History, N_days_first_restriction, N_days_lockdown) %>%
  mutate(across(c(Mean, C_025, C_975), ~lag(., n = 1, default = NA))) %>% ungroup

# Combine all daily/cumulative cases data into single dataframe,
# and remove individual files
names <- colnames(summary_daily_cases_sim_all)
col_join <- names[! names %in% c("Mean", "C_025", "C_975")]
summary_cases_sim_all <- full_join(summary_cumulative_cases_beg_sim_all,
                                   summary_daily_cases_sim_all,
                                   by = col_join,
                                   suffix = c("_cumulative_cases_beg", "_daily_cases")) %>%
  full_join(., summary_cumulative_cases_end_sim_all,
            by = col_join) %>%
  rename_at(vars(Mean, C_025, C_975), function(x) {paste0(x, "_cumulative_cases_end")})
rm(summary_cumulative_cases_beg_sim_all, summary_daily_cases_sim_all, summary_cumulative_cases_end_sim_all)

## Import estimated effects ----------------------------------------------------

# Import (best) between- and within-country effect estimates
effects_between_countries <- read_csv(paste0(results_directory, "effects_between_countries_best.csv")) %>% 
  mutate(across(where(is.character), as.factor))
effects_within_countries <- read_csv(paste0(results_directory, "effects_within_countries.csv")) %>% 
  mutate(across(where(is.character), as.factor))

# Create variable for adjusted vs unadjusted in between-country dataframe
effects_between_countries <- effects_between_countries %>% 
  mutate(Adjusted = ifelse(is.na(Covariates), "Unadjusted", "Adjusted"),
         Adjusted = as.factor(Adjusted))

## Formatting ------------------------------------------------------------------

# Define ordering of threshold, simulation, history, exposure, and leverage levels
threshold_levels <- c("0.0010%", "0.0050%", "0.0100%")
simulation_levels <- c("0,0", "0,1", "0,3", "0,5", "0,7", "7,7", "14,14")
history_levels <- c("Natural history", "Counterfactual history")
exposure_levels <- c("Daily_cases_MA7", "log(Daily_cases_MA7)",
                     "Cumulative_cases_beg_MA7", "log(Cumulative_cases_beg_MA7)")
leverage_levels <- c("Included", "Excluded")

# Reorder levels of Threshold, Simulation, History, and Exposure factors in dataframes
summary_cases_sim_all <- summary_cases_sim_all %>%
  mutate(Simulation = factor(Simulation, levels = simulation_levels),
         History = factor(History, levels = history_levels))
summary_thresholds_sim_all <- summary_thresholds_sim_all %>%
  mutate(Threshold = factor(Threshold, levels = threshold_levels),
         Simulation = factor(Simulation, levels = simulation_levels),
         History = factor(History, levels = history_levels))
effects_between_countries <- effects_between_countries %>%
  mutate(Exposure = factor(Exposure, levels = exposure_levels),
         Leverage_points = factor(Leverage_points, levels = leverage_levels))
effects_within_countries <- effects_within_countries %>% 
  mutate(Threshold = factor(Threshold, levels = threshold_levels),
         Simulation = factor(Simulation, levels = simulation_levels),
         History = factor(History, levels = history_levels))

# Create key for threshold labels
threshold_labels <- c("0.0010%" = "0.0010%\nof population",
                      "0.0050%" = "0.0050%\nof population",
                      "0.0100%" = "0.0100%\nof population")

# Create key for exposure labels
exposure_labels <- c("Daily_cases_MA7" = "Daily cases\n(MA7)",
                     "log(Daily_cases_MA7)" = "Daily cases\n(MA7), logged",
                     "Cumulative_cases_beg_MA7" = "Cumulative casesn(MA7)", 
                     "log(Cumulative_cases_beg_MA7)" = "Cumulative cases\n(MA7), logged")

# Create key for leverage lables
leverage_labels <- c("Included" = "All data points\nincluded",
                     "Excluded" = "Points of high\nleverage excluded")

# Create color key for simulations
color_brewer <- colorRampPalette(brewer.pal(n = 7, name = "Dark2"))
simulation_aes <- tibble(Simulation = simulation_levels,
                         Color = color_brewer(length(simulation_levels)))

# Create shape and transparency key for threshold levels
threshold_aes <- tibble(Threshold = threshold_levels,
                        Shape = c(15, 16, 17),
                        Alpha = c(0.4, 0.7, 1))

# Create colour and shape key for effects
effect_aes <- tibble(Effect = c("Unadjusted", "Adjusted"),
                     Color = c("violetred", "forestgreen"),
                     Shape = c(15, 16))

# ------------------------------------------------------------------------------
# Important dates
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create figure of important dates for a list of countries
# (1) countries = list of countries
# (2) dates = dataframe containing dates to display, with text descriptions and aes mappings (color, shape, size)
# (3) order = date by which to order countries in figure
# (4) out = folder to save figure
Plot_Important_Dates <- function(countries, dates, order, out) {
  
  # Filter observed dataset and summary data by specified countries and dates
  data_eur_filt <- data_eur %>% filter(Country %in% countries)
  summary_eur_filt <- summary_eur %>% filter(Country %in% countries) %>% 
    select(c(Country, dates$Date, Date_start, Date_T))
  
  # Order countries by date of first restriction
  countries_ordered <- summary_eur_filt %>% arrange(eval(parse(text = order))) %>% 
    pull(Country) %>% as.character
  
  # Convert summary data to long form
  summary_eur_filt_long <- summary_eur_filt %>% 
    pivot_longer(contains("Date"), names_to = "Date", values_to = "Value") %>%
    mutate(Date = factor(Date, levels = c(dates$Date, "Date_start", "Date_T")))
  
  # Get min and max dates from summary table
  date_min <- summary_eur_filt_long %>% pull(Value) %>% min(na.rm = TRUE)
  date_max <- summary_eur_filt_long %>% pull(Value) %>% max(na.rm = TRUE)
  
  # Plot specified countries and dates
  plot <- ggplot(data = summary_eur_filt_long %>% filter(Date %in% dates$Date), 
                 aes(x = Value, y = Country)) + 
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.position = "bottom") +
    labs(title = "Important dates in COVID-19 European policy responses",
         #subtitle = paste("Dates when:", paste(dates$Description, collapse = ", ")),
         caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker).") +
    theme(plot.caption = element_text(size = 7),
          plot.subtitle = element_text(size = 10)) +
    geom_line(data = summary_eur_filt_long %>% filter(Date %in% c("Date_start", "Date_T")),
              aes(group = Country),
              color = "grey70", alpha = 0.5, size = 2) +
    geom_point(aes(color = Date, shape = Date, size = Date)) +
    scale_color_manual(name = "Date of:",
                       values = dates$Color,
                       labels = dates$Description) +
    scale_shape_manual(name = "Date of:",
                       values = dates$Shape,
                       labels = dates$Description) +
    scale_size_manual(name = "Date of:",
                      values = dates$Size,
                      labels = dates$Description) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_date(name = "", 
                 limits = c(date_min - 7, date_max + 7), 
                 date_breaks = "1 week", 
                 date_labels = "%d %b %C",
                 expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(name = "",
                     limits = rev(countries_ordered)) 
  
  # Save plot to subfolder
  ggsave(paste0(out, "Figure - Important dates.png"), plot = plot, width = 10, height = 8)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Specify dates to include in figure
dates <- bind_rows(tibble(Date = "Date_1",
                          Description = "first confirmed case",
                          Color = "grey50",
                          Shape = "\u25CB",
                          Size = 4),
                   tibble(Date = "Date_first_restriction",
                          Description = "first restriction",
                          Color = "navyblue",
                          Shape = "\u25A0",
                          Size = 4),
                   tibble(Date = "Date_lockdown",
                          Description = "lockdown",
                          Color = "darkorange",
                          Shape = "\u25CF",
                          Size = 4),
                   tibble(Date = "Date_eased",
                          Description = "easing",
                          Color = "forestgreen",
                          Shape = "\u25BC",
                          Size = 3))

# Specify ordering variable
order <- "Date_first_restriction"

# Create figure
figure_dates <- Plot_Important_Dates(countries = countries_eur, 
                                     dates = dates,
                                     order = order, 
                                     out = results_directory)

# ------------------------------------------------------------------------------
# Fitted splines
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create figure of observed cumulative vs daily cases for a particular country,
# with fitted splines
# Arguments:
# (1) country = country to plot
# (3) out = folder to save combined figure
Plot_Splines <- function(country, out) {
  
  # Filter observed cases/deaths, best knots, and summary dataframes by country
  data_eur_country <- data_eur %>% filter(Country == country)
  knots_best_country <- knots_best %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Define important dates
  date_start <- summary_eur_country %>% pull(Date_start)  # first date of observed data included
  date_T <- summary_eur_country %>% pull(Date_T)  # final date of observed data to include
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  
  # Create In_range variable to indicate whether date is within range of observed data to include
  # (date_start <= Date <= date_T)
  data_eur_country <- data_eur_country %>% 
    mutate(In_range = ifelse(Date >= date_start & Date <= date_T, TRUE, FALSE))
  
  # Calculate max_date (max date to display on plots)
  max_date <- date_T + 7
  
  # Define x-axis range (cumulative cases)
  x_min <- 0
  x_max <- data_eur_country %>% filter(Date == max_date) %>% pull(Cumulative_cases_beg)
  
  # Define y-axis range (incident cases)
  y_min <- 0
  y_max <- data_eur_country %>% filter(Date <= max_date) %>% pull(Daily_cases) %>% max
  y_max <- 1.2*y_max  # (add buffer)
  
  # Plot observed cases
  plot <- ggplot(data = data_eur_country,
                 aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = country,
         subtitle = "Cumulative vs incident cases of COVID-19") +
    geom_path(data = filter(data_eur_country, Date <= date_start),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_path(data = filter(data_eur_country, In_range == TRUE),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 1) +
    geom_path(data = filter(data_eur_country, Date >= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_point(data = filter(data_eur_country, In_range == TRUE),
               alpha = 1, size = 1) +
    scale_x_continuous(name = "Cumulative number of cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "Daily number of cases",
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
  
  # Add fitted splines
  for (i in 1:nrow(knots_best_country)) {
    
    # Filter best knots dataset
    knots_best_country_i <- knots_best_country[i, ]
    
    # Define knot date pair
    knot_date_1 <- knots_best_country_i %>% pull(Knot_date_1)
    knot_date_2 <- knots_best_country_i %>% pull(Knot_date_2)
    
    # Define values of cumulative cases at knot dates
    knot_1 <- data_eur_country %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
    knot_2 <- data_eur_country %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
    
    # Calculate min and max values of cumulative cases in modelling period
    min_cc <- data_eur_country %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% min
    max_cc <- data_eur_country %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% max
    
    # Define Arima spline parameters
    slope_1 <- knots_best_country_i %>% pull(Growth_factor_1) %>% head(1) - 1
    slope_2 <- knots_best_country_i %>% pull(Growth_factor_2) %>% head(1) - 1
    slope_3 <- knots_best_country_i %>% pull(Growth_factor_3) %>% head(1) - 1
    intercept_1 <- knots_best_country_i %>% pull(Intercept_1) %>% head(1)
    intercept_2 <- knots_best_country_i %>% pull(Intercept_2) %>% head(1)
    intercept_3 <- knots_best_country_i %>% pull(Intercept_3) %>% head(1)
    
    # Add fitted line
    if (knot_date_1 == date_start) {
      if (is.na(knot_date_2)) {  # NO knot points
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = max_cc,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*max_cc),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed")
      } else {  # ONE knot point (at knot_date_2)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_2,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_2),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*max_cc),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") 
      }
    } else {
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = max_cc,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*max_cc),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") 
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = knot_2,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                       color = "royalblue", size = 0.1, alpha = 0.1, linetype = "dashed") 
      }
    }  # (close if-else section)
  }  # (close fitted line section)
  
  # Save plot to subfolder
  ggsave(paste0(out, "/", country, ".png"), plot = plot, width = 6, height = 6)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Create folder for storing figures of exponential growth with fitted splines, 
# if none already exists
out_folder <- paste0(results_directory, "Figures - Fitted splines by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Create figures (individual)
figure_splines <- foreach(i = countries_eur_modelled, .errorhandling = "pass") %do% 
  Plot_Splines(country = i, 
               out = out_folder)

# Create figures (combined)
rows <- length(figure_splines) %>% sqrt %>% ceiling
cols <- length(figure_splines) %>% sqrt %>% floor
p <- ggarrange(plotlist = figure_splines, nrow = rows, ncol = cols)
p_annotated <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 30))
ggsave(paste0(results_directory, "Figure - Fitted splines.png"),
       plot = p_annotated, width = 6*cols, height = 6*rows, limitsize = FALSE)

rm(rows, cols, p, p_annotated)

# ------------------------------------------------------------------------------
# Growth factor under lockdown vs cases at lockdown
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create figure of cases on date of lockdown vs growth factor under lockdown
# Arguments: 
# (1) countries = list of countries
# (2) cases = type of cases to display (cumulative or incident moving average)
# (3) log = whether cases should be logged (T/F)
# (4) out = folder to save figure
Plot_Growth_Factor_Lockdown <- function(countries,
                                        cases = c("Daily_cases_MA7",
                                                  "Cumulative_cases_beg_MA7"),
                                        log = c(TRUE, FALSE),
                                        out) {
  
  # Record number of specified cases
  n_cases <- length(cases)
  
  # Filter cases/deaths, summary, best knots, and median growth factor dataframes 
  # by countries and select relevant variables
  data_eur_lockdown <- data_eur %>% filter(Country %in% countries) %>%
    select(Country, Date, all_of(cases)) 
  summary_eur_lockdown <- summary_eur %>% filter(Country %in% countries) %>%
    select(Country, Date_lockdown)
  knots_best_lockdown <- knots_best %>% filter(Country %in% countries)
  median_growth_factors_lockdown <- median_growth_factors %>% filter(Country %in% countries)
  
  # Calculate median growth factor under lockdown
  median_growth_factors_lockdown <- median_growth_factors_lockdown %>% 
    group_by(Country) %>% 
    summarise(Median_growth_factor_lockdown = ifelse(!is.na(Median_growth_factor_3),
                                                     Median_growth_factor_3, Median_growth_factor_2),
              .groups = "keep") %>% ungroup
  
  # Bind data together in single dataframe
  all_data_lockdown <- summary_eur_lockdown %>%
    full_join(., median_growth_factors_lockdown, by = "Country") %>%
    full_join(., data_eur_lockdown, by = "Country") %>% 
    filter(Date == Date_lockdown) %>% select(-contains("Date")) 
  
  # Convert data to long format
  all_data_lockdown <- all_data_lockdown %>% 
    gather(Case_type, Cases, contains("cases")) %>% 
    mutate(Case_type = factor(Case_type, levels = cases)) %>%
    arrange(Country)
  
  # Create key for case labels
  case_labels <- c(Daily_cases = "Daily cases", 
                   Daily_cases_MA7 = "Daily cases (MA7)",
                   Cumulative_cases_beg = "Cumulative cases", 
                   Cumulative_cases_beg_MA7 = "Cumulative cases (MA7)")
  
  # Define x-axis variable (logged or non-logged cases)
  x_var <- ifelse(log == FALSE, "Cases", "log(Cases)")
  
  # Plot cases on date of lockdown vs growth factor
  plot <- ggplot(data = all_data_lockdown,
                 aes(x = eval(parse(text = x_var)), 
                     y = Median_growth_factor_lockdown)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          panel.background = element_blank(),
          panel.border = element_rect(color = "grey95", fill = NA),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left = element_line(color = "black"),
          panel.grid.major = element_line(color = "grey95")) +
    labs(title = "Relationship between number of cases of COVID-19 on the date of lockdown and growth factor under lockdown") +
    geom_point() +
    geom_text_repel(aes(label = Country), size = 2, alpha = 0.4,
                    max.overlaps = Inf) +
    #geom_smooth(method = 'lm', formula = y ~ x, fullrange = TRUE) +
    facet_wrap(. ~ Case_type, ncol = 2, scales = "free",
               labeller = labeller(Case_type = case_labels)) +
    scale_x_continuous(name = x_var,
                       labels = comma_format(accuracy = 1)) +
    scale_y_continuous(name = "Growth factor under lockdown") 
  
  # Save plot to subfolder
  ggsave(paste0(out, "Figure - Cases at lockdown vs growth factor under lockdown.png"), 
         plot = plot, width = 10, height = 2.5*n_cases)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Specify countries and to display
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded_all]

# Create figure
figure_growth_factor <- Plot_Growth_Factor_Lockdown(countries = countries,
                                                    cases = c("Daily_cases_MA7",
                                                              "Cumulative_cases_beg_MA7"),
                                                    log = TRUE,
                                                    out = results_directory)

# ------------------------------------------------------------------------------
# Length of lockdown (observed) vs cases at lockdown
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create figure of cases on date of lockdown vs length of lockdown
# Arguments: 
# (1) countries = list of countries
# (2) cases = type of cases to display (cumulative or incident, moving average or raw)
# (3) log = whether cases should be logged (T/F)
# (4) out = folder to save figure
Plot_Length_Lockdown <- function(countries,
                                 cases = c("Daily_cases",
                                           "Daily_cases_MA7",
                                           "Cumulative_cases_beg", 
                                           "Cumulative_cases_beg_MA7"),
                                 log = c(TRUE, FALSE),
                                 out) {
  
  # Record number of specified cases
  n_cases <- length(cases)
  
  # Filter cases and summary dataframes by countries and select relevant variables
  data_eur_lockdown <- data_eur %>% filter(Country %in% countries) %>%
    select(Country, Date, all_of(cases)) 
  summary_eur_lockdown <- summary_eur %>% filter(Country %in% countries) %>%
    select(Country, Date_lockdown, Length_lockdown)
  
  # Bind data together in single dataframe
  all_data_lockdown <- summary_eur_lockdown %>%
    full_join(., data_eur_lockdown, by = "Country") %>% 
    filter(Date == Date_lockdown) %>% select(-contains("Date")) 
  
  # Convert data to long format
  all_data_lockdown <- all_data_lockdown %>% 
    gather(Case_type, Cases, contains("cases")) %>% 
    mutate(Case_type = factor(Case_type, levels = cases)) %>%
    arrange(Country)
  
  # Create key for case labels
  case_labels <- c(Daily_cases = "Daily cases", 
                   Daily_cases_MA7 = "Daily cases (MA7)",
                   Cumulative_cases_beg = "Cumulative cases", 
                   Cumulative_cases_beg_MA7 = "Cumulative cases (MA7)")
  
  # Define x-axis variable (logged or non-logged cases)
  x_var <- ifelse(log == FALSE, "Cases", "log(Cases)")

  # Plot cases on date of lockdown vs length of lockdown
  plot <- ggplot(data = all_data_lockdown,
                 aes(x = eval(parse(text = x_var)),
                     y = Length_lockdown)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          panel.background = element_blank(),
          panel.border = element_rect(color = "grey95", fill = NA),
          axis.line.x.bottom = element_line(color = "black"),
          axis.line.y.left = element_line(color = "black"),
          panel.grid.major = element_line(color = "grey95")) +
    labs(title = "Relationship between number of cases of COVID-19 on the date of lockdown and length of lockdown") +
    geom_point() +
    geom_text_repel(aes(label = Country), size = 2, alpha = 0.4,
                    max.overlaps = Inf) +
    #geom_smooth(method = 'lm', formula = y ~ x, fullrange = TRUE) +
    facet_wrap(. ~ Case_type, ncol = 2, scales = "free",
               labeller = labeller(Case_type = case_labels)) +
    scale_x_continuous(name = x_var,
                       labels = comma_format(accuracy = 1)) +
    scale_y_continuous(name = "Length of lockdown (days)") 
  
  # Save plot to subfolder
  ggsave(paste0(out, "Figure - Cases at lockdown vs length of lockdown.png"), 
         plot = plot, width = 10, height = 2.5*n_cases)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Specify countries and to display
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded_all]

# Create figure
figure_length_lockdown <- Plot_Length_Lockdown(countries = countries,
                                                    cases = c("Daily_cases_MA7",
                                                              "Cumulative_cases_beg_MA7"),
                                                    log = TRUE,
                                                    out = results_directory)

# ------------------------------------------------------------------------------
# Length of time to reach threshold (simulated)
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Figure to create faceted plot of the time taken for a group of countries 
# to reach important thresholds in different simulations
# Arguments:
# (1) countries = list of countries
# (2) simulations = vector of simulations to include
# (3) out = folder to save figure
Plot_Time_To_Threshold <- function(countries, simulations, out) {
  
  # Filter thresholds dataframe by specified countries and simulations,
  # and order Simulation and History factor levels
  summary_thresholds_sim_countries <- summary_thresholds_sim_all %>% 
    filter(Country %in% countries, Simulation %in% simulations) 
  
  # Record number of specified simulations
  n_sim <- length(simulations)
  
  # Define thresholds
  thresholds <- summary_thresholds_sim_countries %>% pull(Threshold) %>% levels
  
  # Order countries by number of days to reaching lowest threshold
  countries_ordered <- summary_thresholds_sim_countries %>% 
    filter(History == "Natural history",
           Threshold == "0.0010%") %>%
    arrange(Days_since_lockdown) %>% pull(Country) %>% as.character
  
  # Relevel thresholds dataframe using ordered countries
  summary_thresholds_sim_countries <- summary_thresholds_sim_countries %>% 
    mutate(Country = factor(Country, levels = countries_ordered))
  
  # Create plot
  plot <- ggplot(data = summary_thresholds_sim_countries,
                 aes(x = Days_since_lockdown, y = Country,
                     color = Simulation,
                     shape = Threshold,
                     alpha = Threshold)) +
    theme_light() +
    theme(axis.title.y = element_blank(),
          strip.text.y.left = element_text(angle = 0, hjust = 0.95, vjust = 0.5),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          legend.position = "bottom",
          plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
    guides(color = FALSE) +
    geom_point(size = 2) +
    facet_wrap(History + Simulation ~ ., ncol = n_sim) +
    labs(title = "Length of time to reach threshold") +
    scale_color_manual(values = simulation_aes$Color, 
                       breaks = simulation_aes$Simulation) +
    scale_shape_manual(name = "Threshold:",
                       values = threshold_aes$Shape,
                       labels = threshold_aes$Threshold) +
    scale_alpha_manual(name = "Threshold:",
                       values = threshold_aes$Alpha,
                       labels = threshold_aes$Threshold) +
    scale_x_continuous(name = "Days since lockdown") +
    scale_y_discrete(limits = rev(levels(summary_thresholds_sim_countries$Country)))
  
  # Save plot to output folder
  ggsave(paste0(out, "Figure - Time to threshold.png"),
         plot = plot, width = 2 + 3*n_sim, height = 7)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Specify countries to display
countries <- countries_eur_lockdown[!countries_eur_lockdown %in% countries_excluded_time_to_threshold]

# Specify simulations to include in figures 
#simulations <- c("0,0", "7,7", "14,14")
simulations <- c("0,0", "0,1", "0,3", "0,5", "0,7")

# Create figure
plot_time_to_threshold <- Plot_Time_To_Threshold(countries = countries,
                                                 simulations = simulations,
                                                 out = results_directory)

# ------------------------------------------------------------------------------
# Simulation results: natural vs counterfactual histories
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create three-panel figure of simulation results for a specified country
# (from functions Plot_Daily_Cases_Sim, Plot_Cumulative_Cases_Sim, Plot_Exponential_Growth_Sim)
# with common legend and title
# Arguments:
# (1) country = country to plot
# (2) simulations = vector of simulations to include
# (3) thresholds = vector of thresholds to display in figures
# (4) out = folder to save combined figure
Plot_Simulation_Results <- function(country, simulations, thresholds, out) {
  
  # Filter observed cases/deaths, best knots, and summary dataframes by country
  data_eur_country <- data_eur %>% filter(Country == country)
  knots_best_country <- knots_best %>% filter(Country == country)
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Define important dates
  date_start <- summary_eur_country %>% pull(Date_start)  # first date of observed data included
  date_T <- summary_eur_country %>% pull(Date_T)  # final date of observed data to include
  date_1 <- summary_eur_country %>% pull(Date_1)
  date_first_restriction <- summary_eur_country %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_country %>% pull(Date_lockdown)
  
  # If country did not enter lockdown, retain only unique simulations
  # (i.e. where date of first restriction is different)
  if (is.na(date_lockdown)) {
    df <- data.frame(Simulations = simulations)
    df <- df %>% separate(Simulations, c("N_days_first_restriction", "N_days_lockdown"), sep = ",") %>%
      arrange(N_days_first_restriction, N_days_lockdown) %>%
      group_by(N_days_first_restriction) %>% slice(1) %>%
      mutate(Simulations = paste(N_days_first_restriction, N_days_lockdown, sep = ",")) 
    simulations <- df %>% pull(Simulations)
  }
  
  # Filter simulation results by specified country and simulations
  summary_cases_sim_country <- summary_cases_sim_all %>%
    filter(Country == country, Simulation %in% simulations) %>% droplevels
  summary_thresholds_sim_country <- summary_thresholds_sim_all %>% 
    filter(Country == country, Simulation %in% simulations,
           Threshold %in% thresholds) %>% droplevels
  
  # If no simulated data for country, print warning and stop
  if (nrow(summary_cases_sim_country) == 0) { 
    stop(paste0("No simulated data for ", country, "."))
  }
  
  # Create key to map generic simulation description onto simulation actually implemented
  # and define actual simulations implemented
  simulations_actual_key <- summary_cases_sim_country %>% 
    select(History, Simulation, History, N_days_first_restriction, N_days_lockdown) %>% 
    unique %>% 
    mutate(Simulation_actual = paste(N_days_first_restriction, N_days_lockdown, sep = ","),
           Simulation_actual = factor(Simulation_actual, levels = unique(Simulation_actual))) %>%
    select(-contains("N_days")) 
  simulations_actual <- simulations_actual_key %>% pull(Simulation_actual)
  
  # Edit simulation descriptions in summary dataframes to reflect intervention actually implemented
  summary_cases_sim_country <- summary_cases_sim_country %>% 
    full_join(., simulations_actual_key, by = c("History", "Simulation")) %>%
    mutate(Simulation = Simulation_actual) %>%
    select(-Simulation_actual) 
  summary_thresholds_sim_country <- summary_thresholds_sim_country %>% 
    full_join(., simulations_actual_key, by = c("History", "Simulation")) %>%
    mutate(Simulation = Simulation_actual) %>%
    select(-Simulation_actual) 
  
  # Modify simulation aesthetics to reflect actual interventions implemented in country
  simulation_aes_country <- simulations_actual_key %>% 
    left_join(., simulation_aes, by = "Simulation") %>%
    select(-Simulation) %>%
    rename(Simulation = Simulation_actual)
  
  # Create In_range variable to indicate whether date is within range of observed data to include
  # (date_start <= Date <= date_T)
  data_eur_country <- data_eur_country %>% 
    mutate(In_range = ifelse(Date >= date_start & Date <= date_T, TRUE, FALSE))
  
  # Pull first date for which incident cases go below lowest threshold
  date_lowest_threshold <- summary_thresholds_sim_country %>% filter(Threshold_value == min(Threshold_value)) %>% 
    pull(Date_cases_below_threshold) %>% max(na.rm = TRUE)
  
  # Calculate min_date (min date to display on plots)
  min_date <- date_1 - 14
  
  # Calculate max_date (max date to display on plots)
  if (is.infinite(date_lowest_threshold)) { 
    max_date <- date_T + 21
  } else {
    max_date <- max(date_lowest_threshold + 14, date_T + 14)
  }
  
  # Create plots
  plot_inc <- Plot_Daily_Cases_Sim(min_date = min_date, 
                                   max_date = max_date,
                                   obs_data = data_eur_country,
                                   sim_data = summary_cases_sim_country,
                                   threshold_data = summary_thresholds_sim_country,
                                   simulations = simulations_actual,
                                   aesthetics = simulation_aes_country)
  plot_cum <- Plot_Cumulative_Cases_Sim(min_date = min_date, 
                                        max_date = max_date,
                                        obs_data = data_eur_country,
                                        sim_data = summary_cases_sim_country,
                                        simulations = simulations_actual,
                                        aesthetics = simulation_aes_country,
                                        date_T = date_T)
  plot_exp <- Plot_Exponential_Growth_Sim(max_date = max_date,
                                          obs_data = data_eur_country,
                                          sim_data = summary_cases_sim_country,
                                          simulations = simulations_actual,
                                          aesthetics = simulation_aes_country,
                                          knots = knots_best_country, 
                                          date_start = date_start, 
                                          date_T = date_T)
  
  # Combine in triple panel with common legend and country as title
  plots_all <- ggarrange(plotlist = list(plot_inc, plot_cum, plot_exp), align = "h",
                         common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 3)
  plots_all_annotated <- annotate_figure(plots_all, top = text_grob(paste0(country),  size = 20),
                                         bottom = text_grob("Data from https://github.com/CSSEGISandData/COVID-19", size = 8))
  
  # Save plot to subfolder
  ggsave(paste0(out, "/", country, ".png"), plot = plots_all_annotated, width = 6*3, height = 6)
  
  # Return combined plots
  return(list(plots_all_annotated))
  
}

# Function to create figure of observed daily cases for a particular country,
# with simulated cumulative cases (natural and/or counterfactual histories) overlaid
# Arguments:
# (1) min_date = min date to display
# (2) max_date = max date to display
# (3) obs_data = dataframe of observed incidence/cumulative cases data for given country
# (4) sim_data = dataframe of simulated incidence/cumulate cases data for given country
# (5) simulations = vector of simulations to include
# (6) aesthetics = aesthetic mapping for simulation onto colours for given country
# (7) threshold_data = dataframe containing population-based thresholds and threshold values
Plot_Daily_Cases_Sim <- function(min_date, max_date, obs_data, sim_data, 
                                 simulations, aesthetics, threshold_data) {
  
  # Define x-axis range
  x_min <- min_date
  x_max <- max_date
  
  # Define min y-axis value
  y_min <- 0
  
  # Calculate max y-axis value as upper limit of incident case data in date range 
  # (95% SI upper bound or max number of observed cases, whichever is greater)
  y_max <- max(filter(sim_data, Date <= max_date)$C_975_daily_cases, 
               filter(obs_data, In_range == TRUE)$Daily_cases)
  
  # Create dataframe which combines threshold aesthetics with threshold values
  threshold_values <- threshold_data %>% 
    select(Threshold, Threshold_value) %>% unique %>% 
    left_join(., threshold_aes, by = "Threshold") %>%
    mutate(Threshold = factor(Threshold, levels = threshold_levels))
  
  # Plot observed cases and threshold values
  plot <- ggplot(data = obs_data,
                 aes(x = Date, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Incident cases of COVID-19") +
    geom_col(data = filter(obs_data, In_range == FALSE), 
             aes(x = Date, y = Daily_cases), alpha = 0.2) +
    geom_col(data = filter(obs_data, In_range == TRUE), 
             aes(x = Date, y = Daily_cases), alpha = 0.5) +
    geom_hline(data = threshold_values, 
               aes(yintercept = Threshold_value, alpha = Threshold),
               linetype = "dotdash", color = "firebrick",
               show.legend = FALSE) +
    geom_text(data = threshold_values, 
              aes(x = min_date + 0.01*(x_max - min_date), 
                  y = Threshold_value + 0.01*y_max, 
                  label = Threshold, alpha = Threshold), 
              hjust = 0, vjust = 0, size = 3, color = "firebrick",
              show.legend = FALSE) +
    scale_alpha_manual(values = threshold_aes$Alpha, 
                       breaks = threshold_aes$Threshold) +
    scale_x_date(name = "Date", limits = c(x_min, x_max), 
                 date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Daily number of cases",
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(ylim = c(y_min, y_max), expand = FALSE)
  
  # Add simulated lines
  plot <- plot + 
    geom_line(data = sim_data,
              aes(x = Date, y = Mean_daily_cases, 
                  group = Simulation,
                  color = Simulation), 
              size = 1) +
    geom_ribbon(data = sim_data,
                aes(x = Date, y = Mean_daily_cases, 
                    ymin = C_025_daily_cases, ymax = C_975_daily_cases,
                    group = Simulation,
                    fill = Simulation),
                alpha = 0.15) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       breaks = aesthetics$Simulation) +
    scale_fill_manual(name = "Simulation:",
                      values = aesthetics$Color, 
                      breaks = aesthetics$Simulation)
  
  # Return plot
  return(plot)
  
}

# Function to create figure of observed cumulative cases for a particular country,
# with simulated cumulative cases (natural and/or counterfactual histories) overlaid
# Arguments:
# (1) min_date = min date to display
# (2) max_date = max date to display
# (3) obs_data = dataframe of observed incidence/cumulative cases data for given country
# (4) sim_data = dataframe of simulated incidence/cumulate cases data for given country
# (5) simulations = vector of simulations to include
# (6) aesthetics = aesthetic mapping for simulation onto colours for given country
# (7) date_T = last date of observed data included in modelling period
Plot_Cumulative_Cases_Sim <- function(min_date, max_date, obs_data, sim_data, 
                                      simulations, aesthetics, date_T) {
  
  # Define x-axis range
  x_min <- min_date
  x_max <- max_date
  
  # Define min y-axis value
  y_min <- 0
  
  # Calculate max y-axis value as upper limit of cumulative case data in displayed date range 
  # (95% SI upper bound or max number of observed cases, whichever is greater)
  y_max <- max(filter(sim_data, Date <= max_date)$C_975_cumulative_cases_end, 
               filter(obs_data, In_range == TRUE)$Cumulative_cases_end)
  
  # Plot observed cases and threshold values
  plot <- ggplot(data = obs_data,
                 aes(x = Date, y = Cumulative_cases_end)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative cases of COVID-19") +
    geom_col(data = filter(obs_data, In_range == FALSE), 
             aes(x = Date, y = Cumulative_cases_end), alpha = 0.2) +
    geom_col(data = filter(obs_data, In_range == TRUE), 
             aes(x = Date, y = Cumulative_cases_end), alpha = 0.5) +
    scale_x_date(name = "Date", limits = c(x_min, x_max), 
                 date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Cumulative number of cases",
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(ylim = c(y_min, y_max), expand = FALSE)
  
  # Add simulated lines, text containing cumulative cases on date_T
  plot <- plot + 
    geom_line(data = sim_data,
              aes(x = Date, y = Mean_cumulative_cases_end, 
                  group = Simulation,
                  color = Simulation), 
              size = 1) +
    geom_ribbon(data = sim_data,
                aes(x = Date, y = Mean_cumulative_cases_end, 
                    ymin = C_025_cumulative_cases_end, 
                    ymax = C_975_cumulative_cases_end,
                    group = Simulation,
                    fill = Simulation),
                alpha = 0.15) +
    geom_text_repel(data = sim_data,
                    aes(x = Date, y = Mean_cumulative_cases_end,
                        color = Simulation,
                        label = ifelse(Date == date_T, 
                                       formatC(Mean_cumulative_cases_end, 
                                               format = "f", big.mark = ",", digits = 0), "")),
                    bg.color = "white", bg.r = 0.25,
                    hjust = 0.5, vjust = 0.5, size = 3, fontface = 2, 
                    inherit.aes = FALSE,
                    max.overlaps = Inf) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       breaks = aesthetics$Simulation) +
    scale_fill_manual(name = "Simulation:",
                      values = aesthetics$Color, 
                      breaks = aesthetics$Simulation)
  
  # Return plot
  return(plot)
  
}

# Function to create figure of observed cumulative vs daily cases for a particular country,
# with fitted splines and simulated cumulative vs daily cases (natural and/or counterfactual) overlaid
# Arguments:
# (1) max_date = max date to display
# (2) obs_data = dataframe of observed incidence/cumulative cases data
# (3) sim_data = dataframe of simulated incidence/cumulative cases data
# (4) simulations = vector of simulations to include
# (5) aesthetics = aesthetic mapping for simulation onto colours for given country
# (6) knots = dataframe of spline parameters (knot points, growth factors)
# (7) date_start = first date of observed data included in modelling period
# (8) date_T = last date of observed data included in modelling period
Plot_Exponential_Growth_Sim <- function(max_date, obs_data, sim_data, 
                                        simulations, aesthetics, knots, date_start, date_T) {
  
  # Define x-axis range (cumulative cases)
  x_min <- 0
  x_max <- sim_data %>% filter(Date == max_date) %>% pull(Mean_cumulative_cases_beg) %>% max
  
  # Define y-axis range (incident cases)
  y_min <- 0
  y_max <- max(filter(sim_data, Date <= max_date)$C_975_daily_cases, 
               filter(obs_data, In_range == TRUE)$Daily_cases)
  
  # Define color, size, and transparency for fitted lines
  color <- simulation_aes %>% filter(Simulation == "0,0") %>% pull(Color)
  size <- 0.1
  alpha <- 0.05
  
  # Plot observed cases
  plot <- ggplot(data = obs_data,
                 aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative vs incident cases of COVID-19") +
    geom_path(data = filter(obs_data, Date <= date_start),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.2) +
    geom_path(data = filter(obs_data, In_range == TRUE),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_path(data = filter(obs_data, Date >= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.2) +
    geom_point(data = filter(obs_data, In_range == TRUE),
               alpha = 0.5, size = 0.5) +
    scale_x_continuous(name = "Cumulative number of cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "Daily number of cases",
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
  
  # Add fitted splines
  for (i in 1:nrow(knots)) {
    
    # Filter best knots dataset
    knots_i <- knots[i, ]
    
    # Define knot date pair
    knot_date_1 <- knots_i %>% pull(Knot_date_1)
    knot_date_2 <- knots_i %>% pull(Knot_date_2)
    
    # Define values of cumulative cases at knot dates
    knot_1 <- obs_data %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
    knot_2 <- obs_data %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
    
    # Calculate min and max values of cumulative cases in modelling period
    min_cc <- obs_data %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% min
    max_cc <- obs_data %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% max
    
    # Define Arima spline parameters
    slope_1 <- knots_i %>% pull(Growth_factor_1) %>% head(1) - 1
    slope_2 <- knots_i %>% pull(Growth_factor_2) %>% head(1) - 1
    slope_3 <- knots_i %>% pull(Growth_factor_3) %>% head(1) - 1
    intercept_1 <- knots_i %>% pull(Intercept_1) %>% head(1)
    intercept_2 <- knots_i %>% pull(Intercept_2) %>% head(1)
    intercept_3 <- knots_i %>% pull(Intercept_3) %>% head(1)
    
    # Add fitted line
    if (knot_date_1 == date_start) {
      if (is.na(knot_date_2)) {  # NO knot points
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = max_cc,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed")
      } else {  # ONE knot point (at knot_date_2)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_2,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_2),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed") 
      }
    } else {
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = max_cc,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed") 
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = knot_2,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed") 
      }
    }  # (close if-else section)
  }  # (close fitted line section)
  
  # Add simulated lines
  plot <- plot + geom_line(data = sim_data,
                           aes(x = Mean_cumulative_cases_beg, y = Mean_daily_cases,
                               group = Simulation,
                               color = Simulation),
                           size = 1) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       breaks = aesthetics$Simulation)
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Create folder for storing figures of incident and cumulative cases by country, 
# if none already exists
out_folder <- paste0(results_directory, "Figures - Simulation results by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Specify countries to display
countries <- countries_eur_modelled

# Specify simulations to include in figures 
#simulations <- c("0,0", "7,7", "14,14")
simulations <- c("0,0", "0,3", "0,7", "7,7", "14,14")

# Specify thresholds to include in figure
thresholds <- summary_thresholds_sim_all %>% pull(Threshold) %>% unique

# Create figures
figure_sim_results <- foreach(i = countries, .errorhandling = "pass") %do% 
  Plot_Simulation_Results(country = i, 
                          simulations = simulations,
                          thresholds = thresholds, 
                          out = out_folder)

# ------------------------------------------------------------------------------
# Simulation results:  model residuals
# ------------------------------------------------------------------------------

## Functions -------------------------------------------------------------------

# Function to create two-panel figure of residual plots (for both incident and cumulative cases)
# of simulation results for a particular country
# Arguments:
# (1) country = country to plot
# (2) out = folder to save combined figure
Plot_Model_Residuals_Combined <- function(country, out) {
  
  # Plot residuals of incident cases
  plot_inc <- Plot_Model_Residuals(country = country,
                                   cases = "Daily_cases",
                                   out = out)
  
  # Plot residuals of cumulative cases
  plot_cum <- Plot_Model_Residuals(country = country,
                                   cases = "Cumulative_cases_end",
                                   out = out)
  
  # Create copy of plots with description as title
  plot_inc_copy <- plot_inc + 
    labs(title = "Model residuals: incident cases")
  plot_cum_copy <- plot_cum +
    labs(title = "Model residuals: cumulative cases")
  
  # Combine copied plots in double panel with country as title
  plots_all <- ggarrange(plotlist = list(plot_inc_copy, plot_cum_copy), align = "h",
                         nrow = 1, ncol = 2)
  plots_all_annotated <- annotate_figure(plots_all, top = text_grob(paste0(country),  size = 20))
  
  # Save combined plot to subfolder
  ggsave(paste0(out, "/", country, ".png"), plot = plots_all_annotated, width = 6*2, height = 6)
  
  # Return lists of individual plots
  return(list(plot_inc = plot_inc, plot_cum = plot_cum))
  
}

# Function to create single figure of model residual plots
# Arguments:
# (1) country = country to plot
# (2) cases = type of cases to display (cumulative or incident)
# (3) out = folder to save combined figure
Plot_Model_Residuals <- function(country, cases = c("Daily_cases", "Cumulative_cases_beg"), out) {
  
  # Filter observed cases/deaths and summary dataframes by country, and
  # select relevant variables
  data_eur_country <- data_eur %>% filter(Country == country) %>%
    select(Country, Date, all_of(cases))
  summary_eur_country <- summary_eur %>% filter(Country == country)
  
  # Filter simulation results by specified country, natural history, and 
  # select relevant variables
  summary_cases_sim_country <- summary_cases_sim_all %>%
    filter(Country == country, History == "Natural history") %>%
    select(Country, Date, paste0("Mean_", tolower(cases)))
  
  # If no simulated data for country, print warning and stop
  if (nrow(summary_cases_sim_country) == 0) { 
    stop(paste0("No simulated data for ", country, "."))
  }
  
  # Record important dates
  date_start <- summary_eur_country %>% pull(Date_start)  # first date of observed data included
  date_T <- summary_eur_country %>% pull(Date_T)  # final date of observed data to include
  
  # Filter observed and simulated data within modelling period, and
  # rename cases in dataframes to observed and predicted
  data_eur_country <- data_eur_country %>% 
    filter(Date >= date_start, Date <= date_T) %>%
    rename(Observed_cases = cases)
  summary_cases_sim_country <- summary_cases_sim_country %>% 
    filter(Date >= date_start, Date <= date_T) %>%
    rename(Predicted_cases = paste0("Mean_", tolower(cases)))
  
  # Join observed and simulated data, calculate residuals
  residuals <- full_join(data_eur_country, summary_cases_sim_country, 
                         by = c("Date", "Country")) %>%
    mutate(Residual = Observed_cases - Predicted_cases)
  
  # Calculate absolute maximum value of residuals
  max_res <- residuals %>% pull(Residual) %>% abs %>% max
  
  # Define subtitle
  if (cases == "Daily_cases") {
    subtitle <- "Model residuals: incident cases"
  } else {
    subtitle <- "Model residuals: cumulative cases"
  }
  
  # Plot residuals against date
  plot <- ggplot(data = residuals, aes(x = Date, y = Residual)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = country) +
    geom_hline(yintercept = 0, color = "firebrick") +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Residual",
                       limits = c(-1.2*max_res, 1.2*max_res), 
                       labels = comma_format(accuracy = 1))
  
  # Return plot
  return(plot)
  
}

## Figures ---------------------------------------------------------------------

# Create folder for storing figures of model residuals by country, 
# if none already exists
out_folder <- paste0(results_directory, "Figures - Model residuals by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Create figures
figure_model_residuals <- foreach(i = countries_eur_modelled, .errorhandling = "pass") %do% 
  Plot_Model_Residuals_Combined(country = i,
                                out = out_folder)

# Create separate lists for residuals of incident and cumulative cases
figure_model_residuals_inc <- map(.x = figure_model_residuals,
                                  .f = ~.x$plot_inc) 
figure_model_residuals_cum <- map(.x = figure_model_residuals,
                                  .f = ~.x$plot_cum) 

# Create combined figures (all countries) for residuals of incident and cumulative cases
rows <- length(figure_model_residuals_inc) %>% sqrt %>% ceiling
cols <- length(figure_model_residuals_inc) %>% sqrt %>% floor
## Incident cases
p_inc <- ggarrange(plotlist = figure_model_residuals_inc, nrow = rows, ncol = cols)
p_inc_annotated <- annotate_figure(p_inc, 
                                   top = text_grob("Model residuals: incident cases", size = 30))
ggsave(paste0(results_directory, "Figure - Model residuals (incident cases).png"),
       plot = p_inc_annotated, width = 6*cols, height = 6*rows, limitsize = FALSE)
## Cumulative cases
p_cum <- ggarrange(plotlist = figure_model_residuals_cum, nrow = rows, ncol = cols)
p_cum_annotated <- annotate_figure(p_cum, 
                                   top = text_grob("Model residuals: cumulative cases", size = 30))
ggsave(paste0(results_directory, "Figure - Model residuals (cumulative cases).png"),
       plot = p_cum_annotated, width = 6*cols, height = 6*rows, limitsize = FALSE)

# ------------------------------------------------------------------------------
# Analysis: effect sizes
# ------------------------------------------------------------------------------

## (1) Between-country effects -------------------------------------------------

### Functions ------------------------------------------------------------------

# Function to create individual and multi-panel figure of between-country effects 
# of cases at lockdown on time to reach important thresholds, length of lockdown, 
# and growth factor under lockdown, for a specified set of exposures
# Arguments:
# (1) exposures = vector of exposures to display
# (2) plots = plots to display in combination
# (3) out = folder to save combined figure
# Returns: list of 4 figures - each individual, and specified combination
Plot_Between_Country_Effects <- function(exposures = c("Daily_cases_MA7",
                                                       "Cumulative_cases_beg_MA7"), 
                                         plots = c("plot_length_lockdown",
                                                   "plot_growth_factor"), 
                                         out) {
  
  
  # Record number of specified exposures and plots
  n_exp <- length(exposures)
  n_plots <- length(plots)
  
  # Filter between-country effects by specified exposures
  effects_between_countries_exposures <- 
    map(.x = as.list(exposures),
        .f = ~filter(effects_between_countries, str_detect(Exposure, .x))) %>%
    bind_rows
  
  # Plot length of lockdown
  plot_length_lockdown <- Plot_Between_Length_Lockdown(effects = effects_between_countries_exposures)
  
  # Plot growth factor under lockdown
  plot_growth_factor <- Plot_Between_Growth_Factor(effects = effects_between_countries_exposures)
  
  # Create list of specified plots
  plot_list <- map(.x = plots, .f = ~eval(parse(text = .x)))
  
  # Combine figures of between-country effects in double panel with common legend, annotate 
  plots_all <- ggarrange(plotlist = plot_list,
                         align = "hv", common.legend = TRUE, legend = "bottom", ncol = n_plots)
  plots_all_annotated <- annotate_figure(plots_all,
                                         top = text_grob("Estimated effects of each additional case of COVID-19 at lockdown"))
  
  # Save combined plot to Results folder
  ggsave(paste0(results_directory, "Figure - Effects between countries.png"), 
         plot = plots_all_annotated, width = 3*n_exp*n_plots, height = 7)
  
  # Return list of individual and combined plots
  return(list(plot_length_lockdown = plot_length_lockdown, 
              plot_growth_factor = plot_growth_factor,
              plot_combined = plots_all_annotated))
  
}

# Function to create figure of between-country effect of cases at lockdown
# on length of lockdown 
# Arguments:
# (1) effects = dataframe containing estimated effects
Plot_Between_Length_Lockdown <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome and threshold
  effects_length_lockdown <- effects %>% 
    filter(Outcome == "Length_lockdown")
  
  # Calculate maximum deviation from zero across all confidence intervals
  max_y <- effects_length_lockdown %>% pull(CI_lower, CI_upper) %>% abs %>% max 
  
  # Create plot
  plot <- ggplot(data = effects_length_lockdown,
                 aes(x = Exposure, y = Effect, 
                     color = Adjusted, shape = Adjusted)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          ggh4x.facet.nestline = element_line(color = "gray20", size = 0.2)) +
    geom_hline(yintercept = 0, color = "gray40", lty = "dashed") +
    labs(title = "Effect on length of lockdown",
         color = "", shape = "") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, width = 0.2) +
    facet_nested(. ~ Exposure + Leverage_points,
                 scale = "free", nest_line = TRUE,
                 labeller = labeller(Exposure = exposure_labels,
                                     Leverage_points = leverage_labels)) +
    scale_color_manual(values = effect_aes$Color,
                       breaks = effect_aes$Effect) +
    scale_shape_manual(values = effect_aes$Shape,
                       breaks = effect_aes$Effect) +
    scale_x_discrete(labels = threshold_labels) +
    scale_y_continuous(limits = 1.05*c(-max_y, max_y))
  
  # Return plot
  return(plot)
  
}

# Function to create figure of between-country effect of cases at lockdown 
# on growth factor under lockdown
# Arguments:
# (1) effects = dataframe containing estimated effects
Plot_Between_Growth_Factor <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome
  effects_growth_factor <- effects %>% filter(Outcome == "Median_growth_factor_lockdown")
  
  # Calculate maximum deviation from zero across all confidence intervals
  max_y <- effects_growth_factor %>% pull(CI_lower, CI_upper) %>% abs %>% max 
  
  # Create plot
  plot <- ggplot(data = effects_growth_factor,
                 aes(x = Exposure, y = Effect, 
                     color = Adjusted, shape = Adjusted)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          ggh4x.facet.nestline = element_line(color = "gray20", size = 0.2)) +
    geom_hline(yintercept = 0, color = "gray40", lty = "dashed") +
    labs(title = "Effect on growth factor under lockdown",
         color = "", shape = "") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, width = 0.2) +
    facet_nested(. ~ Exposure + Leverage_points,
                 scale = "free", nest_line = TRUE,
                 labeller = labeller(Exposure = exposure_labels,
                                     Leverage_points = leverage_labels)) +
    scale_x_discrete(labels = exposure_labels) +
    scale_y_continuous(limits = 1.05*c(-max_y, max_y),
                       labels = comma) +
    scale_color_manual(values = effect_aes$Color,
                       breaks = effect_aes$Effect) +
    scale_shape_manual(values = effect_aes$Shape,
                       breaks = effect_aes$Effect)
  
  # Return plot
  return(plot)
  
}

### Figures --------------------------------------------------------------------

# Create figures
figure_between_country_effects <- Plot_Between_Country_Effects(exposures = c("Daily_cases_MA7",
                                                                             "Cumulative_cases_beg_MA7"),
                                                               plots = c("plot_length_lockdown",
                                                                         "plot_growth_factor"),
                                                               out = results_directory)

## (2) Within-country effects --------------------------------------------------

### Functions ------------------------------------------------------------------

# Function to create two-panel figure of within-country effects of lockdown timing on
# percentage change in length of lockdown and percentage change in total cases
# (compared to the natural history), for a specified set of simulations
# Arguments:
# (1) simulations = vector of simulations to include
# (2) plots = plots to display in combination
# (3) out = folder to save combined figure
# Returns: list of 4 figures - each individual, and specified combination
Plot_Within_Country_Effects <- function(simulations, 
                                        plots = c("plot_time_to_thresholds",
                                                  "plot_length_lockdown",
                                                  "plot_total_cases"),
                                        out) {
  
  # Record number of specified simulations and plots
  n_sim <- length(simulations)
  n_plots <- length(plots)
  
  # Filter within-country effects dataframe by specified simulations
  effects_within_countries_counterfactual <- effects_within_countries %>%
    filter(Simulation %in% simulations)
  
  # Plot length of lockdown
  plot_length_lockdown <- Plot_Within_Length_Lockdown(effects = effects_within_countries_counterfactual)
  
  # Plot time to thresholds
  plot_time_to_thresholds <- Plot_Within_Time_To_Thresholds(effects = effects_within_countries_counterfactual)
  
  # Plot total cases
  plot_total_cases <- Plot_Within_Total_Cases(effects = effects_within_countries_counterfactual)
  
  # Create list of specified plots
  plot_list <- map(.x = plots, .f = ~eval(parse(text = .x)))
  
  # Combine figures of within-country effects in double panel, annotate 
  plots_all <- ggarrange(plotlist = plot_list,
                         align = "hv", ncol = n_plots)
  plots_all_annotated <- annotate_figure(plots_all,
                                         top = text_grob("Estimated within-country effects of lockdown timing", 
                                                         size = 20))
  
  # Save combined plot to Results folder
  ggsave(paste0(out, "Figure - Effects within countries.png"), 
         plot = plots_all_annotated, width = 1.5*n_sim*n_plots, height = 7)
  
  # Return list of individual and combined plots
  return(list(plot_time_to_thresholds = plot_time_to_thresholds,
              plot_length_lockdown = plot_length_lockdown, 
              plot_total_cases = plot_total_cases,
              plot_combined = plots_all_annotated))
  
}

# Function to create figure of within-country effect of lockdown timing on length of lockdown
# Arguments:
# (1) effects = dataframe containing estimated effects
Plot_Within_Length_Lockdown <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome
  effects_length_lockdown <- effects %>% 
    filter(Outcome == "Length of lockdown") 
  
  # Create plot
  plot <- ggplot(data = effects_length_lockdown,
                 aes(x = Threshold, y = Pct_change,
                     color = Simulation)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20")) +
    guides(color = FALSE) +
    geom_hline(yintercept = 0, color = "gray20", lty = "dashed") +
    labs(title = "Effect on length of lockdown",
         y = "Percentage change compared to natural history (0,0)") +
    geom_point(shape = 16, alpha = 0.6) +
    stat_summary(fun = median, shape = 18, size = 1.5) +
    facet_grid(. ~ History + Simulation,
               scale = "free") +
    scale_color_manual(values = simulation_aes$Color, 
                       breaks = simulation_aes$Simulation) +
    scale_x_discrete(labels = threshold_labels) +
    scale_y_continuous(limits = c(NA, 0))
  
  # Return plot
  return(plot)
  
}

# Function to create figure of within-country effect of cases at lockdown
# on time to reach important thresholds
# Arguments:
# (1) effects = dataframe containing estimated effects
Plot_Within_Time_To_Thresholds <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome
  effects_time_to_threshold <- effects %>% 
    filter(Outcome == "Time to threshold") 
  
  # Create plot
  plot <- ggplot(data = effects_time_to_threshold,
                 aes(x = Threshold, y = Pct_change,
                     color = Simulation)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20")) +
    guides(color = FALSE) +
    geom_hline(yintercept = 0, color = "gray20", lty = "dashed") +
    labs(title = "Effect on time to reach thresholds",
         y = "Percentage change compared to natural history (0,0)") +
    geom_point(shape = 16, alpha = 0.6) +
    stat_summary(fun = median, shape = 18, size = 1.5) +
    facet_grid(. ~ History + Simulation) +
    scale_color_manual(values = simulation_aes$Color, 
                       breaks = simulation_aes$Simulation) +
    scale_x_discrete(labels = threshold_labels) +
    scale_y_continuous(limits = c(NA, 0))
  
  # Return plot
  return(plot)
  
}

# Function to create figure of within-country effect of lockdown timing on total cases
# Arguments:
# (1) effects = dataframe containing estimated effects
Plot_Within_Total_Cases <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome
  effects_total_cases <- effects %>% 
    filter(Outcome == "Total cases") 
  
  # Create plot
  plot <- ggplot(data = effects_total_cases,
                 aes(x = interaction(History, Simulation), 
                     y = Pct_change,
                     color = Simulation)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20")) +
    guides(color = FALSE) +
    geom_hline(yintercept = 0, color = "gray20", lty = "dashed") +
    labs(title = "Effect on total cases",
         y = "Percentage change compared to natural history (0,0)") +
    geom_point(shape = 16, alpha = 0.6) +
    stat_summary(fun = median, shape = 18, size = 1.5) +
    facet_grid(. ~ History + Simulation,
               scale = "free") +
    scale_color_manual(values = simulation_aes$Color, 
                       breaks = simulation_aes$Simulation) +
    scale_y_continuous(limits = c(NA, 0))
  
  # Return plot
  return(plot)
  
}

### Figures --------------------------------------------------------------------

# Specify simulations to include in figures (comparison is natural history 0,0)
#simulations <- c("7,7", "14,14")
simulations <- c("0,1", "0,3", "0,5", "0,7", "7,7", "14,14")

# Create figures
figure_within_country_effects <- Plot_Within_Country_Effects(simulations = simulations,
                                                             plots = c("plot_length_lockdown",
                                                                       "plot_total_cases"),
                                                             out = results_directory)

