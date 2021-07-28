# DEFINE AESTHETICS ------------------------------------------------------------

## Date aesthetics -------------------------------------------------------------

# Date levels (defines ordering)
date_levels <- c("Date_1", "Date_first_restriction", "Date_restrictions_eased", 
                 "Date_lockdown", "Date_lockdown_eased", "Date_lockdown_end",
                 "Date_start", "Date_T")

# Create color, shape, size, and label key for dates
color_brewer <- colorRampPalette(brewer.pal(n = 10, name = "Paired"))
date_aes <- tibble(Date = date_levels,
                   Label = c("first confirmed case", "first restriction",
                             "restriction easing", "lockdown", "lockdown easing",
                             "end of lockdown", "beginning of included data",
                             "end of first wave"),
                   Color = c("grey50", color_brewer(length(date_levels) - 1)),
                   Shape = c("\u25CB", "\u25CF", "\u25AC", "\u25A0", "\u25BC",
                             "\u25BD", "\u25D6", "\u25D7"),
                   Size = c(4, 4, 4, 4, 3, 3, 4, 4))

## Simulation aesthetics -------------------------------------------------------

# Simulation levels (defines ordering)
simulation_levels <- c("Natural history",
                       "Earliest possible lockdown",
                       "Earlier intervention sequence (7,7)",
                       "Earlier first restriction (7) and earliest possible lockdown")
#simulation_levels <- c("0,0", "0,1", "0,3", "0,5", "0,7", 
#                       "1,1", "3,3", "5,5", "7,7")  

# Create function which labels simulation levels: 
# removes text in parentheses for clarity
Simulation_Labeller <- function(simulation_level) {
  simulation_level %>%
    gsub(pattern = "\\s*\\([^\\)]+\\)", replacement = "", .)
}

# Create color and label key for simulations
color_brewer <- colorRampPalette(brewer.pal(n = 7, name = "Dark2"))
simulation_aes <- tibble(Simulation = simulation_levels,
                         Color = color_brewer(length(simulation_levels)))
simulation_aes <- simulation_aes %>% 
  mutate(Label = Simulation_Labeller(Simulation)) %>%
  relocate(Label, .after = Simulation)

## History aesthetics ----------------------------------------------------------

# History levels (defines ordering)
history_levels <- c("Natural history", "Counterfactual history")

## Threshold aesthetics --------------------------------------------------------

# Threshold levels (defines ordering)
threshold_levels <- c("1 case per 100,000", "1 case per 20,000", "1 case per 10,000",
                      "Lockdown eased")

# Create function which labels threshold levels
# (e.g. if level is "1 case per 100,000", label is "1 case per\n100,000")
Threshold_Labeller <- function(threshold_level) {
  threshold_level %>%
    gsub(pattern = "per ", replacement = "per\n") %>%
    gsub(pattern = "Lockdown ", replacement = "Lockdown\n")
}

# Create label, shape, and transparency key for threshold levels
threshold_aes <- tibble(Threshold = threshold_levels,
                        Label = Threshold_Labeller(Threshold),
                        Shape = c(15, 16, 17, 18),
                        Alpha = c(0.4, 0.7, 1, 0.5))

## Model fit criteria aesthetics -----------------------------------------------

# Model fit criteria levels (defines ordering)
model_fit_levels <- c("Pois_dev_inc", "Pois_dev_cum", "Diff_total_cases", "Diff_time_to_threshold")

# Create key for model fit labels
model_fit_labels <- c("Diff_time_to_threshold" = "Difference in days\nto reach thresholds",
                      "Diff_total_cases" = "Difference in\ntotal cases",
                      "Pois_dev_inc" = "Poisson deviance\n(incident cases)",
                      "Pois_dev_cum" = "Poisson deviance\n(cumulative cases)")

# Create labels for model fit data type
model_fit_type_labels <- c("Number" = "Raw value",
                           "Pct" = "Percentage difference\ncompared to\nobserved data")

## Exposure aesthetics ---------------------------------------------------------

# Exposure levels (defines ordering)
exposure_levels <- c("Daily_cases_MA7", "log(Daily_cases_MA7)",
                     "Cumulative_cases_beg", "log(Cumulative_cases_beg)")

# Create key for exposure labels
exposure_labels <- c("Daily_cases_MA7" = "Daily cases\n(7-day moving average)",
                     "log(Daily_cases_MA7)" = "Daily cases\n(7-day moving average),\nlogged",
                     "Cumulative_cases_beg" = "Cumulative cases", 
                     "log(Cumulative_cases_beg)" = "Cumulative cases,\nlogged")

## Analysis aesthetics ---------------------------------------------------------

# Analysis levels (defines ordering)
analysis_levels <- c("Unadjusted", "Primary", "Secondary")

# Create colour and shape kep for analysis
color_brewer <- colorRampPalette(brewer.pal(n = 8, name = "Dark2"))
analysis_aes <- tibble(Analysis = analysis_levels,
                       Color = color_brewer(length(analysis_levels)),
                       Shape = c(15, 16, 17))

## Leverage aesthetics ---------------------------------------------------------

# Leverage levels (defines ordering)
leverage_levels <- c("Included", "Excluded")

# Create key for leverage lables
leverage_labels <- c("Included" = "All data points\nincluded",
                     "Excluded" = "Points of high\nleverage excluded")
