# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates various figures related to COVID-19:
# (1) Important dates by country (including dates of first restrictions and lockdowns)

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(readr); library(dplyr); library(ggplot2); library(ggrepel); library(scales)

# Run source code to import, format, and summarise data
source("./Code/Import, format, and summarise data.R")

# Import files containing best knots
knots_best <- read_csv(paste0(out, "Best knot points - all.csv")) %>% group_by(Country)
knots_best_final <- read_csv(paste0(out, "Best knot points - final.csv")) %>% group_by(Country)

# Set storage directory for outputs
out <- paste0("./Results/")

# ------------------------------------------------------------------------------
# Important dates
# ------------------------------------------------------------------------------

# Order countries by date of first restriction
countries_ordered <- summary_eur_final %>% arrange(Date_first_restriction) %>% 
  pull(Country) %>% as.character

## Dates of first restriction and lockdown -------------------------------------

# Get min and max dates from summary table
date_min <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = min(Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull
date_max <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = max(Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull

# Define colours and shapes (for first restriction and lockdown)
cols <- c("col1" = "navyblue", "col2" = "darkorange")
shapes <- c("sh1" = 15, "sh2" = 18)

plot_1 <- ggplot(data = summary_eur_final, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Date when: first restriction imposed, lockdown imposed",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_first_restriction, color = "col1", shape = "sh1"), size = 3) +
  geom_point(aes(x = Date_lockdown, color = "col2", shape = "sh2"), size = 3) +
  scale_color_manual(name = "Date of...",
                     breaks = c("col1", "col2"),
                     values = cols,
                     labels = c("first restriction", "lockdown")) +
  scale_shape_manual(name = "Date of...",
                     breaks = c("sh1", "sh2"),
                     values = shapes,
                     labels = c("first restriction", "lockdown")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 4, date_max + 4), 
               date_breaks = "2 days", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_1

# Save plot
ggsave(paste0(out, "Figure - Important dates.png"), plot = plot_1, width = 9, height = 8)

## Dates of first restriction, lockdown, first case, and cases >= 100 ----------

# Get min and max dates
date_min <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = min(Date_0, Date_100, Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull
date_max <- summary_eur_final %>% ungroup %>% 
  summarise(Date_min = max(Date_0, Date_100, Date_first_restriction, Date_lockdown, na.rm = TRUE)) %>% pull

# Define colours and shapes 
# (for first restriction, lockdown, first case, and cases >= 100)
cols <- c("col1" = "navyblue", "col2" = "darkorange", "col3" = "grey80", "col4" = "grey50")
shapes <- c("sh1" = 15, "sh2" = 18, "sh3" = 1, "sh4" = 1)

plot_2 <- ggplot(data = summary_eur_final, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Date when: first restriction imposed, lockdown imposed, first case recorded, 100th case recorded",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)
       and Johns Hopkins COVID-19 Data Repository (https://github.com/CSSEGISandData/COVID-19).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_first_restriction, color = "col1", shape = "sh1"), size = 3) +
  geom_point(aes(x = Date_lockdown, color = "col2", shape = "sh2"), size = 3) +
  geom_point(aes(x = Date_0, color = "col3", shape = "sh3"), size = 3) +
  geom_point(aes(x = Date_100 - 1, color = "col4", shape = "sh4"), size = 3) +
  scale_color_manual(name = "Date of...",
                     breaks = c("col1", "col2", "col3", "col4"),
                     values = cols,
                     labels = c("first restriction", "lockdown", "first confirmed case", "100th confirmed case")) +
  scale_shape_manual(name = "Date of...",
                     breaks = c("sh1", "sh2", "sh3", "sh4"),
                     values = shapes,
                     labels = c("first restriction", "lockdown", "first confirmed case", "100th confirmed case")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 4, date_max + 4), 
               date_breaks = "2 days", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_2

# Save plot
ggsave(paste0(out, "Figure - Important dates (including first and 100th cases).png"),
       plot = plot_2, width = 12, height = 8)

# Remove plotting objects from environment
rm(countries_ordered, date_min, date_max, cols, shapes, plot_1, plot_2)

# ------------------------------------------------------------------------------
# Exponential growth 
# ------------------------------------------------------------------------------

## Each country on separate grid -----------------------------------------------

### Normal scale ---------------------------------------------------------------

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
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
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

### Log scale ------------------------------------------------------------------

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
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1),
                       trans = log10_trans()) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
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

## All countries on same grid --------------------------------------------------

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
  scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                     labels = comma_format(accuracy = 1)) + 
  scale_y_continuous(name = "New daily number of COVID-19 cases",
                     limits = c(NA, 30000),
                     labels = comma_format(accuracy = 1))
#plot_exp_growth_cases

# Save plot
ggsave(paste0(out, "Figure - Cumulative vs incident cases - combined.png"),
       plot = plot_exp_growth_cases, width = 12, height = 8)

# Remove plotting objects from environment
rm(plot_exp_growth_cases, i, country, 
   knots_best_final, data_eur_final_i, 
   date_first_restriction, date_lockdown, date_100)

# ------------------------------------------------------------------------------
# Exponential growth (with fitted splines)
# ------------------------------------------------------------------------------

## Best knots by min RMSE incidence --------------------------------------------

# Filter best knots dataframe by min RMSE incidence
knots_best_final_inc <- knots_best_final %>% filter(Criteria == "min RMSE_inc")

# Create list for plots
plot_exp_growth_cases_inc <- list()

# Create plots
for (i in countries_eur_final) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, best knots, and summary dataframes by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  data_eur_final_100_i <- data_eur_final_i %>% filter(Date >= Date_100 & Date <= date_T)
  knots_best_final_inc_i <- knots_best_final_inc %>% filter(Country == country)
  summary_eur_final_i <- summary_eur_final %>% filter(Country == country)
  
  # Define dates of first restriction, lockdown, when cases first exceeded 100
  date_first_restriction <- summary_eur_final_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_final_i %>% pull(Date_lockdown)
  date_100 <- summary_eur_final_i %>% pull(Date_100)
  
  # Define knot dates
  knot_date_1 <- knots_best_final_inc_i %>% pull(Knot_date_1) %>% head(1)
  knot_date_2 <- knots_best_final_inc_i %>% pull(Knot_date_2) %>% head(1)
  
  # Define values of cumulative cases at knot dates
  knot_1 <- data_eur_final_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
  knot_2 <- data_eur_final_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
  
  # Calculate min and max values of cumulative cases in modelling period
  x_min <- min(data_eur_final_100_i$Cumulative_cases_beg)
  x_max <- max(data_eur_final_100_i$Cumulative_cases_beg)
  
  # Define Arima spline parameters
  slope_1 <- knots_best_final_inc_i %>% pull(Growth_factor_1) %>% head(1) - 1
  slope_2 <- knots_best_final_inc_i %>% pull(Growth_factor_2) %>% head(1) - 1
  slope_3 <- knots_best_final_inc_i %>% pull(Growth_factor_3) %>% head(1) - 1
  intercept_1 <- knots_best_final_inc_i %>% pull(Intercept_1) %>% head(1)
  intercept_2 <- knots_best_final_inc_i %>% pull(Intercept_2) %>% head(1)
  intercept_3 <- knots_best_final_inc_i %>% pull(Intercept_3) %>% head(1)
  
  # Create base plot
  p <- ggplot(data = filter(data_eur_final_100_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_path(data = filter(data_eur_final_i, Date <= Date_100),
              aes(x = Cumulative_cases_beg, y = Daily_cases),
              linetype = "dashed") +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_final_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 4) +
    geom_point(data = filter(data_eur_final_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 1, size = 4) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       labels = comma_format(accuracy = 1))
  
  # Add fitted spline segments to base plot
  if (knot_date_1 == date_100) {
    if (is.na(knot_date_2)) {  # NO knot points
      p <- p +
        geom_segment(aes_(x = x_min, xend = x_max,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3")
    } else {  # ONE knot point (at knot_date_2)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_2,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_2),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_2, xend = x_max,
                          y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_2), 
                        label = paste0(as.character(knot_date_2, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    }
  } else {
    if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_1,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_1, xend = x_max,
                          y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    } else {  # TWO knot points (at knot_date_1 and knot_date_2)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_1,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_1, xend = knot_2,
                          y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_2, xend = x_max,
                          y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_2), 
                        label = paste0(as.character(knot_date_2, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    }
  }
  
  # Add plot to list
  plot_exp_growth_cases_inc[[i]] <- p
  
}

# Save plots
dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases_inc, nrow = 6, ncol = 6)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 16))
ggsave(paste0(out, "Figure - Cumulative vs incident cases (with fitted splines by min RMSE_inc).png"),
       plot = g, width = 6*6, height = 6*6, limitsize = FALSE)
dev.off()

## Best knots by min RMSE cumulative -------------------------------------------

# Filter best knots dataframe by min RMSE cumulative
knots_best_final_cum <- knots_best_final %>% filter(Criteria == "min RMSE_cum")

# Create list for plots
plot_exp_growth_cases_cum <- list()

# Create plots
for (i in countries_eur_final) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, best knots, and summary dataframes by country
  data_eur_final_i <- data_eur_final %>% filter(Country == country)
  data_eur_final_100_i <- data_eur_final_i %>% filter(Date >= Date_100 & Date <= date_T)
  knots_best_final_cum_i <- knots_best_final_cum %>% filter(Country == country)
  summary_eur_final_i <- summary_eur_final %>% filter(Country == country)
  
  # Define dates of first restriction, lockdown, when cases first exceeded 100
  date_first_restriction <- summary_eur_final_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_final_i %>% pull(Date_lockdown)
  date_100 <- summary_eur_final_i %>% pull(Date_100)
  
  # Define knot dates
  knot_date_1 <- knots_best_final_cum_i %>% pull(Knot_date_1) %>% head(1)
  knot_date_2 <- knots_best_final_cum_i %>% pull(Knot_date_2) %>% head(1)
  
  # Define values of cumulative cases at knot dates
  knot_1 <- data_eur_final_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
  knot_2 <- data_eur_final_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
  
  # Calculate min and max values of cumulative cases in modelling period
  x_min <- min(data_eur_final_100_i$Cumulative_cases_beg)
  x_max <- max(data_eur_final_100_i$Cumulative_cases_beg)
  
  # Define Arima spline parameters
  slope_1 <- knots_best_final_cum_i %>% pull(Growth_factor_1) %>% head(1) - 1
  slope_2 <- knots_best_final_cum_i %>% pull(Growth_factor_2) %>% head(1) - 1
  slope_3 <- knots_best_final_cum_i %>% pull(Growth_factor_3) %>% head(1) - 1
  intercept_1 <- knots_best_final_cum_i %>% pull(Intercept_1) %>% head(1)
  intercept_2 <- knots_best_final_cum_i %>% pull(Intercept_2) %>% head(1)
  intercept_3 <- knots_best_final_cum_i %>% pull(Intercept_3) %>% head(1)
  
  # Create base plot
  p <- ggplot(data = filter(data_eur_final_100_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_path(data = filter(data_eur_final_i, Date <= Date_100),
              aes(x = Cumulative_cases_beg, y = Daily_cases),
              linetype = "dashed") +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_final_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 4) +
    geom_point(data = filter(data_eur_final_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_final_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 1, size = 4) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       labels = comma_format(accuracy = 1))
  
  # Add fitted spline segments to base plot
  if (knot_date_1 == date_100) {
    if (is.na(knot_date_2)) {  # NO knot points
      p <- p +
        geom_segment(aes_(x = x_min, xend = x_max,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3")
    } else {  # ONE knot point (at knot_date_2)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_2,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_2),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_2, xend = x_max,
                          y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_2), 
                        label = paste0(as.character(knot_date_2, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    }
  } else {
    if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_1,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_1, xend = x_max,
                          y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    } else {  # TWO knot points (at knot_date_1 and knot_date_2)
      p <- p +
        geom_segment(aes_(x = x_min, xend = knot_1,
                          y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_1, xend = knot_2,
                          y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                     color = "deeppink3") +
        geom_segment(aes_(x = knot_2, xend = x_max,
                          y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*x_max),
                     color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_1), 
                        label = paste0(as.character(knot_date_1, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") +
        geom_text_repel(data = filter(data_eur_final_i, Date == knot_date_2), 
                        label = paste0(as.character(knot_date_2, format = "%d %b")),
                        vjust = 0, size = 4, color = "deeppink3") 
    }
  }
  
  # Add plot to list
  plot_exp_growth_cases_cum[[i]] <- p
  
}

# Save plots
dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases_cum, nrow = 6, ncol = 6)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 16))
ggsave(paste0(out, "Figure - Cumulative vs incident cases (with fitted splines by min RMSE_cum).png"),
       plot = g, width = 6*6, height = 6*6, limitsize = FALSE)
dev.off()

# Remove plotting objects from environment
rm(plot_exp_growth_cases_inc, plot_exp_growth_cases_cum, i, country, 
   knots_best_final_inc, knots_best_final_cum, data_eur_final_i, data_eur_final_100_i, 
   knots_best_final_inc_i, knots_best_final_cum_i, summary_eur_final_i, 
   date_first_restriction, date_lockdown, date_100, knot_date_1, knot_date_2,
   knot_1, knot_2, x_min, x_max, slope_1, slope_2, slope_3,
   intercept_1, intercept_2, intercept_3, p, g)

# ------------------------------------------------------------------------------
# Growth factor under lockdown -------------------------------------------------
# ------------------------------------------------------------------------------

# (What to do with countries who don't have lockdowns??)

# Calculate median growth factor under lockdown from list of best knots
median_growth_factor_final <- knots_best %>% select(Country, contains("Median")) %>% 
  unique %>% summarise(Median_growth_factor_final = ifelse(!is.na(Median_growth_factor_3),
                                                           Median_growth_factor_3, Median_growth_factor_2),
                       .groups = "keep")

# Bind median growth factors to summary_eur_final
summary_eur_final <- full_join(summary_eur_final, median_growth_factor_final)

# Plot relationship between cases on date of lockdown/sd and median growth factor under lockdown
plot_growth_factor_1 <- ggplot(data = summary_eur_final, 
       aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_final)) +
  theme_classic() +
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.position = "none",
        panel.grid.major = element_line(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(size = 9)) +
  labs(title = "Relationship between cumulative number of COVID-19 cases at the date of lockdown \nand growth factor under lockdown") +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_text_repel(aes(label = Country), size = 2) +
  scale_x_continuous(name = "Cumulative number of COVID-19 cases at date of lockdown",
                     labels = comma_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth factor under lockdown") 
#plot_growth_factor_1

# Plot without UK (outlier)
plot_growth_factor_2 <- ggplot(data = filter(summary_eur_final, Country != "United Kingdom"), 
                               aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_final)) +
  theme_classic() +
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.position = "none",
        panel.grid.major = element_line(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(size = 9)) +
  labs(title = "Relationship between cumulative number of COVID-19 cases at the date of lockdown \nand growth factor under lockdown") +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  #geom_smooth(data = filter(summary_eur_final, Country != "United Kingdom" & !is.na(Date_lockdown)),
  #            method = "glm", method.args = list(family = gaussian(link = "log")), se = FALSE,
  #            aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_final)) +
  geom_text_repel(aes(label = Country), size = 2) +
  scale_x_continuous(name = "Cumulative number of COVID-19 cases at date of lockdown",
                     labels = comma_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth factor under lockdown")
#plot_growth_factor_2

# Save plots
ggsave(paste0(out, "Figure - Cases at lockdown vs growth factor.png"),
       plot = plot_growth_factor_1, width = 6, height = 6)
ggsave(paste0(out, "Figure - Cases at lockdown vs growth factor - without UK.png"),
       plot = plot_growth_factor_2, width = 6, height = 6)

# Remove plotting objects from environment
rm(plot_growth_factor_1, plot_growth_factor_2)

# ------------------------------------------------------------------------------
# Incident and cumulative cases ------------------------------------------------
# ------------------------------------------------------------------------------









# 3-panel plot for each country with incidence, cumulative cases, and cumulative vs incidence (w/ fitted spline)


