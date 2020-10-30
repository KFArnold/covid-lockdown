# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates various figures related to COVID-19:
# (1) Important dates by country (including dates of first restrictions and lockdowns)

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(readr); library(dplyr); library(ggplot2); 
library(ggrepel); library(scales); library(ggpubr)

# Run source code to import, format, and summarise data
source("./Code/Import, format, and summarise data.R")

# Import file containing best knot date pairs
knots_best <- read_csv(paste0(out, "Best knot points.csv")) %>% group_by(Country)

# Import files containing simulation results
summary_daily_cases_sim <- read_csv(paste0(out, "Simulation summary - daily cases.csv"))
summary_cumulative_cases_end_sim <- read_csv(paste0(out, "Simulation summary - cumulative cases.csv"))
summary_thresholds <- read_csv(paste0(out, "Simulation summary - thresholds.csv"))

# Set storage directory for outputs
out <- paste0("./Results/")

# ------------------------------------------------------------------------------
# Important dates
# ------------------------------------------------------------------------------

# Order countries by date of first restriction
countries_ordered <- summary_eur %>% arrange(Date_first_restriction) %>% 
  pull(Country) %>% as.character

## Dates of first restriction and lockdown -------------------------------------

# Get min and max dates from summary table
date_min <- summary_eur %>% ungroup %>% 
  summarise(Date_min = min(Date_first_restriction, Date_lockdown, 
                           Date_lockdown_eased, Date_lockdown_end, na.rm = TRUE)) %>% pull
date_max <- summary_eur %>% ungroup %>% 
  summarise(Date_max = max(Date_first_restriction, Date_lockdown, 
                           Date_lockdown_eased, Date_lockdown_end, na.rm = TRUE)) %>% pull

# Define colours and shapes (for first restriction and lockdown)
# (unicode shapes: https://jrgraphix.net/r/Unicode/25A0-25FF)
cols <- c("col1" = "navyblue", "col2" = "darkorange", "col3" = "firebrick", "col4" = "forestgreen")
shapes <- c("sh1" = "\u25A0", "sh2" = "\u25CF", "sh3" = "\u25BC", "sh4" = "\u25B2")
sizes <- c(4, 4, 3, 3)

plot_1 <- ggplot(data = summary_eur, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Dates when: first restriction imposed, lockdown imposed, lockdown eased, and lockdown lifted",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_first_restriction, color = "col1", shape = "sh1"), size = sizes[1]) +
  geom_point(aes(x = Date_lockdown, color = "col2", shape = "sh2"), size = sizes[2]) +
  geom_point(aes(x = Date_lockdown_eased, color = "col3", shape = "sh3"), size = sizes[3]) +
  geom_point(aes(x = Date_lockdown_end, color = "col4", shape = "sh4"), size = sizes[4]) +
  scale_color_manual(name = "Date:",
                     breaks = c("col1", "col2", "col3", "col4"),
                     values = cols,
                     labels = c("first restriction imposed", "lockdown imposed", "lockdown eased", "lockdown lifted")) +
  scale_shape_manual(name = "Date:",
                     breaks = c("sh1", "sh2", "sh3", "sh4"),
                     values = shapes,
                     labels = c("first restriction imposed", "lockdown imposed", "lockdown eased", "lockdown lifted")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 7, date_max + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_1

# Save plot
ggsave(paste0(out, "Figure - Important dates.png"), plot = plot_1, width = 12, height = 8)

## Dates of first restriction, lockdown, first case, and cases >= 50 ----------

# Get min and max dates
date_min <- summary_eur %>% ungroup %>% 
  summarise(Date_min = min(Date_0, Date_50, Date_first_restriction, Date_lockdown, 
                           Date_lockdown_eased, Date_lockdown_end, na.rm = TRUE)) %>% pull
date_max <- summary_eur %>% ungroup %>% 
  summarise(Date_max = max(Date_0, Date_50, Date_first_restriction, Date_lockdown, 
                           Date_lockdown_eased, Date_lockdown_end, na.rm = TRUE)) %>% pull

# Define colours and shapes (for first restriction, lockdown, first case, and cases >= 50)
# (unicode shapes: https://jrgraphix.net/r/Unicode/25A0-25FF)
cols <- c("col1" = "grey80", "col2" = "grey50", "col3" = "navyblue", "col4" = "darkorange", "col5" = "firebrick", "col6" = "forestgreen")
shapes <- c("sh1" = "\u25CB", "sh2" = "\u25CB", "sh3" = "\u25A0", "sh4" = "\u25CF", "sh5" = "\u25BC", "sh6" = "\u25B2")
sizes <- c(4, 4, 4, 4, 3, 3)

plot_2 <- ggplot(data = summary_eur, aes(y = Country)) + 
  theme_minimal() +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Important dates in COVID-19 European policy responses",
       subtitle = "Dates when: first case recorded, 50th case recorded, first restriction imposed, lockdown imposed, lockdown eased, and lockdown lifted",
       caption = "Data from Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker)
       and Johns Hopkins COVID-19 Data Repository (https://github.com/CSSEGISandData/COVID-19).") +
  theme(plot.caption = element_text(size = 7),
        plot.subtitle = element_text(size = 10)) +
  geom_point(aes(x = Date_0, color = "col1", shape = "sh1"), size = sizes[1]) +
  geom_point(aes(x = Date_50 - 1, color = "col2", shape = "sh2"), size = sizes[2]) +
  geom_point(aes(x = Date_first_restriction, color = "col3", shape = "sh3"), size = sizes[3]) +
  geom_point(aes(x = Date_lockdown, color = "col4", shape = "sh4"), size = sizes[4]) +
  geom_point(aes(x = Date_lockdown_eased, color = "col5", shape = "sh5"), size = sizes[5]) +
  geom_point(aes(x = Date_lockdown_end, color = "col6", shape = "sh6"), size = sizes[6]) +
  scale_color_manual(name = "Date:",
                     breaks = c("col1", "col2", "col3", "col4", "col5", "col6"),
                     values = cols,
                     labels = c("first confirmed case", "50th confirmed case", 
                                "first restriction imposed", "lockdown imposed", 
                                "lockdown eased", "lockdown lifted")) +
  scale_shape_manual(name = "Date:",
                     breaks = c("sh1", "sh2", "sh3", "sh4", "sh5", "sh6"),
                     values = shapes,
                     labels = c("first confirmed case", "50th confirmed case", 
                                "first restriction imposed", "lockdown imposed", 
                                "lockdown eased", "lockdown lifted")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  scale_x_date(name = "", 
               limits = c(date_min - 7, date_max + 7), 
               date_breaks = "1 week", 
               date_labels = "%d %b %C",
               expand = expansion(mult = c(0, 0))) +
  scale_y_discrete(name = "",
                   limits = rev(countries_ordered)) 
#plot_2

# Save plot
ggsave(paste0(out, "Figure - Important dates (including first and 50th cases).png"),
       plot = plot_2, width = 12, height = 8)

# Remove plotting objects from environment
rm(countries_ordered, date_min, date_max, cols, shapes, plot_1, plot_2)

# ------------------------------------------------------------------------------
# Exponential growth 
# ------------------------------------------------------------------------------

## Each country on separate grid -----------------------------------------------

### Normal scale ---------------------------------------------------------------

plot_exp_growth_cases <- list()

for (i in countries_eur) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, summary dataframes by country
  data_eur_i <- data_eur %>% filter(Country == country)
  summary_eur_i <- summary_eur %>% filter(Country == country)
  
  # Define dates of first restriction, lockdown, and 50 cases
  date_first_restriction <- summary_eur_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)
  date_50 <- summary_eur_i %>% pull(Date_50)
  date_T <- summary_eur_i %>% pull(Date_T)
  
  # Plot
  p <- ggplot(data = filter(data_eur_i, Date <= date_T),
              aes(x = Cumulative_cases_beg, 
                  y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_i, Date == date_50),
               size = 3, color = "grey", shape = 15) +
    geom_text_repel(data = filter(data_eur_i, Date == date_50), color = "grey",
                    label = paste0(as.character(date_50, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = 15) +
    geom_text_repel(data = filter(data_eur_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 3) +
    geom_point(data = filter(data_eur_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = 18) +
    geom_text_repel(data = filter(data_eur_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 0, size = 3) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       labels = comma_format(accuracy = 1))
  
  # Add plot to list
  plot_exp_growth_cases[[i]] <- p
  
}

# Calculate number of rows and columns
rows <- length(plot_exp_growth_cases) %>% sqrt %>% ceiling
cols <- length(plot_exp_growth_cases) %>% sqrt %>% floor

# Save plots
#dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = rows, ncol = cols)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 30))
ggsave(paste0(out, "Figure - Cumulative vs incident cases.png"),
       plot = g, width = 6*cols, height = 6*rows, limitsize = FALSE)
#dev.off()

### Log scale ------------------------------------------------------------------

#plot_exp_growth_cases <- list()
#
#for (i in countries_eur) {
#  
#  # Define country
#  country <- i
#  
#  # Filter cases/deaths, summary dataframes by country
#  data_eur_i <- data_eur %>% filter(Country == country)
#  summary_eur_i <- summary_eur %>% filter(Country == country)
#  
#  # Define dates of first restriction, lockdown, and 50 cases
#  date_first_restriction <- summary_eur_i %>% pull(Date_first_restriction)
#  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)
#  #date_50 <- summary_eur_i %>% pull(Date_50)
#  date_T <- summary_eur_i %>% pull(Date_T)
#  
#  # Plot
#  p <- ggplot(data = filter(data_eur_i, Date <= date_T),
#              aes(x = Cumulative_cases_beg, 
#                  y = Daily_cases)) +
#    theme_minimal() +
#    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
#    labs(title = paste0(country)) +
#    geom_path() +
#    geom_point(size = 1) +
#    geom_point(data = filter(data_eur_i, Date == date_first_restriction),
#               size = 3, color = "navyblue", shape = 15) +
#    geom_text_repel(data = filter(data_eur_i, Date == date_first_restriction), color = "blue",
#                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
#                    hjust = 0, size = 3) +
#    geom_point(data = filter(data_eur_i, Date == date_lockdown), 
#               size = 3, color = "darkorange", shape = 18) +
#    geom_text_repel(data = filter(data_eur_i, Date == date_lockdown), color = "darkorange",
#                    label = paste0(as.character(date_lockdown, format = "%d %b")),
#                    hjust = 0, size = 3) +
#    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
#                       labels = comma_format(accuracy = 1),
#                       trans = log10_trans()) + 
#    scale_y_continuous(name = "New daily number of COVID-19 cases",
#                       labels = comma_format(accuracy = 1),
#                       trans = log10_trans())
#  
#  # Add plot to list
#  plot_exp_growth_cases[[i]] <- p
#  
#}
#
## Calculate number of rows and columns
#rows <- length(plot_exp_growth_cases) %>% sqrt %>% ceiling
#cols <- length(plot_exp_growth_cases) %>% sqrt %>% floor
#
## Save plots
#dev.new()  # make very large to avoid bug with saving
#p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = rows, ncol = cols)
#g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 30))
#ggsave(paste0(out, "Figure - Cumulative vs incident cases (log scale).png"),
#       plot = g, width = 6*cols, height = 6*rows, limitsize = FALSE)
#dev.off()

# Remove plotting objects from environment
rm(i, country, data_eur_i, summary_eur_i,
   date_first_restriction, date_lockdown, date_50, date_T,
   rows, cols, p, g)

# ------------------------------------------------------------------------------
# Exponential growth (with fitted splines)
# ------------------------------------------------------------------------------

# Create folder for storing figures of exponential growth with fitted splines, 
# if none already exists
out_folder <- paste0(out, "Figures - cumulative vs incident cases (with fitted splines) by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Create list for plots
plot_exp_growth_cases <- list()

# Create plots
for (i in countries_eur) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, best knots, and summary dataframes by country
  data_eur_i <- data_eur %>% filter(Country == country)
  knots_best_i <- knots_best %>% filter(Country == country)
  summary_eur_i <- summary_eur %>% filter(Country == country)
  
  # Define number of best knot point pairs
  n_knots_i <- nrow(knots_best_i)
  
  # Define dates of first restriction, lockdown, and 50 cases
  date_first_restriction <- summary_eur_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)
  date_50 <- summary_eur_i %>% pull(Date_50)
  date_T <- summary_eur_i %>% pull(Date_T)
  
  # Create copy of cases/deaths dataframe where cumulative cases >= 50 and up to date_T
  data_eur_50_i <- data_eur_i %>% filter(Date >= date_50 & Date <= date_T)
  
  # Create base plot
  p <- ggplot(data = data_eur_50_i,
              aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = paste0(country)) +
    geom_path() +
    geom_path(data = filter(data_eur_i, Date <= date_50),
              aes(x = Cumulative_cases_beg, y = Daily_cases),
              linetype = "dashed") +
    geom_point(size = 1) +
    geom_point(data = filter(data_eur_i, Date == date_first_restriction),
               size = 3, color = "navyblue", shape = "\u25A0") +
    geom_text_repel(data = filter(data_eur_i, Date == date_first_restriction), color = "blue",
                    label = paste0(as.character(date_first_restriction, format = "%d %b")),
                    hjust = 0, size = 4) +
    geom_point(data = filter(data_eur_i, Date == date_lockdown), 
               size = 3, color = "darkorange", shape = "\u25CF") +
    geom_text_repel(data = filter(data_eur_i, Date == date_lockdown), color = "darkorange",
                    label = paste0(as.character(date_lockdown, format = "%d %b")),
                    hjust = 1, size = 4) +
    geom_point(data = filter(data_eur_i, Date == date_T), 
               size = 3, color = "purple", shape = "\u25C4") +
    geom_text_repel(data = filter(data_eur_i, Date == date_T), color = "purple",
                    label = paste0(as.character(date_T, format = "%d %b")),
                    hjust = 1, size = 4) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       labels = comma_format(accuracy = 1))
  
  # Add fitted lines corresponding to best knot dates onto base plot
  for (j in 1:nrow(knots_best_i)) {
    
    # Filter best knots dataset
    knots_best_j <- knots_best_i[j, ]
    
    # Define knot date pair
    knot_date_1 <- knots_best_j %>% pull(Knot_date_1)
    knot_date_2 <- knots_best_j %>% pull(Knot_date_2)
    
    # Define values of cumulative cases at knot dates
    knot_1 <- data_eur_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
    knot_2 <- data_eur_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
    
    # Calculate min and max values of cumulative cases in modelling period
    x_min <- data_eur_50_i %>% pull(Cumulative_cases_beg) %>% min()
    x_max <- data_eur_50_i %>% pull(Cumulative_cases_beg) %>% max()
    
    # Define Arima spline parameters
    slope_1 <- knots_best_j %>% pull(Growth_factor_1) %>% head(1) - 1
    slope_2 <- knots_best_j %>% pull(Growth_factor_2) %>% head(1) - 1
    slope_3 <- knots_best_j %>% pull(Growth_factor_3) %>% head(1) - 1
    intercept_1 <- knots_best_j %>% pull(Intercept_1) %>% head(1)
    intercept_2 <- knots_best_j %>% pull(Intercept_2) %>% head(1)
    intercept_3 <- knots_best_j %>% pull(Intercept_3) %>% head(1)
    
    # Add fitted line
    if (knot_date_1 == date_50) {
      if (is.na(knot_date_2)) {  # NO knot points
        p <- p +
          geom_segment(aes_(x = x_min, xend = x_max,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*x_max),
                       color = "deeppink3", size = 0.25, linetype = "dashed")
      } else {  # ONE knot point (at knot_date_2)
        p <- p +
          geom_segment(aes_(x = x_min, xend = knot_2,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_2),
                       color = "deeppink3", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = x_max,
                            y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*x_max),
                       color = "deeppink3", size = 0.25, linetype = "dashed") 
      }
    } else {
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        p <- p +
          geom_segment(aes_(x = x_min, xend = knot_1,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                       color = "deeppink3", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = x_max,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*x_max),
                       color = "deeppink3", size = 0.25, linetype = "dashed") 
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        p <- p +
          geom_segment(aes_(x = x_min, xend = knot_1,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                       color = "deeppink3", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = knot_2,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                       color = "deeppink3", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = x_max,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*x_max),
                       color = "deeppink3", size = 0.25, linetype = "dashed") 
      }
      
    }  # (close if-else section)
    
  }  # (close fitted line section)
  
  # Add plot to list
  plot_exp_growth_cases[[i]] <- p
  
  # Save plot to subfolder
  ggsave(paste0(out_folder, "/", country, ".png"), plot = p, width = 6, height = 6)
  
}

# Calculate number of rows and columns
rows <- length(plot_exp_growth_cases) %>% sqrt %>% ceiling
cols <- length(plot_exp_growth_cases) %>% sqrt %>% floor

# Save combined plots
#dev.new()  # make very large to avoid bug with saving
p <- ggarrange(plotlist = plot_exp_growth_cases, nrow = rows, ncol = cols)
g <- annotate_figure(p, top = text_grob("Exponential growth of Covid-19 cases: Cumulative versus incident cases", size = 30))
ggsave(paste0(out, "Figure - Cumulative vs incident cases (with fitted splines).png"),
       plot = g, width = 6*cols, height = 6*rows, limitsize = FALSE)
#dev.off()

# Remove plotting objects from environment
rm(i, country, data_eur_i, knots_best_i, summary_eur_i, n_knots_i,
   date_first_restriction, date_lockdown, date_50, date_T, data_eur_50_i,
   j, knots_best_j, knot_date_1, knot_date_2, knot_1, knot_2, x_min, x_max,
   slope_1, slope_2, slope_3, intercept_1, intercept_2, intercept_3,
   rows, cols, p, g)

# ------------------------------------------------------------------------------
# Growth factor under lockdown 
# ------------------------------------------------------------------------------

# (use only subset of European countries that entered lockdown)

# Calculate median growth factor under lockdown from list of best knots
median_growth_factor_lockdown <- knots_best %>% filter(Country %in% countries_eur_lockdown) %>%
  select(Country, contains("Median")) %>% unique %>% 
  summarise(Median_growth_factor_lockdown = ifelse(!is.na(Median_growth_factor_3),
                                                Median_growth_factor_3, Median_growth_factor_2),
            .groups = "keep")

# Bind median growth factors to summary_eur_lockdown
summary_eur_lockdown <- full_join(summary_eur_lockdown, median_growth_factor_lockdown,
                                  by = "Country")
rm(median_growth_factor_lockdown)

## Cumulative cases on date of lockdown ----------------------------------------

# Plot relationship between cumulative cases on date of lockdown and median growth factor under lockdown
plot_growth_factor_1 <- ggplot(data = summary_eur_lockdown, 
       aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_lockdown)) +
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
plot_growth_factor_2 <- ggplot(data = filter(summary_eur_lockdown, Country != "United Kingdom"), 
                               aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_lockdown)) +
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
  #geom_smooth(data = filter(summary_eur_lockdown, Country != "United Kingdom" & !is.na(Date_lockdown)),
  #            method = "glm", method.args = list(family = gaussian(link = "log")), se = FALSE,
  #            aes(x = Cumulative_cases_beg_lockdown, y = Median_growth_factor_lockdown)) +
  geom_text_repel(aes(label = Country), size = 2) +
  scale_x_continuous(name = "Cumulative number of COVID-19 cases at date of lockdown",
                     labels = comma_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth factor under lockdown")
#plot_growth_factor_2

# Save plots
ggsave(paste0(out, "Figure - Cumulative cases at lockdown vs growth factor.png"),
       plot = plot_growth_factor_1, width = 6, height = 6)
ggsave(paste0(out, "Figure - Cumulative cases at lockdown vs growth factor - without UK.png"),
       plot = plot_growth_factor_2, width = 6, height = 6)

## Incident cases on date of lockdown ------------------------------------------

# Plot relationship between daily cases on date of lockdown and median growth factor under lockdown
plot_growth_factor_3 <- ggplot(data = summary_eur_lockdown, 
                               aes(x = Daily_cases_lockdown, y = Median_growth_factor_lockdown)) +
  theme_classic() +
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.position = "none",
        panel.grid.major = element_line(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(size = 9)) +
  labs(title = "Relationship between daily number of COVID-19 cases at the date of lockdown \nand growth factor under lockdown") +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  geom_text_repel(aes(label = Country), size = 2) +
  scale_x_continuous(name = "Daily number of COVID-19 cases at date of lockdown",
                     labels = comma_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth factor under lockdown") 
#plot_growth_factor_3

# Plot without UK (outlier)
plot_growth_factor_4 <- ggplot(data = filter(summary_eur_lockdown, Country != "United Kingdom"), 
                               aes(x = Daily_cases_lockdown, y = Median_growth_factor_lockdown)) +
  theme_classic() +
  theme(axis.text = element_text(size = 6), 
        axis.title = element_text(size = 8),
        legend.position = "none",
        panel.grid.major = element_line(),
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(size = 9)) +
  labs(title = "Relationship between daily number of COVID-19 cases at the date of lockdown \nand growth factor under lockdown") +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE, linetype = "dashed") +
  #geom_smooth(data = filter(summary_eur_lockdown, Country != "United Kingdom" & !is.na(Date_lockdown)),
  #            method = "glm", method.args = list(family = gaussian(link = "log")), se = FALSE,
  #            aes(x = Daily_cases_lockdown, y = Median_growth_factor_lockdown)) +
  geom_text_repel(aes(label = Country), size = 2) +
  scale_x_continuous(name = "Daily number of COVID-19 cases at date of lockdown",
                     labels = comma_format(accuracy = 1)) +
  scale_y_continuous(name = "Growth factor under lockdown")
#plot_growth_factor_4

# Save plots
ggsave(paste0(out, "Figure - Daily cases at lockdown vs growth factor.png"),
       plot = plot_growth_factor_3, width = 6, height = 6)
ggsave(paste0(out, "Figure - Daily cases at lockdown vs growth factor - without UK.png"),
       plot = plot_growth_factor_4, width = 6, height = 6)

# ------------------------------------------------------------------------------
# Incident and cumulative cases 
# ------------------------------------------------------------------------------

# Incident, cumulative, and cumulative vs incident cases up to present, 
# with lines indicating important dates
# (...might be nice of plots were on same scale for each country??)

# Create folder for storing figures of incident and cumulative cases by country, 
# if none already exists
out_folder <- paste0(out, "Figures - incident and cumulative cases by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Create list for plots
plot_coutry_cases <- list()

# Create plots
for (i in countries_eur) {
  
  # Define country
  country <- i
  
  # Filter cases/deaths, best knots, and summary dataframes by country
  data_eur_i <- data_eur %>% filter(Country == country) %>% ungroup()
  summary_eur_i <- summary_eur %>% filter(Country == country)
  
  # Define dates of first restriction, lockdown, lockdown eased, and lockdown lifted
  date_first_restriction <- summary_eur_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_i %>% pull(Date_lockdown)
  date_lockdown_eased <- summary_eur_i %>% pull(Date_lockdown_eased)
  date_lockdown_end <- summary_eur_i %>% pull(Date_lockdown_end)
  date_T <- summary_eur_i %>% pull(Date_T)
  
  # Define cumulative cases on dates of first restriction, lockdown, lockdown eased, and lockdown lifted
  cc_first_restriction <- summary_eur_i %>% pull(Cumulative_cases_beg_first_restriction)
  cc_lockdown <- summary_eur_i %>% pull(Cumulative_cases_beg_lockdown)
  cc_lockdown_eased <- summary_eur_i %>% pull(Cumulative_cases_beg_lockdown_eased)
  cc_lockdown_end <- summary_eur_i %>% pull(Cumulative_cases_beg_lockdown_end)
  
  # Re-filter cases/deaths dataset to only include data up to date_T
  # (don't do this if you want all data displayed / want to see data up to lockdown)
  # data_eur_i <- data_eur_i %>% filter(Date <= date_T)
  
  # Create dataframe which maps colours and linetyples onto important dates
  dates <- data.frame(xint_date = c(date_first_restriction, date_lockdown, date_lockdown_eased, date_lockdown_end),
                      xint_cc = c(cc_first_restriction, cc_lockdown, cc_lockdown_eased, cc_lockdown_end), 
                      Date = c("first restriction imposed", "lockdown imposed", "lockdown eased", "lockdown lifted"),
                      col = c("navyblue", "darkorange", "firebrick", "forestgreen"),
                      lty = c("solid", "dashed", "solid", "dashed"))
  
  # Plot incident cases
  plot_inc <- ggplot(data = data_eur_i, 
                     aes(x = Date, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Incident cases of COVID-19") +
    geom_col(alpha = 0.75) +
    geom_vline(data = dates, aes(xintercept = xint_date, color = Date, linetype = Date)) + 
    scale_color_manual(values = dates$col, breaks = dates$Date) +
    scale_linetype_manual(values = dates$lty, breaks = dates$Date) +
    scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(name = "Number of daily cases",
                       limits = c(0, NA),
                       expand = expansion(mult = c(0, 0))) 
  
  # Plot cumulative cases
  plot_cum <- ggplot(data = data_eur_i, 
                     aes(x = Date, y = Cumulative_cases_end)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative cases of COVID-19") +
    geom_col(alpha = 0.75) +
    geom_vline(data = dates, aes(xintercept = xint_date, color = Date, linetype = Date)) + 
    scale_color_manual(values = dates$col, breaks = dates$Date) +
    scale_linetype_manual(values = dates$lty, breaks = dates$Date) +
    scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(name = "Number of cumulative cases",
                       limits = c(0, NA),
                       expand = expansion(mult = c(0, 0)))
  
  # Plot incident vs cumulative cases
  plot_exp <- ggplot(data = data_eur_i,
                     aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative vs incident cases of COVID-19") +
    geom_path(alpha = 0.75) +
    geom_point(alpha = 0.75, size = 0.5) +
    geom_vline(data = dates, aes(xintercept = xint_cc, color = Date, linetype = Date)) + 
    scale_color_manual(values = dates$col, breaks = dates$Date) +
    scale_linetype_manual(values = dates$lty, breaks = dates$Date) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       limits = c(0, NA),
                       labels = comma_format(accuracy = 1))
  
  # Combine in triple panel (with title as country)
  p <- ggarrange(plotlist = list(plot_inc, plot_cum, plot_exp), nrow = 1, ncol = 3, common.legend = TRUE, legend = "bottom")
  p_annotated <- annotate_figure(p, top = text_grob(paste0(country),  size = 20),
                                 bottom = text_grob("Data from https://github.com/CSSEGISandData/COVID-19 and https://github.com/OxCGRT/covid-policy-tracker", size = 8))
  
  # Save to list
  plot_coutry_cases[[i]] <- p_annotated
  
  # Save plot to subfolder
  ggsave(paste0(out_folder, "/", country, ".png"), plot = p_annotated, width = 6*3, height = 6)
  
}

# Remove plotting objects from environment
rm(out_folder, i, country, data_eur_i, summary_eur_i, 
   date_first_restriction, date_lockdown, date_lockdown_eased, date_lockdown_end,
   cc_first_restriction, cc_lockdown, cc_lockdown_eased, cc_lockdown_end,
   dates, plot_inc, plot_cum, plot_exp, p, p_annotated)

# ------------------------------------------------------------------------------
# Simulation results
# ------------------------------------------------------------------------------

# Incident, cumulative, and cumulative vs incident cases up to...?

# note: max_t (365 days) extends beyond true data; need to edit max date on plots (adaptable for each country)

# Create folder for storing figures of incident and cumulative cases by country, 
# if none already exists
out_folder <- paste0(out, "Figures - simulation results by country")
if(!dir.exists(out_folder)) {
  dir.create(out_folder)
} else {
  print("Folder already exists")
}

# Create list for plots
plot_coutry_sim <- list()

# Create plots
for (i in countries_eur_lockdown) {
  
  # Define country
  country <- i
  
  # Filter datasets by country
  summary_daily_cases_sim_i <- summary_daily_cases_sim %>% filter(Country == country)
  summary_cumulative_cases_end_sim_i <- summary_cumulative_cases_end_sim %>% filter(Country == country)
  summary_thresholds_i <- summary_thresholds %>% filter(Country == country)
  data_eur_lockdown_i <- data_eur_lockdown %>% filter(Country == country)
  summary_eur_lockdown_i <- summary_eur_lockdown %>% filter(Country == country)
  knots_best_i <- knots_best %>% filter(Country == country)
  
  # Define number of best knot point pairs
  n_knots_i <- nrow(knots_best_i)
  
  # Define important dates
  date_50 <- summary_eur_lockdown_i %>% pull(Date_50)
  date_first_restriction <- summary_eur_lockdown_i %>% pull(Date_first_restriction)
  date_lockdown <- summary_eur_lockdown_i %>% pull(Date_lockdown)
  date_lockdown_eased <- summary_eur_lockdown_i %>% pull(Date_lockdown_eased)
  date_T <- summary_eur_lockdown_i
  
  # Define cumulative cases on important dates
  cc_first_restriction <- summary_eur_lockdown_i %>% pull(Cumulative_cases_beg_first_restriction)
  cc_lockdown <- summary_eur_lockdown_i %>% pull(Cumulative_cases_beg_lockdown)
  cc_lockdown_eased <- summary_eur_lockdown_i %>% pull(Cumulative_cases_beg_lockdown_eased)
  
  # Calculate max_date (end date to display) as either...
  # date_max or first date when daily cases equal zero, whichever comes first
  # max_date 
  
  # Create copy of datasets which include data only up to date_T
  #summary_daily_cases_sim_i <- summary_daily_cases_sim_i %>% filter(Date <= date_T)
  #summary_cumulative_cases_end_sim_i <- summary_cumulative_cases_end_sim_i %>% filter(Date <= date_T)
  data_eur_lockdown_T_i <- data_eur_lockdown_i %>% filter(Date <= date_T)
  
  # Calculate upper limits of y-axes
  y_max_inc <- max(summary_daily_cases_sim_i$C_975, data_eur_lockdown_T_i$Daily_cases)
  y_max_cum <- max(summary_cumulative_cases_end_sim_i$C_975, data_eur_lockdown_T_i$Cumulative_cases_end)
  
  # Create dataframe which maps colours onto thresholds
  threshold_value <- data.frame(yint_threshold = pull(summary_thresholds_i, Threshold_value),
                                Threshold = percent(pull(summary_thresholds_i, Threshold)),
                                col = c("darkorchid4", "mediumorchid1", "firebrick1"))
  
  # (1) Plot incident cases
  plot_inc <- ggplot(data = summary_daily_cases_sim_i, 
                     aes(x = Date, y = Mean)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Incident cases of COVID-19") +
    geom_col(data = data_eur_lockdown_T_i, aes(x = Date, y = Daily_cases), alpha = 0.5) +
    geom_col(data = filter(data_eur_lockdown_i, Date > date_T), aes(x = Date, y = Daily_cases), alpha = 0.2) +
    geom_line(color = "navyblue", size = 1) +
    geom_ribbon(aes(ymin = C_025, ymax = C_975), fill = "navyblue", alpha = 0.25) +
    geom_hline(data = threshold_value, aes(yintercept = yint_threshold, color = Threshold),
               linetype = "dotdash") +
    #geom_text(data = threshold_value, 
    #          aes(x = as.Date("2020-01-14"), y = yint_threshold, label = Threshold, color = Threshold), 
    #          hjust = 0, vjust = 0, size = 3) +
    scale_color_manual(values = threshold_value$col, breaks = threshold_value$Threshold) +
    scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Number of daily cases") +
    coord_cartesian(ylim = c(0, y_max_inc), expand = FALSE)
  
  # (2) Plot cumulative cases
  plot_cum <- ggplot(data = summary_cumulative_cases_end_sim_i, 
                     aes(x = Date, y = Mean)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative cases of COVID-19") +
    geom_col(data = data_eur_lockdown_T_i, aes(x = Date, y = Cumulative_cases_end), alpha = 0.5) +
    geom_col(data = filter(data_eur_lockdown_i, Date > date_T), aes(x = Date, y = Cumulative_cases_end), alpha = 0.2) +
    geom_line(color = "navyblue", size = 1) +
    geom_ribbon(aes(ymin = C_025, ymax = C_975), fill = "navyblue", alpha = 0.25) +
    scale_x_date(name = "Date", date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Number of cumulative cases") +
    coord_cartesian(ylim = c(0, y_max_cum), expand = FALSE)
  
  # (3) Plot incident vs cumulative cases
  ## Base plot:
  plot_exp <- ggplot(data = filter(data_eur_lockdown_T_i, Date >= date_50),
                     aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = "Cumulative vs incident cases of COVID-19") +
    geom_path(alpha = 0.5) +
    geom_path(data = filter(data_eur_lockdown_T_i, Date <= date_50),
              aes(x = Cumulative_cases_beg, y = Daily_cases),
              linetype = "dashed", alpha = 0.5) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_x_continuous(name = "Cumulative number of COVID-19 cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "New daily number of COVID-19 cases",
                       limits = c(0, y_max_inc),
                       expand = expansion(mult = c(0, 0)),
                       labels = comma_format(accuracy = 1))
  ## Add fitted lines corresponding to best knot dates onto base plot:
  for (j in 1:nrow(knots_best_i)) {
    
    # Filter best knots dataset
    knots_best_j <- knots_best_i[j, ]
    
    # Define knot date pair
    knot_date_1 <- knots_best_j %>% pull(Knot_date_1)
    knot_date_2 <- knots_best_j %>% pull(Knot_date_2)
    
    # Define values of cumulative cases at knot dates
    knot_1 <- data_eur_lockdown_T_i %>% filter(Date == knot_date_1) %>% pull(Cumulative_cases_beg)
    knot_2 <- data_eur_lockdown_T_i %>% filter(Date == knot_date_2) %>% pull(Cumulative_cases_beg)
    
    # Calculate min and max values of cumulative cases in modelling period
    x_min <- data_eur_lockdown_T_i %>% filter(Date >= date_50) %>% pull(Cumulative_cases_beg) %>% min()
    x_max <- data_eur_lockdown_T_i %>% filter(Date >= date_50) %>% pull(Cumulative_cases_beg) %>% max()
    
    # Define Arima spline parameters
    slope_1 <- knots_best_j %>% pull(Growth_factor_1) %>% head(1) - 1
    slope_2 <- knots_best_j %>% pull(Growth_factor_2) %>% head(1) - 1
    slope_3 <- knots_best_j %>% pull(Growth_factor_3) %>% head(1) - 1
    intercept_1 <- knots_best_j %>% pull(Intercept_1) %>% head(1)
    intercept_2 <- knots_best_j %>% pull(Intercept_2) %>% head(1)
    intercept_3 <- knots_best_j %>% pull(Intercept_3) %>% head(1)
    
    # Add fitted line
    if (knot_date_1 == date_50) {
      if (is.na(knot_date_2)) {  # NO knot points
        plot_exp <- plot_exp +
          geom_segment(aes_(x = x_min, xend = x_max,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*x_max),
                       color = "navyblue", size = 0.25, linetype = "dashed")
      } else {  # ONE knot point (at knot_date_2)
        plot_exp <- plot_exp +
          geom_segment(aes_(x = x_min, xend = knot_2,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_2),
                       color = "navyblue", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = x_max,
                            y = intercept_2 + slope_2*knot_2, yend = intercept_2 + slope_2*x_max),
                       color = "navyblue", size = 0.25, linetype = "dashed") 
      }
    } else {
      if (is.na(knot_date_2)) {  # ONE knot point (at knot_date_1)
        plot_exp <- plot_exp +
          geom_segment(aes_(x = x_min, xend = knot_1,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                       color = "navyblue", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = x_max,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*x_max),
                       color = "navyblue", size = 0.25, linetype = "dashed") 
      } else {  # TWO knot points (at knot_date_1 and knot_date_2)
        plot_exp <- plot_exp +
          geom_segment(aes_(x = x_min, xend = knot_1,
                            y = intercept_1 + slope_1*x_min, yend = intercept_1 + slope_1*knot_1),
                       color = "navyblue", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = knot_2,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                       color = "navyblue", size = 0.25, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = x_max,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*x_max),
                       color = "navyblue", size = 0.25, linetype = "dashed") 
      }
    }  # (close if-else section)
  }  # (close fitted line section)
  
  # Combine in triple panel (with title as country)
  p <- ggarrange(plotlist = list(plot_inc, plot_cum, plot_exp), common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 3)
  p_annotated <- annotate_figure(p, top = text_grob(paste0(country),  size = 20),
                                 bottom = text_grob("Data from https://github.com/CSSEGISandData/COVID-19", size = 8))
  
  # Save to list
  plot_coutry_sim[[i]] <- p_annotated
  
  # Save plot to subfolder
  ggsave(paste0(out_folder, "/", country, ".png"), plot = p_annotated, width = 6*3, height = 6)
  
}

# Remove plotting objects from environment
rm(out_folder, i, j, 
   summary_daily_cases_sim_i, summary_cumulative_cases_end_sim_i, summary_thresholds_i,
   data_eur_lockdown_i, summary_eur_lockdown_i, knots_best_i,
   n_knots_i, date_50, date_first_restriction, date_lockdown, date_lockdown_eased,
   cc_first_restriction, cc_lockdown, cc_lockdown_eased,
   date_T, y_max_inc, y_max_cum, threshold_value,
   plot_inc, plot_cum, plot_exp, knots_best_j, knot_date_1, knot_date_2,
   knot_1, knot_2, x_min, x_max, 
   slope_1, slope_2, slope_3, intercept_1, intercept_2, intercept_3,
   p, p_annotated)


