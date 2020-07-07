# ------------------------------------------------------------------------------
# Notes
# ------------------------------------------------------------------------------

# This script creates various figures related to COVID-19:
# (1) Important dates by country (including dates of first restrictions and lockdowns)

# ------------------------------------------------------------------------------
# Setup
# ------------------------------------------------------------------------------

# Load required packages
library(tidyverse); library(readr); library(dplyr); library(ggplot2)

# Run source code to import, format, and summarise data
source("./Code/Import, format, and summarise data.R")

# Set storage directory for outputs
out <- paste0("./Results/")

# ------------------------------------------------------------------------------
# Important dates
# ------------------------------------------------------------------------------

# Order countries by date of first restriction
countries_ordered <- summary_eur_final %>% arrange(Date_first_restriction) %>% 
  pull(Country) %>% as.character

# Dates of first restriction and lockdown --------------------------------------

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

# Dates of first restriction, lockdown, first case, and cases >= 100 -----------

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

