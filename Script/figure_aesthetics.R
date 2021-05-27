library(RColorBrewer); library(scales); library(ggpubr)

# Import all files

# Import required files
Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                        "knots_best",
                                        "thresholds_eur"), 
                          silent = TRUE)
Import_All_Simulated_Data(location = out_folder,
                          silent = TRUE)

# DEFINE AESTHETICS ------------------------------------------------------------

## Simulation aesthetics -------------------------------------------------------

# Simulation levels (defines ordering)
simulation_levels <- c("0,0", "0,1", "0,3", "0,5", "0,7", 
                       "1,1", "3,3", "5,5", "7,7")  

# Key for simulation labels
#simulation_labels <- c("0,0" = "(a - 0 , b - 0)", 
#                       "0,1" = "(a - 0 , b - 1)", 
#                       "0,3" = "(a - 0 , b - 3)", 
#                       "0,5" = "(a - 0 , b - 5)", 
#                       "0,7" = "(a - 0 , b - 7)", 
#                       "1,1" = "(a - 1 , b - 1)", 
#                       "3,3" = "(a - 3 , b - 3)", 
#                       "5,5" = "(a - 5 , b - 5)", 
#                       "7,7" = "(a - 7 , b - 7)")
#

# Create function which labels simulation levels
# (e.g. if level is "0,0", label is "(a - 0 , b - 0)")
Simulation_Labeller <- function(simulation_level) {
  simulation_level %>% 
    gsub(pattern = ",", replacement = " , b - ", .) %>% 
    paste0("(a - ", ., ")")
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
threshold_levels <- c("1 case per 100,000", "1 case per 20,000", "1 case per 10,000")

# Create function which labels threshold levels
# (e.g. if level is "1 case per 100,000", label is "1 case per\n100,000")
Threshold_Labeller <- function(threshold_level) {
  threshold_level %>%
    gsub(pattern = "per ", replacement = "per\n")
}

# Create label, shape, and transparency key for threshold levels
threshold_aes <- tibble(Threshold = threshold_levels,
                        Label = Threshold_Labeller(Threshold),
                        Shape = c(15, 16, 17),
                        Alpha = c(0.4, 0.7, 1))




### Example function for creating labels
#
#appender <- function(string, suffix = "-foo") paste0(string, suffix)
#p + facet_wrap(~am, labeller = as_labeller(appender))
#
#
#Simulation_Labeller <- function(simulation_level) {
#  simulation_level %>% 
#    gsub(pattern = ",", replacement = " , b - ", .) %>% 
#    paste0("(a - ", ., ")")
#}
#p + facet_wrap(~x, labeller = as_labeller(Simulation_Labeller))


