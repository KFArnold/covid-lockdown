library(RColorBrewer); library(scales)

# Simulation aesthetics --------------------------------------------------------

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


