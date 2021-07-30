#' Create figure of length of lockdown required for all countries and for
#' each of the simulations performed.
#'
#' @param out_folder Folder to save figure in
#'
#' @return Dot plot displaying length of lockdown across all simulations,
#' with median value highlighted.
#'
#' @examples 
#' Plot_Length_Lockdown_Sim(out_folder = "./Output/Figures/")
Plot_Length_Lockdown_Sim <- function(out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import formatted data
  data_formatted <- Format_Data_For_Plotting(filenames = "effects_within_countries")
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Filter formatted data by length of lockdown outcome, select rel
  length_lockdown_data <- effects_within_countries_formatted %>%
    filter(Outcome == "Length_lockdown")
  
  # Calculate number of simulations
  n_sim <- length_lockdown_data %>%
    pull(Simulation) %>% levels %>% length
  
  # Create plot
  plot <- ggplot(data = length_lockdown_data,
         aes(x = Simulation, y = Value,
             fill = Simulation, color = Simulation)) +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 15),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.text.x = element_text(size = 11),
          axis.title.y = element_text(size = 12),
          legend.position = "none") +
    labs(title = "Length of full lockdown required in all simulations",
         y = "Days") +
    geom_violin(trim = TRUE, fill = NA) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 1.5, 
                 alpha = 0.5, color = NA) +
    stat_summary(fun = median, shape = 18, size = 2) +
    EnvStats::stat_n_text(y.pos = -5, size = 3, color = "gray50") +
    scale_fill_manual(values = simulation_aes$Color, 
                      breaks = simulation_aes$Simulation,
                      labels = simulation_aes$Label) +
    scale_color_manual(values = simulation_aes$Color, 
                      breaks = simulation_aes$Simulation,
                      labels = simulation_aes$Label) +
    scale_x_discrete(labels = str_wrap(simulation_aes$Label, width = 20))
    
  # Save plot to designated output folder
  ggsave(paste0(out_folder, "Length of lockdown.png"), plot = plot, width = 2*n_sim, height = 7)
  
  # Return plot
  return(plot)
    
}
