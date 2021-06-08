#' Create figures of all within-country effects.
#'
#' @param exposures Vector of exposures to include in figure
#' @param plots Vector of plots to include in figure (possible values = 
#' c("plot_length_lockdown", "plot_growth_factor"))
#' @param out_folder Folder to save combined figure in
#'
#' @return Named list containing specified \code{plots} and combined plot 
#' ('plot_combined')
#'
#' @examples
#' Plot_Effects_Between_Country_All(plots = "plot_length_lockdown",
#' out_folder = "./Output/Figures/")
Plot_Effects_Between_Country_All <- function(exposures = c("Daily_cases_MA7",
                                                           "Cumulative_cases_beg"), 
                                             plots = c("plot_length_lockdown",
                                                       "plot_growth_factor"), 
                                             out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import formatted data
  data_formatted <- Format_Data_For_Plotting(filenames = "effects_between_countries")
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")
  
  # Record number of specified exposures and plots
  n_exp <- length(exposures)
  n_plots <- length(plots)
  
  # Filter between-country effects by specified exposures
  effects <- as.list(exposures) %>%
    map(., .f = ~filter(effects_between_countries_formatted, str_detect(Exposure, .x))) %>%
    bind_rows
  
  # Create empty list for storing plots
  plot_list <- list()
  
  # Plot length of lockdown and add to plot list
  if ("plot_length_lockdown" %in% plots) {
    plot_length_lockdown <- Plot_Effects_Between_Length_Lockdown(effects = effects)
    plot_list[["plot_length_lockdown"]] <- plot_length_lockdown
  } 
  
  # Plot growth factor under lockdown and add to plot list
  if ("plot_growth_factor" %in% plots) {
    plot_growth_factor <- Plot_Effects_Between_Growth_Factor(effects = effects)
    plot_list[["plot_growth_factor"]] <- plot_growth_factor
    
  }
  
  # Define name of figure to be saved
  name <- "Figure - Effects between countries.png"
  
  # Combine all plots into single figure and save to specified folder,
  # and add to plot list
  plot_combined <- Plot_Combined(plotlist = plot_list, 
                                 height = 7, 
                                 width = 4*n_exp,
                                 cols = n_plots, 
                                 title = "Estimated between-country effects of lockdown timing",
                                 title_size = 20, 
                                 out_folder = out_folder,
                                 out_name = name,
                                 return = TRUE)
  plot_list[["plot_combined"]] <- plot_combined
  
  
  # Return list of individual and combined plots
  return(plot_list)
  
}
