#' Create figures of all within-country effects.
#'
#' @param simulations Vector of simulations to include
#' @param outcomes Vector of outcomes to create 
#' @param description Text description of figure for saving
#' @param out_folder Folder to save combined figure in
#'
#' @return Named list containing plots for each of specified \code{outcomes} 
#' and combined figure ('plot_combined'). The combined figure is also saved to 
#' the specified folder as 'Figure - Effects within countries - \code{description}.png'.
#'
#' @examples
#' Plot_Effects_Within_Country_All(simulations = c("0,1", "0,3", "0,5", "0,7"),
#' description = "earlier lockdown", out_folder = "./Output/Figures/")
Plot_Effects_Within_Country_All <- function(simulations, 
                                            outcomes = c("Time_to_threshold",
                                                         "Length_lockdown",
                                                         "Total_cases"),
                                            description, 
                                            out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import formatted data
  data_formatted <- Format_Data_For_Plotting(filenames = "effects_within_countries")
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")
  
  # Record number of specified simulations and plots
  n_sim <- length(simulations)
  n_plots <- length(outcomes)
  
  # Filter within-country effects dataframe by specified simulations & outcomes, 
  # and convert proportions to percentages
  effects <- effects_within_countries_formatted %>%
    filter(Simulation %in% simulations,
           Outcome %in% outcomes) %>%
    mutate(Pct_change = 100*Pct_change)
  
  # Calculate max value of percentage change across all outcomes, 
  # and round up to nearest 10
  max_pct_change <- effects %>% pull(Pct_change) %>% max(na.rm = TRUE) %>%
    plyr::round_any(accuracy = 10)
  
  # Create empty lists to store specified plots
  plot_list <- list()
  
  # Plot length of lockdown and add to list, if specified
  if ("Length_lockdown" %in% outcomes) {
    plot_length_lockdown <- 
      Plot_Effects_Within_Length_Lockdown(effects = effects,
                                          max_y = max_pct_change)
    plot_list[["plot_length_lockdown"]] <- plot_length_lockdown
  } 
  
  # Plot time to thresholds and add to list, if specified
  if ("Time_to_threshold" %in% outcomes) {
    plot_time_to_threshold <- Plot_Effects_Within_Time_To_Threshold(effects = effects,
                                                                    max_y = max_pct_change)
    plot_list[["plot_time_to_threshold"]] <- plot_time_to_threshold
  } 
  
  # Plot total cases and add to list, if specified
  if ("Total_cases" %in% outcomes) {
    plot_total_cases <- Plot_Effects_Within_Total_Cases(effects = effects,
                                                        max_y = max_pct_change)
    plot_list[["plot_total_cases"]] <- plot_total_cases
  } 
  
  # Define name of figure to be saved
  name <- paste0("Effects within countries - ", description, ".png")
  
  # Combine all plots into single figure and save to specified folder,
  # and add to plot list
  plot_combined <- Plot_Combined(plotlist = plot_list, 
                                 height = 7, 
                                 width = 1.4*n_sim,
                                 cols = n_plots, 
                                 title = "Estimated within-country effects of lockdown timing",
                                 title_size = 15, 
                                 out_folder = out_folder,
                                 out_name = name,
                                 return = TRUE)
  plot_list[["plot_combined"]] <- plot_combined
  
  # Return list of individual and combined plots
  return(plot_list)
  
}

