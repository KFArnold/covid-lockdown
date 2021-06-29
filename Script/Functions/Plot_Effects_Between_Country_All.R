#' Create figures of all within-country effects.
#'
#' @param exposures Vector of exposures to include in figure
#' @param outcomes Vector of outcomes to include in figure
#' @param analyses Vector of analyses to include in figure
#' @param out_folder Folder to save combined figure in
#'
#' @return Named list containing estimated effects from specified 
#' \code{analyses} for specified \code{exposures} and \code{outcomes}, 
#' and combined plot ('plot_combined')
#'
#' @examples
#' Plot_Effects_Between_Country_All(outcomes = "Length_lockdown",
#' out_folder = "./Output/Figures/")
Plot_Effects_Between_Country_All <- function(exposures = c("Daily_cases_MA7",
                                                           "Cumulative_cases_beg"), 
                                             outcomes = c("Length_lockdown",
                                                          "Median_growth_factor_lockdown"), 
                                             analyses = c("Unadjusted",
                                                          "Primary", "Secondary"),
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
  n_out <- length(outcomes)
  
  # Filter between-country effects by specified exposures and analyses
  effects <- as.list(exposures) %>%
    map_dfr(., .f = ~filter(effects_between_countries_formatted, str_detect(Exposure, .x))) %>%
    filter(Analysis %in% analyses)
  
  # Create empty list for storing plots
  plot_list <- list()
  
  # Plot length of lockdown and add to plot list
  if ("Length_lockdown" %in% outcomes) {
    plot_length_lockdown <- Plot_Effects_Between_Length_Lockdown(effects = effects)
    plot_list[["plot_length_lockdown"]] <- plot_length_lockdown
  } 
  
  # Plot growth factor under lockdown and add to plot list
  if ("Median_growth_factor_lockdown" %in% outcomes) {
    plot_growth_factor <- Plot_Effects_Between_Growth_Factor(effects = effects)
    plot_list[["plot_growth_factor"]] <- plot_growth_factor
  }
  
  # Combine all plots into single figure and save to specified folder,
  # and add to plot list
  plot_combined <- Plot_Combined(plotlist = plot_list, 
                                 height = 7, 
                                 width = 2.8*n_exp,
                                 cols = n_out, 
                                 title = "Estimated between-country effects of lockdown timing",
                                 title_size = 15, 
                                 out_folder = out_folder,
                                 out_name = "Effects between countries.png",
                                 return = TRUE)
  plot_list[["plot_combined"]] <- plot_combined
  
  # Return list of individual and combined plots
  return(plot_list)
  
}
