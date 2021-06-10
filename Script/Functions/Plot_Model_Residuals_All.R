#' Create two-panel figure of simulated model residuals (with resepect to both 
#' incident and cumulative cases).
#' 
#' This function is largely a wrapper for the function 'Plot_Model_Residuals', 
#' which creates a single figure of model residuals with respect to either 
#' incident or cumulative cases.
#'
#' @param country Country
#' @param out_folder Where to save combined figure
#'
#' @return List of 2 items:
#' (1) 'plot_inc' = figure displaying residuals (observed incidnet cases minus
#' simulated incident cases) over time, with title = \code{country}; and
#' (2) 'plot_cum' = figure displaying residuals (observed incidnet cases minus
#' simulated cumulative cases) over time, with title = \code{country}.
#'
#' @examples
#' Plot_Model_Residuals_All(country = "United Kingdom", 
#' out_folder = "./Output/Figures/Model residuals by country/")
Plot_Model_Residuals_All <- function(country, out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import file containing observed case data
  Import_Unloaded_CSV_Files(filenames = "Cases_deaths_data_europe",
                            silent = TRUE)
  
  # Plot residuals of incident cases
  plot_inc <- Plot_Model_Residuals(country = country,
                                   cases = "Daily_cases")
  
  # Plot residuals of cumulative cases
  plot_cum <- Plot_Model_Residuals(country = country,
                                   cases = "Cumulative_cases_end")
  
  # Create copy of plots with description as title
  plot_inc_copy <- plot_inc + 
    labs(title = "Model residuals: incident cases")
  plot_cum_copy <- plot_cum +
    labs(title = "Model residuals: cumulative cases")
  
  #  Combine copied plots in double panel with country as title,
  # and save to specified out folder
  plots_all <- Plot_Combined(plotlist = list(plot_inc_copy, plot_cum_copy),
                             rows = 1,
                             cols = 2,
                             title = paste(country),
                             title_size = 20, 
                             out_folder = out_folder,
                             out_name = paste0(country, ".png"))
  
  # Return lists of individual plots
  return(list(plot_inc = plot_inc, 
              plot_cum = plot_cum))
  
}
