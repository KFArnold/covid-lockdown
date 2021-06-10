#' Create figure of all specified model fit statistics.
#'
#' @param countries List of countries to include
#' @param measures List of model fit measures to display in figure
#' (values in c("Diff_time_to_threshold", "Diff_total_cases", 
#' "Pois_dev_inc", "Pois_dev_cum"); default is all)
#' @param out_folder Folder to save figure in
#'
#' @return Figure displaying all specified \code{measures} for all specicied 
#' \code{countries}. Figure is faceted by measure and type (i.e. raw value or
#' percentage difference compared to observed data). Outliers with respect to each
#' measure are additionally labelled in each facet.
#'
#' @examples
#' Plot_Model_Fit(countries = countries_eur, out_folder = "./Output/Figures/")
Plot_Model_Fit <- function(countries, 
                           measures = c("Diff_time_to_threshold", "Diff_total_cases", 
                                        "Pois_dev_inc", "Pois_dev_cum"),
                           out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import formatted data
  data_formatted <- Format_Data_For_Plotting(filenames = "model_fit")
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Filter model fit data by designated countries and measures
  model_fit_formatted_filt <- model_fit_formatted %>% 
    filter(Country %in% countries, Measure %in% measures)
  
  # Record number of distinct groups by measure and type
  n_groups <- model_fit_formatted_filt %>% 
    group_by(Measure, Type) %>% n_groups
  
  # Create faceted plot
  plot <- ggplot(data = model_fit_formatted_filt, 
                 aes(x = Threshold, y = Value, 
                     color = Measure)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          axis.text.x = element_text(angle = 90, hjust = 0.95, vjust = 0.5),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20")) +
    guides(color = FALSE) +
    geom_hline(yintercept = 0, color = "gray20", lty = "dashed") +
    labs(title = "Model fit statistics") +
    geom_point(shape = 16, alpha = 0.6) +
    geom_text(aes(label = ifelse(Outlier == TRUE, 
                                 paste(Country), 
                                 "")),
              size = 2, hjust = 0.5, vjust = 1, 
              color = "gray40", fontface = "italic") + 
    stat_summary(fun = median, shape = 18, size = 1.5) +
    facet_nested_wrap(. ~ Measure + Type,
                      scale = "free", ncol = n_groups,
                      labeller = labeller(Measure = model_fit_labels,
                                          Type = model_fit_type_labels)) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_continuous(name = "",
                       labels = comma_format()) +
    coord_cartesian(clip = "off")
  
  # Save combined plot to Results folder
  ggsave(paste0(out_folder, "Model fit.png"), plot = plot, width = 2*n_groups, height = 7)
  
  # Return plot
  return(plot)
  
}
