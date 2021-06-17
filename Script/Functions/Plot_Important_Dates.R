#' Create figure of important dates for a group of countries.
#'
#' @param countries List of countries to include
#' @param dates Vector of dates to include (possible values = c("Date_1", 
#' "Date_first_restriction", "Date_restrictions_eased", "Date_lockdown", 
#' "Date_lockdown_eased", "Date_lockdown_end", "Date_start", "Date_T"))
#' @param order Date by which to order countries in the figure
#' @param out_folder Folder to save figure in
#'
#' @return Figure of important dates.
#'
#' @examples
#' Plot_Important_Dates(countries = countries_eur,
#' dates = c("Date_1", "Date_first_restriction", "Date_lockdown", "Date_lockdown_eased"),
#' order = "Date_lockdown", out_folder = "./Output/Figures/")
Plot_Important_Dates <- function(countries, dates, order, 
                                 out_folder) {
  
  # Format table containing important dates for plotting
  data_formatted <- Format_Data_For_Plotting(filenames = "summary_eur",
                                             silent = TRUE)
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")
  
  # Filter summary dataframe by specified countries and dates
  data <- summary_eur_formatted %>%
    filter(Country %in% countries,
           Date %in% c(all_of(dates), "Date_start", "Date_T")) %>%
    droplevels
  
  # Order countries by specified ordering date 
  countries_ordered <- data %>% 
    filter(Date == order) %>% 
    arrange(Value) %>% pull(Country)
  
  # Create list of countries which entered lockdown
  countries_lockdown <- data %>% 
    filter(Date == "Date_lockdown", !is.na(Value)) %>% 
    pull(Country) 
  
  # Get min and max dates from summary table
  date_min <- data %>% pull(Value) %>% min(na.rm = TRUE)
  date_max <- data %>% pull(Value) %>% max(na.rm = TRUE)
  
  # Filter aesthetics key by specified dates
  date_aes_filt <- date_aes %>%
    filter(Date %in% c(all_of(dates), "Date_start", "Date_T"))
  
  # Plot specified countries and dates
  plot <- ggplot(data = data %>% filter(Date %in% dates), 
                 aes(x = Value, y = Country)) + 
    theme_minimal() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.margin = margin(0, 0, 0, 0, "cm"),
          legend.spacing.y = unit(0.1, "cm"),
          plot.caption = element_text(margin = margin(0.5, 0, 0, 0, "cm"))) +
    labs(title = "Important dates in COVID-19 European policy responses",
         caption = "Data from Johns Hopkins University CSSE COVID-19 Data Repository (https://github.com/CSSEGISandData/COVID-19)
         and Oxford Covid-19 Government Response Tracker (https://github.com/OxCGRT/covid-policy-tracker).") +
    theme(plot.caption = element_text(size = 7),
          plot.subtitle = element_text(size = 10)) +
    geom_line(data = data %>% 
                filter(Country %in% countries_lockdown,
                       Date %in% c("Date_start", "Date_T")),
              aes(group = Country, color = "grey70"),
              alpha = 0.5, size = 2) +
    scale_color_manual(name = "",
                       values = "grey70",
                       labels = "range of dates included in within-country analysis (1 \u2264 t \u2264 T)") +
    guides(color = guide_legend(order = 2)) +
    ggnewscale::new_scale_color() +
    geom_point(aes(color = Date, shape = Date, size = Date)) +
    scale_color_manual(name = "Date of:",
                       values = date_aes_filt$Color,
                       labels = date_aes_filt$Label) +
    scale_shape_manual(name = "Date of:",
                       values = date_aes_filt$Shape,
                       labels = date_aes_filt$Label) +
    scale_size_manual(name = "Date of:",
                      values = date_aes_filt$Size,
                      labels = date_aes_filt$Label) +
    guides(color = guide_legend(order = 1),
           shape = guide_legend(order = 1),
           size = guide_legend(order = 1)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    scale_x_date(name = "", 
                 limits = c(date_min - 7, date_max + 7), 
                 date_breaks = "1 week", 
                 date_labels = "%d %b %C",
                 expand = expansion(mult = c(0, 0))) +
    scale_y_discrete(name = "",
                     limits = rev(countries_ordered)) 
  
  # Save plot to subfolder
  ggsave(paste0(out_folder, "Important dates.png"), 
         plot = plot, width = 8, height = 8)
  
  # Return plot
  return(plot)
  
}
