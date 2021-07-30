#' Create figure of simulated cumulative cases for a given country.
#'
#' @param country Country
#' @param title Title of figure
#' @param labs Whether axes should be labelled (T/F; default is TRUE)
#' @param min_date Minimum date to display on figure
#' @param max_date Maximum date to display on figure
#' @param obs_data Dataframe of observed data for the specified country
#' @param sim_data Dataframe of simulated cumulative data for the specified country
#' @param simulations Vector of simulations to include in figure
#' @param aesthetics Aesthetic mapping for simulation onto colours 
#' @param date_T Last date of observed data included in modelling period
#' @param print_cases Whether to print value of cases on date_T on plot
#' (T/F, default = FALSE)
#'
#' @return Figure of simulated cumulative cases for the specified \code{country},
#' overlaid the true observed cumulative case data
#'
#' @examples
Plot_Cumulative_Cases_Sim <- function(country, title, labs = TRUE, 
                                      min_date, max_date, obs_data, sim_data, 
                                      simulations, aesthetics, date_T, print_cases = FALSE) {
  
  # Define x-axis range
  x_min <- min_date
  x_max <- max_date
  
  # Define min y-axis value
  y_min <- 0
  
  # Calculate max y-axis value as upper limit of cumulative case data in displayed date range 
  # (95% SI upper bound or max number of observed cases, whichever is greater)
  y_max <- max(filter(sim_data, Date <= max_date)$C_upper, 
               filter(obs_data, In_range == TRUE)$Cumulative_cases_end)
  
  # Plot observed cases 
  plot <- ggplot(data = obs_data,
                 aes(x = Date, y = Cumulative_cases_end)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.position = "bottom") +
    labs(title = title) +
    geom_col(data = filter(obs_data, In_range == FALSE), 
             aes(x = Date, y = Cumulative_cases_end), alpha = 0.2) +
    geom_col(data = filter(obs_data, In_range == TRUE), 
             aes(x = Date, y = Cumulative_cases_end), alpha = 0.5) +
    scale_x_date(name = ifelse(labs == TRUE, "Date", ""), 
                 limits = c(x_min, x_max), 
                 date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = ifelse(labs == TRUE, "Cumulative number of cases", ""),
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(ylim = c(y_min, y_max), expand = FALSE)
  
  # Add simulated lines
  plot <- plot + 
    geom_line(data = sim_data,
              aes(x = Date, y = Mean, 
                  group = Simulation,
                  color = Simulation), 
              size = 1) +
    geom_ribbon(data = sim_data,
                aes(x = Date, y = Mean, 
                    ymin = C_lower, 
                    ymax = C_upper,
                    group = Simulation,
                    fill = Simulation),
                alpha = 0.15) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       limits = aesthetics$Simulation,
                       breaks = aesthetics$Simulation,
                       labels = str_wrap(aesthetics$Label, width = 27)) +
    scale_fill_manual(name = "Simulation:",
                      values = aesthetics$Color, 
                      limits = aesthetics$Simulation,
                      breaks = aesthetics$Simulation,
                      labels = str_wrap(aesthetics$Label, width = 27))
  
  # If print_cases = TRUE, add text for cumulative cases on date_T
  if (print_cases == TRUE) {
    plot <- plot + 
      geom_text_repel(data = sim_data,
                      aes(x = Date, y = Mean,
                          color = Simulation,
                          label = ifelse(Date == date_T, 
                                         formatC(Mean, 
                                                 format = "f", big.mark = ",", digits = 0), "")),
                      bg.color = "white", bg.r = 0.25,
                      hjust = 0.5, vjust = 0.5, size = 3, fontface = 2, 
                      inherit.aes = FALSE,
                      max.overlaps = Inf, 
                      show.legend = FALSE)
  }
  
  # Return plot
  return(plot)
  
}
