#' Create figure of simulated incident cases for a given country.
#'
#' @param country Country
#' @param title Title of figure
#' @param labs Whether axes should be labelled (T/F; default is TRUE)
#' @param min_date Minimum date to display on figure
#' @param max_date Maximum date to display on figure
#' @param obs_data Dataframe of observed data for the specified country
#' @param sim_data Dataframe of simulated incidence data for the specified country
#' @param simulations Vector of simulations to include in figure
#' @param aesthetics Aesthetic mapping for simulation onto colours 
#' @param threshold_data Dataframe containing population-based threshold
#'
#' @return Figure of simulated incident cases for the specified \code{country},
#' overlaid the true observed incidence
#'
#' @examples
Plot_Daily_Cases_Sim <- function(country, title, labs = TRUE, 
                                 min_date, max_date, obs_data, sim_data, 
                                 simulations, aesthetics, threshold_data) {
  
  # Define x-axis range
  x_min <- min_date
  x_max <- max_date
  
  # Define min y-axis value
  y_min <- 0
  
  # Calculate max y-axis value as upper limit of incident case data in date range 
  # (95% SI upper bound or max number of observed cases, whichever is greater);
  ## if country is France, only consider SI upper bound, due to huge outlier which distorts scale
  if (country != "France") {
    y_max <- max(filter(sim_data, Date <= max_date)$C_upper, 
                 filter(obs_data, In_range == TRUE)$Daily_cases)
  } else {
    y_max <- max(filter(sim_data, Date <= max_date)$C_upper)
  }
  
  # Create dataframe which combines threshold aesthetics with threshold values
  threshold_values <- threshold_data %>% 
    select(Threshold, Threshold_value) %>% unique %>% 
    left_join(., threshold_aes, by = "Threshold") %>%
    mutate(Threshold = factor(Threshold, levels = threshold_levels))
  
  # Plot observed cases and threshold values
  plot <- ggplot(data = obs_data,
                 aes(x = Date, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = title) +
    geom_col(data = filter(obs_data, In_range == FALSE), 
             aes(x = Date, y = Daily_cases), alpha = 0.2) +
    geom_col(data = filter(obs_data, In_range == TRUE), 
             aes(x = Date, y = Daily_cases), alpha = 0.5) +
    geom_hline(data = threshold_values, 
               aes(yintercept = Threshold_value, alpha = Threshold),
               linetype = "dotdash", color = "firebrick",
               show.legend = FALSE) +
    geom_text(data = threshold_values, 
              aes(x = min_date + 0.01*(x_max - min_date), 
                  y = Threshold_value + 0.01*y_max, 
                  label = Threshold, alpha = Threshold), 
              hjust = 0, vjust = 0, size = 3, color = "firebrick",
              show.legend = FALSE) +
    scale_alpha_manual(values = threshold_aes$Alpha, 
                       breaks = threshold_aes$Threshold) +
    scale_x_date(name = ifelse(labs == TRUE, "Date", ""), 
                 limits = c(x_min, x_max), 
                 date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = ifelse(labs == TRUE, "Daily number of cases", " "),
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
                    ymin = C_lower, ymax = C_upper,
                    group = Simulation,
                    fill = Simulation),
                alpha = 0.15) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       limits = aesthetics$Simulation,
                       breaks = aesthetics$Simulation,
                       labels = aesthetics$Label) +
    scale_fill_manual(name = "Simulation:",
                      values = aesthetics$Color, 
                      limits = aesthetics$Simulation,
                      breaks = aesthetics$Simulation,
                      labels = aesthetics$Label)
  
  # Return plot
  return(plot)
  
}
