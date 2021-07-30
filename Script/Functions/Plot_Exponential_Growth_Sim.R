#' Create figure of simulated cumulative vs incident cases for a given country.
#'
#' @param country Country
#' @param title Title of figure
#' @param labs Whether axes should be labelled (T/F; default is TRUE)
#' @param max_date Maximum date to display on figure
#' @param obs_data Dataframe of observed data for the specified country
#' @param sim_data_inc Dataframe of simulated incidence data for the specified country
#' @param sim_data_cum Dataframe of simulated cumulative data for the specified country
#' @param simulations Vector of simulations to include in figure
#' @param aesthetics Aesthetic mapping for simulation onto colours 
#' @param knots Dataframe containing spline parameters for the specified country
#' @param date_start First date of observed data included in modelling period
#' @param date_T Last date of observed data included in modelling period
#'
#' @return Figure of simulated cumulative vs incident cases for the specified 
#' \code{country}, overlaid the true observed data and fitted splines
#'
#' @examples
Plot_Exponential_Growth_Sim <- function(country, title, labs = TRUE, 
                                        max_date, obs_data, sim_data_inc, sim_data_cum, 
                                        simulations, aesthetics, knots, date_start, date_T) {
  
  # Mutate dataframe for simulated cumulative cases to show counts 
  # at BEGINNING of time t
  sim_data_cum <- sim_data_cum %>%
    group_by(Country, History, N_days_first_restriction, N_days_lockdown) %>%
    mutate(across(c(Mean, C_lower, C_upper), ~lag(., n = 1, default = NA))) %>% ungroup
  
  # Combine all daily/cumulative cases data into single dataframe
  names <- colnames(sim_data_inc)
  col_join <- names[! names %in% c("Mean", "C_lower", "C_upper")]
  sim_data_all <- full_join(sim_data_cum, sim_data_inc,
                            by = col_join,
                            suffix = c("_cum", "_inc"))
  
  # Define x-axis range (cumulative cases)
  x_min <- 0
  x_max <- sim_data_all %>% filter(Date == max_date) %>% pull(Mean_cum) %>% max
  
  # Define y-axis range (incident cases)
  y_min <- 0
  if (country != "France") {
    y_max <- max(filter(sim_data_all, Date <= max_date)$C_upper_inc, 
                 filter(obs_data, In_range == TRUE)$Daily_cases)
  } else {
    y_max <- max(filter(sim_data_all, Date <= max_date)$C_upper_inc)
  }
  
  # Define total number of knot points
  n_knots <- knots %>% pull(N_knots) %>% unique
  
  # Define color, size, and transparency for fitted lines
  color <- simulation_aes %>% filter(Simulation == "Natural history") %>% pull(Color)
  size <- 0.1
  alpha <- 0.05
  
  # Plot observed cases
  plot <- ggplot(data = obs_data,
                 aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.position = "bottom") +
    labs(title = title) +
    geom_path(data = filter(obs_data, Date <= date_start),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.2) +
    geom_path(data = filter(obs_data, In_range == TRUE),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_path(data = filter(obs_data, Date >= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.2) +
    geom_point(data = filter(obs_data, In_range == TRUE),
               alpha = 0.5, size = 0.5) +
    scale_x_continuous(name = ifelse(labs == TRUE, "Cumulative number of cases", ""),
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = ifelse(labs == TRUE, "Daily number of cases", ""),
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
  
  # Add fitted splines
  for (i in 1:nrow(knots)) {
    
    # Filter best knots dataframe by row i, convert to list,
    # and save to local environment
    knots_i <- knots %>% 
      filter(row_number() == i) %>%
      as.list %>%
      setNames(., tolower(names(.)))
    list2env(knots_i, envir = environment())
    
    # Define values of cumulative cases at knot dates
    knot_1 <- Get_Cases_On_Date(country = country, date = knot_date_1,
                                casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
    knot_2 <- Get_Cases_On_Date(country = country, date = knot_date_2,
                                casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
    
    # Calculate min and max values of cumulative cases in modelling period
    min_cc <- obs_data %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% min
    max_cc <- obs_data %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% max
    
    # Define slopes of spline segments
    slope_1 <- growth_factor_1 - 1
    slope_2 <- growth_factor_2 - 1
    slope_3 <- growth_factor_3 - 1
    
    # Add fitted line
    if (n_knots == 1) {
      if (n_knots_in_range == 0) {
        # Second segment only
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = max_cc,
                            y = intercept_2 + slope_2*min_cc, yend = intercept_2 + slope_2*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed")
      } else {  # (n_knots_in_range == 1)
        # First and second segments
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = max_cc,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed") 
      }
    } else {  # (n_knots == 2)
      if (n_knots_in_range == 0) {
        # Third segment only
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = max_cc,
                            y = intercept_3 + slope_3*min_cc, yend = intercept_3 + slope_3*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed")
      } else if (n_knots_in_range == 1) {
        # Second and third segments
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_2,
                            y = intercept_2 + slope_2*min_cc, yend = intercept_2 + slope_2*knot_2),
                       color = color, size = size, alpha = alpha, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                       color = color, size = size, alpha = alpha, linetype = "dashed") 
      } else {  # (n_knots_in_range == 2)
        if (knot_1 == knot_2) {
          # First and third segments
          plot <- plot +
            geom_segment(aes_(x = min_cc, xend = knot_2,
                              y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_2),
                         color = color, size = size, alpha = alpha, linetype = "dashed") +
            geom_segment(aes_(x = knot_2, xend = max_cc,
                              y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                         color = color, size = size, alpha = alpha, linetype = "dashed") 
        } else {
          # All three segments
          plot <- plot +
            geom_segment(aes_(x = min_cc, xend = knot_1,
                              y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                         color = color, size = size, alpha = alpha, linetype = "dashed") +
            geom_segment(aes_(x = knot_1, xend = knot_2,
                              y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                         color = color, size = size, alpha = alpha, linetype = "dashed") +
            geom_segment(aes_(x = knot_2, xend = max_cc,
                              y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                         color = color, size = size, alpha = alpha, linetype = "dashed") 
        }
      }
    }  # (close if-else section)
  }  # (close fitted line section)
  
  # Add simulated lines
  plot <- plot + geom_line(data = sim_data_all,
                           aes(x = Mean_cum, y = Mean_inc,
                               group = Simulation,
                               color = Simulation),
                           size = 1) +
    scale_color_manual(name = "Simulation:",
                       values = aesthetics$Color, 
                       limits = aesthetics$Simulation,
                       breaks = aesthetics$Simulation,
                       labels = str_wrap(aesthetics$Label, width = 27))
  
  # Return plot
  return(plot)
  
}
