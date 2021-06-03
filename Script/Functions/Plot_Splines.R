#' Create figure of best fitted splines for a given country.
#'
#' @param country Country
#' @param out_folder Folder to save figure in
#'
#' @return Figure of best fitted splines for \code{country}. Figure is also
#' saved to the specified \code{out_folder} with name \code{country}.
#'
#' @examples
#' Plot_Splines(country = "United Kingdom", 
#' out_folder = "./Output/Figures/Fitted splies by country")
Plot_Splines <- function(country, out_folder) {
  
  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import required files
  Import_Unloaded_CSV_Files(filenames = c("Cases_deaths_data_europe",
                                          "knots_best"), 
                            silent = TRUE)
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")

  # Get first/last dates of observed data included in spline estimation,
  # and save to environment
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_start", "Date_T")) 
  list2env(important_dates, envir = environment())
  
  # Filter dataframe of best knots by specified country
  knots_best_country <- knots_best %>% filter(Country == country)
  
  # Filter dataframe of observed cases by country, and create In_range variable
  # to indicate whether date is within range of observed data to include
  # (i.e. date_start <= Date <= date_T)
  data_country <- Cases_deaths_data_europe %>% 
    filter(Country == country) %>%
    mutate(In_range = ifelse(Date >= date_start & Date <= date_T, TRUE, FALSE))
  
  # Define total number of knot points
  n_knots <- knots_best_country %>% pull(N_knots) %>% unique
  
  # Calculate max_date (max date to display on plots)
  max_date <- date_T + 7
  
  # Define x-axis range (cumulative cases)
  x_min <- 0
  x_max <- data_country %>% filter(Date == max_date) %>% pull(Cumulative_cases_beg)
  
  # Define y-axis range (incident cases)
  y_min <- 0
  y_max <- data_country %>% filter(Date <= max_date) %>% pull(Daily_cases) %>% max
  y_max <- 1.2*y_max  # (add buffer)
  
  # Define color for fitted lines
  color <- simulation_aes %>% filter(Simulation == "0,0") %>% pull(Color)
  
  # Plot observed cases
  plot <- ggplot(data = data_country,
                 aes(x = Cumulative_cases_beg, y = Daily_cases)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = country,
         subtitle = "Cumulative vs incident cases of COVID-19") +
    geom_path(data = filter(data_country, Date <= date_start),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_path(data = filter(data_country, In_range == TRUE),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 1) +
    geom_path(data = filter(data_country, Date >= date_T),
              aes(x = Cumulative_cases_beg, y = Daily_cases), alpha = 0.5) +
    geom_point(data = filter(data_country, In_range == TRUE),
               alpha = 1, size = 1) +
    scale_x_continuous(name = "Cumulative number of cases",
                       labels = comma_format(accuracy = 1)) + 
    scale_y_continuous(name = "Daily number of cases",
                       labels = comma_format(accuracy = 1)) +
    coord_cartesian(xlim = c(x_min, x_max), ylim = c(y_min, y_max), expand = FALSE)
  
  # Add fitted splines
  for (i in 1:nrow(knots_best_country)) {
    
    # Filter best knots dataframe by row i, convert to list,
    # and save to local environment
    knots_best_country_i <- knots_best_country %>% 
      filter(row_number() == i) %>%
      as.list %>%
      setNames(., tolower(names(.)))
    list2env(knots_best_country_i, envir = environment())
    
    # Define values of cumulative cases at knot dates
    knot_1 <- Get_Cases_On_Date(country = country, date = knot_date_1,
                                casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
    knot_2 <- Get_Cases_On_Date(country = country, date = knot_date_2,
                                casetypes = "Cumulative_cases_beg") %>% unlist %>% unname
    
    # Calculate min and max values of cumulative cases in modelling period
    min_cc <- data_country %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% min
    max_cc <- data_country %>% filter(In_range == TRUE) %>% pull(Cumulative_cases_beg) %>% max
    
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
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed")
      } else {  # (n_knots_in_range == 1)
        # First and second segments
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_1,
                            y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_1, xend = max_cc,
                            y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*max_cc),
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed") 
      }
    } else {  # (n_knots == 2)
      if (n_knots_in_range == 0) {
        # Third segment only
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = max_cc,
                            y = intercept_3 + slope_3*min_cc, yend = intercept_3 + slope_3*max_cc),
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed")
      } else if (n_knots_in_range == 1) {
        # Second and third segments
        plot <- plot +
          geom_segment(aes_(x = min_cc, xend = knot_2,
                            y = intercept_2 + slope_2*min_cc, yend = intercept_2 + slope_2*knot_2),
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed") +
          geom_segment(aes_(x = knot_2, xend = max_cc,
                            y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                       color = color, size = 0.15, alpha = 0.1, linetype = "dashed") 
      } else {  # (n_knots_in_range == 2)
        if (knot_1 == knot_2) {
          # First and third segments
          plot <- plot +
            geom_segment(aes_(x = min_cc, xend = knot_2,
                              y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_2),
                         color = color, size = 0.15, alpha = 0.1, linetype = "dashed") +
            geom_segment(aes_(x = knot_2, xend = max_cc,
                              y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                         color = color, size = 0.15, alpha = 0.1, linetype = "dashed") 
        } else {
          # All three segments
          plot <- plot +
            geom_segment(aes_(x = min_cc, xend = knot_1,
                              y = intercept_1 + slope_1*min_cc, yend = intercept_1 + slope_1*knot_1),
                         color = color, size = 0.15, alpha = 0.1, linetype = "dashed") +
            geom_segment(aes_(x = knot_1, xend = knot_2,
                              y = intercept_2 + slope_2*knot_1, yend = intercept_2 + slope_2*knot_2),
                         color = color, size = 0.15, alpha = 0.1, linetype = "dashed") +
            geom_segment(aes_(x = knot_2, xend = max_cc,
                              y = intercept_3 + slope_3*knot_2, yend = intercept_3 + slope_3*max_cc),
                         color = color, size = 0.15, alpha = 0.1, linetype = "dashed") 
        }
      }
    }  # (close if-else section)
  }  # (close fitted line section)
  
  # Save plot to out_folder
  ggsave(paste0(out_folder, "/", country, ".png"), plot = plot, width = 6, height = 6)
  
  # Return plot
  return(plot)
  
}
