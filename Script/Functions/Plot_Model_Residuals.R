#' Create figure of simulated model residuals (with resepect to either incident
#' or cumulative cases).
#'
#' @param country Country
#' @param cases Type of cases (one of c("Daily_cases", "Cumulative_cases_beg"))
#'
#' @return Figure displaying residuals (observed \code{cases} minus
#' simulated \code{cases}) over time, with title = \code{country}.
#'
#' @examples
#' Plot_Model_Residuals(country = "United Kingdom", cases = "Daily_cases")
Plot_Model_Residuals <- function(country, cases) {

  # Get important dates in designated country (date of lockdown and simulation start date),
  # and save to environment
  important_dates <- Get_Dates(country = country,
            dates = c("Date_start", "Date_T"))
  list2env(important_dates, envir = environment())
  
  # Filter observed cases/deaths dataframe by country and modelling period; 
  # select relevant variables
  data_obs <- Cases_deaths_data_europe %>% 
    filter(Country == country,
           Date >= date_start, Date <= date_T) %>%
    select(Country, Date, Observed_cases = all_of(cases))
  
  # Import relevant simulated data
  if (cases == "Daily_cases") {
    Import_All_Simulated_Data(filenames = "summary_daily_cases_sim",
                              silent = TRUE)
    data_sim <- summary_daily_cases_sim_all 
  } else {
    Import_All_Simulated_Data(filenames = "summary_cumulative_cases_end_sim",
                              silent = TRUE)
    data_sim <- summary_cumulative_cases_end_sim_all 
  }
  
  # Filter simulated data by specified country, modelling period, and 
  # natural history; select relevant variables
  data_sim <- data_sim %>%
    filter(Country == country, History == "Natural history",
           Date >= date_start, Date <= date_T) %>%
    select(Country, Date, Predicted_cases = Mean)
  
  # If no simulated data for country, print warning and stop
  if (nrow(data_sim) == 0) { 
    stop(paste0("No simulated data for ", country, "."))
  }
  
  # Join observed and simulated data, calculate residuals
  residuals <- full_join(data_obs, data_sim, 
                         by = c("Date", "Country")) %>%
    mutate(Residual = Observed_cases - Predicted_cases)
  
  # Calculate absolute maximum value of residuals
  max_res <- residuals %>% pull(Residual) %>% abs %>% max
  
  # Plot residuals against date
  plot <- ggplot(data = residuals, aes(x = Date, y = Residual)) +
    theme_minimal() +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
    labs(title = country) +
    geom_hline(yintercept = 0, color = "firebrick") +
    geom_line() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b\n%y") +
    scale_y_continuous(name = "Residual",
                       limits = c(-1.2*max_res, 1.2*max_res), 
                       labels = comma_format(accuracy = 1))
  
  # Return plot
  return(plot)
  
}
