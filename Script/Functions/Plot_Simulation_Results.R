#' Create figures of simulation results for a specified country.
#' 
#' This function overlays observed data with specified simulation summaries.
#'
#' @param country Country
#' @param simulations Vector containing simulations to include
#' @param out_folder Folder to save figure in
#'
#' @return Named list of 5 objects: 
#' (1) "plots_all_annotated": combined 3-panel figure (incident, cumulative,
#' and cumulative vs incident), with \code{country} as title; 
#' (2) "plots_two_annotated": combined 2-panel figure (incident and cumulative),
#' two-panel figure (incident cases and cumulative cases only);
#' (3) "plot_inc": incident cases, with \code{country} as title;
#' (4) "plot_cum": cumulative cases, with \code{country} as title'; and
#' (5) "plot_exp": cumulative vs incident cases, with \code{country} as title.
#'
#' @examples
Plot_Simulation_Results <- function(country, simulations, out_folder) {

  # Create specified folder to save figures in, if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Import formatted data
  data_formatted <- Format_Data_For_Plotting(filenames = c("thresholds_eur",
                                                           "summary_daily_cases_sim_all",
                                                           "summary_cumulative_cases_end_sim_all"))
  if (is.list(data_formatted)) {
    list2env(data_formatted, envir = environment())
  }
  
  # Import aesthetic specifications
  source("./Script/figure_aesthetics.R")

  # Filter dataframes of best knots and thresholds by specified country
  knots_country <- knots_best %>% filter(Country == country)
  thresholds_country <- thresholds_eur_formatted %>% filter(Country == country)
  
  # Get first/last dates of observed data included in spline estimation
  # and other important dates, and save to environment
  important_dates <- Get_Dates(country = country,
                               dates = c("Date_start", "Date_T",
                                         "Date_1", "Date_first_restriction",
                                         "Date_lockdown")) %>%
    setNames(., tolower(names(.)))
  list2env(important_dates, envir = environment())
  
  # If country did not enter lockdown or entered lockdown immediately, retain only unique simulations
  # (i.e. where date of first restriction is different)
  if (is.na(date_lockdown) | date_first_restriction == date_lockdown) {
    df <- data.frame(Simulations = simulations)
    df <- df %>% separate(Simulations, c("N_days_first_restriction", "N_days_lockdown"), sep = ",") %>%
      arrange(N_days_first_restriction, N_days_lockdown) %>%
      group_by(N_days_first_restriction) %>% slice(1) %>%
      mutate(Simulations = paste(N_days_first_restriction, N_days_lockdown, sep = ",")) 
    simulations <- df %>% pull(Simulations)
  }
  
  # Filter dataframe of observed cases by country, and create In_range variable
  # to indicate whether date is within range of observed data to include
  # (i.e. date_start <= Date <= date_t)
  data_country <- Cases_deaths_data_europe %>% 
    filter(Country == country) %>%
    mutate(In_range = ifelse(Date >= date_start & Date <= date_t, TRUE, FALSE))
  
  # Filter aesthetic specifications by specified simulations
  simulation_aes_filt <- simulation_aes %>% filter(Simulation %in% simulations)
  
  # Filter simulation results by specified country and simulations
  inc_cases_sim_country <- summary_daily_cases_sim_all_formatted %>% 
    filter(Country == country, Simulation %in% simulations) %>% droplevels
  cum_cases_sim_country <- summary_cumulative_cases_end_sim_all_formatted %>% 
    filter(Country == country, Simulation %in% simulations) %>% droplevels
  
  # If no simulated data for country, print warning and stop
  if (nrow(inc_cases_sim_country) == 0) { 
    stop(paste0("No simulated data for ", country, "."))
  }
  
  # Create key to map generic simulation description onto simulation actually implemented
  # and define actual simulations implemented
  simulations_actual_key <- inc_cases_sim_country %>% 
    select(History, Simulation, History, N_days_first_restriction, N_days_lockdown) %>% 
    unique %>% 
    mutate(Simulation_actual = paste(N_days_first_restriction, N_days_lockdown, sep = ","),
           Simulation_actual = factor(Simulation_actual, levels = unique(Simulation_actual))) %>%
    select(-contains("N_days")) 
  simulations_actual <- simulations_actual_key %>% pull(Simulation_actual)
  
  # Edit simulation descriptions in summary dataframes to reflect intervention actually implemented
  inc_cases_sim_country <- inc_cases_sim_country %>%
    full_join(., simulations_actual_key, by = c("History", "Simulation")) %>%
    mutate(Simulation = Simulation_actual) %>%
    select(-Simulation_actual) 
  cum_cases_sim_country <- cum_cases_sim_country %>%
    full_join(., simulations_actual_key, by = c("History", "Simulation")) %>%
    mutate(Simulation = Simulation_actual) %>%
    select(-Simulation_actual)
  
  # Modify simulation aesthetics to reflect actual interventions implemented in country
  simulation_aes_actual <- simulations_actual_key %>% 
    left_join(., simulation_aes_filt, by = "Simulation") %>%
    select(-Simulation) %>%
    rename(Simulation = Simulation_actual)
  
  # Calculate min and max dates to display on plots
  min_date <- date_1 - 14
  max_date <- date_t + 21
  
  # Create plots for triple panel figure
  plot_inc <- Plot_Daily_Cases_Sim(country = country,
                                   title = "Incident cases of COVID-19",
                                   min_date = min_date, 
                                   max_date = max_date,
                                   obs_data = data_country,
                                   sim_data = inc_cases_sim_country,
                                   threshold_data = thresholds_country,
                                   simulations = simulations_actual,
                                   aesthetics = simulation_aes_actual)
  plot_cum <- Plot_Cumulative_Cases_Sim(country = country, 
                                        title = "Cumulative cases of COVID-19",
                                        min_date = min_date, 
                                        max_date = max_date,
                                        obs_data = data_country,
                                        sim_data = cum_cases_sim_country,
                                        simulations = simulations_actual,
                                        aesthetics = simulation_aes_actual,
                                        date_T = date_t, 
                                        print_cases = TRUE)
  plot_exp <- Plot_Exponential_Growth_Sim(country = country,
                                          title = "Cumulative vs incident cases of COVID-19",
                                          labs = TRUE,
                                          max_date = max_date,
                                          obs_data = data_country,
                                          sim_data_inc = inc_cases_sim_country,
                                          sim_data_cum = cum_cases_sim_country,
                                          simulations = simulations_actual,
                                          aesthetics = simulation_aes_actual,
                                          knots = knots_country, 
                                          date_start = date_start, 
                                          date_T = date_t)
  
  # Combine in triple panel with common legend and country as title
  plots_all <- ggarrange(plotlist = list(plot_inc, plot_cum, plot_exp), align = "h",
                         common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 3)
  plots_all_annotated <- annotate_figure(plots_all, top = text_grob(paste0(country), size = 20),
                                         bottom = text_grob("Data from https://github.com/CSSEGISandData/COVID-19", size = 8))
  
  # Save combined plot to subfolder
  ggsave(paste0(out_folder, "/", country, ".png"), plot = plots_all_annotated, width = 6*3, height = 6)
  
  # Combine only incident and cumulative cases in double panel with common legend 
  # and country as title
  plots_two <- ggarrange(plotlist = list(plot_inc, plot_cum), align = "h",
                         common.legend = TRUE, legend = "bottom", nrow = 1, ncol = 2)
  plots_two_annotated <- annotate_figure(plots_two, top = text_grob(paste0(country),  size = 20),
                                         bottom = text_grob(" ", size = 20))
  
  # Create figure for each of incident, cumulative, and incident vs cumulative cases separately,
  # with country as title and no printing of cases on date_T
  plot_inc <- Plot_Daily_Cases_Sim(country = country,
                                   title = country,
                                   labs = FALSE,
                                   min_date = min_date, 
                                   max_date = max_date,
                                   obs_data = data_country,
                                   sim_data = inc_cases_sim_country,
                                   threshold_data = thresholds_country,
                                   simulations = simulations_actual,
                                   aesthetics = simulation_aes_actual)
  plot_cum <- Plot_Cumulative_Cases_Sim(country = country, 
                                        title = country,
                                        labs = FALSE,
                                        min_date = min_date, 
                                        max_date = max_date,
                                        obs_data = data_country,
                                        sim_data = cum_cases_sim_country,
                                        simulations = simulations_actual,
                                        aesthetics = simulation_aes_actual,
                                        date_T = date_t, 
                                        print_cases = FALSE)
  plot_exp <- Plot_Exponential_Growth_Sim(country = country,
                                          title = country,
                                          labs = FALSE,
                                          max_date = max_date,
                                          obs_data = data_country,
                                          sim_data_inc = inc_cases_sim_country,
                                          sim_data_cum = cum_cases_sim_country,
                                          simulations = simulations_actual,
                                          aesthetics = simulation_aes_actual,
                                          knots = knots_country, 
                                          date_start = date_start, 
                                          date_T = date_t)
  
  # Return list combined plots
  return(list(plots_all_annotated = plots_all_annotated,
              plots_two_annotated = plots_two_annotated,
              plot_inc = plot_inc,
              plot_cum = plot_cum,
              plot_exp = plot_exp))
  
}
