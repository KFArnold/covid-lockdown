#' Create figure of within-country effect of lockdown timing on total cases.
#'
#' @param effects Dataframe containing estimated effects across all simulations
#' @param max_y Maximum value of y (i.e. percentage change) to display on figure
#' (note that minimum value is set to -100)
#'
#' @return Faceted plot displaying percentage change in total cases
#' for each simulation, with median value highlighted.
#'
#' @examples
Plot_Effects_Within_Total_Cases <- function(effects, max_y) {
  
  # Filter effects dataframe by outcome
  effects_outcome <- effects %>% filter(Outcome == "Total_cases")
  
  # Create plot
  plot <- ggplot(data = effects_outcome,
                 aes(x = Threshold, 
                     y = Pct_change,
                     color = Simulation)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          ggh4x.facet.nestline = element_line(color = "gray20", size = 0.2)) +
    guides(color = FALSE) +
    geom_hline(yintercept = 0, color = "gray20", lty = "dashed") +
    labs(title = "Effect on total cases",
         y = "Percentage change compared to natural history") +
    geom_point(shape = 16, alpha = 0.6) +
    stat_summary(fun = median, shape = 18, size = 1.5) +
    EnvStats::stat_n_text(y.pos = 10, size = 3, color = "gray50") +
    facet_nested(. ~ History + Simulation,
                 nest_line = TRUE, scale = "free",
                 labeller = labeller(Simulation = 
                                       as_labeller(Simulation_Labeller,
                                                   default = label_wrap_gen(width = 20)))) +
    scale_color_manual(values = simulation_aes$Color, 
                       breaks = simulation_aes$Simulation,
                       labels = simulation_aes$Label) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_continuous(limits = c(-100, max_y))
  
  # Return plot
  return(plot)
  
}
