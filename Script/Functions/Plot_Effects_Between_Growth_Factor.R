#' Create figure of between-country effects of lockdown timing on median growth
#' factor under lockdown.
#'
#' @param effects Dataframe containing estimated between-country effects 
#'
#' @return Faceted plot displaying effect sizes of daily/cumulative cases on the
#' date of lockdown on median growth factor under lockdown, both adjusted and 
#' unadjusted, and with/without points of high leverage included.
#'
#' @examples
Plot_Effects_Between_Growth_Factor <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome
  effects_growth_factor <- effects %>% 
    filter(Outcome == "Median_growth_factor_lockdown")
  
  # Calculate maximum deviation from zero across all confidence intervals
  max_y <- effects_growth_factor %>% select(CI_lower, CI_upper) %>% abs %>% max 
  
  # Create plot
  plot <- ggplot(data = effects_growth_factor,
                 aes(x = Analysis, y = Effect, 
                     color = Analysis, shape = Analysis)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom", 
          panel.background = element_rect(fill = "gray95"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          ggh4x.facet.nestline = element_line(color = "gray20", size = 0.2)) +
    geom_hline(yintercept = 0, color = "gray40", lty = "dashed") +
    labs(title = "Effect on growth factor under lockdown",
         color = "", shape = "") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, width = 0.2) +
    facet_nested(. ~ Exposure + Leverage_points,
                 scale = "free", nest_line = TRUE,
                 labeller = labeller(Exposure = exposure_labels,
                                     Leverage_points = leverage_labels)) +
    scale_x_discrete(labels = exposure_labels) +
    scale_y_continuous(limits = 1.05*c(-max_y, max_y),
                       labels = comma) +
    scale_color_manual(values = analysis_aes$Color,
                       breaks = analysis_aes$Analysis) +
    scale_shape_manual(values = analysis_aes$Shape,
                       breaks = analysis_aes$Analysis)
  
  # Return plot
  return(plot)
  
}
