#' Create figure of between-country effects of lockdown timing on length of lockdown.
#'
#' @param effects Dataframe containing estimated between-country effects 
#'
#' @return Faceted plot displaying effect sizes of daily/cumulative cases on the
#' date of lockdown on length of lockdown, both adjusted and unadjusted, and 
#' with/without points of high leverage included. 
#'
#' @examples 
Plot_Effects_Between_Length_Lockdown <- function(effects) {
  
  # Filter dataframe with effect sizes by relevant outcome and threshold
  effects_length_lockdown <- effects %>% 
    filter(Outcome == "Length_lockdown")
  
  # Calculate maximum deviation from zero across all confidence intervals
  max_y <- effects_length_lockdown %>% pull(CI_lower, CI_upper) %>% abs %>% max 
  
  # Create plot
  plot <- ggplot(data = effects_length_lockdown,
                 aes(x = Exposure, y = Effect, 
                     color = Adjusted, shape = Adjusted)) +
    theme_light() +
    theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
          plot.title = element_text(size = 10),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          legend.position = "bottom",
          panel.background = element_rect(fill = "gray90"),
          panel.grid.major = element_line(color = "white"),
          strip.text = element_text(color = "gray20"),
          ggh4x.facet.nestline = element_line(color = "gray20", size = 0.2)) +
    geom_hline(yintercept = 0, color = "gray40", lty = "dashed") +
    labs(title = "Effect on length of lockdown",
         color = "", shape = "") +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = CI_lower, ymax = CI_upper), alpha = 0.4, width = 0.2) +
    facet_nested(. ~ Exposure + Leverage_points,
                 scale = "free", nest_line = TRUE,
                 labeller = labeller(Exposure = exposure_labels,
                                     Leverage_points = leverage_labels)) +
    scale_color_manual(values = adjustment_aes$Color,
                       breaks = adjustment_aes$Effect) +
    scale_shape_manual(values = adjustment_aes$Shape,
                       breaks = adjustment_aes$Effect) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
    scale_y_continuous(limits = 1.05*c(-max_y, max_y))
  
  # Return plot
  return(plot)
  
}
