#' Combine list of figures into single multi-panel figure.
#'
#' Note that if \code{rows} or \code{cols} are not specified, the function will
#' calculate these; if \code{labels} is unspecified, no labels will be added.
#'
#' @param plotlist List of figures
#' @param height Desired height of each individual figure (default = 6)
#' @param width Desired width of each individual figure (default = 6)
#' @param rows Number of rows in multi-panel figure
#' @param cols Number of cols in multi-panel figure
#' @param labels List of labels to added to multi-panel figure;
#' ("AUTO" for upper-case labels, "auto" for lower-case labels; default is none)
#' @param title Title to display on multi-panel figure
#' @param title_size Size of title
#' @param out_folder Folder to save multi-panel figure to
#' @param out_name Name of file to save
#' @param return Whether the function should return the resulting figure 
#' (T/F, defalt is TRUE); if FALSE, figure will be saved to specified folder but
#' not returned.
#'
#' @return Multi-panel figure is saved in the specified location; 
#' if \code{return = TRUE}, the figure is also returned
#'
#' @examples
Plot_Combined <- function(plotlist, height = 6, width = 6, rows, cols, labels,
                          title, title_size, out_folder, out_name, return = TRUE) {
  
  # Assign number of rows and columns if not specified
  if (missing(rows) | missing(cols)) {
    if (missing(rows) & missing(cols)) {
      rows <- length(plotlist) %>% sqrt %>% ceiling
      cols <- length(plotlist) %>% sqrt %>% floor
    } else if (missing(rows)) {
      rows <- length(plotlist)/cols %>% ceiling
    } else {
      cols <- length(plotlist)/rows %>% ceiling
    }
  }
  
  # Create combined figure, with or without specified labels
  if (missing(labels)) {
    plot <- ggarrange(plotlist = plotlist, align = "hv", nrow = rows, ncol = cols)
  } else {
    plot <- ggarrange(plotlist = plotlist, align = "hv", nrow = rows, ncol = cols,
                      labels = labels)
  }
  
  # Annotate combined figure
  plot_annotated <- annotate_figure(plot, top = text_grob(title, size = title_size))
  
  # Save combined figure to out folder, if specified
  if (!missing(out_folder)) {
    
    # Create folder for saving combined figure, if it doesn't exist
    Create_Folder_If_None_Exists(folder = out_folder, silent = TRUE)
    
    # Save
    ggsave(paste0(out_folder, out_name),
           plot = plot_annotated, 
           width = width*cols, height = height*rows, 
           limitsize = FALSE)
    
  }
  
  # Return figure
  if (return == TRUE) {
    return(plot_annotated)
  }

}
