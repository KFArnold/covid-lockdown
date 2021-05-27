#' Create folder in specified location if none exists.
#'
#' If \code{path} is not specified, the function assumes the path is included
#' in \code{folder} and creates a folder at this location.
#'
#' @param folder Name of folder
#' @param path Path name (ends in "/")
#' #' @param silent Whether to return message describing files loaded into
#' global environment (T/F, default is FALSE)
#'
#' @return If folder doesn't exist, folder is created and message "Folder
#' created" is returned; else, message "Folder already exists" is returned
#'
#' @examples
#' Create_Folder_If_None_Exists(folder = "Simulations", path = "./Output/")
#' Create_Folder_If_None_Exists(folder = "./Output/Simulations", silent = TRUE)
Create_Folder_If_None_Exists <- function(folder, path, silent = FALSE) {
  
  # Specify full path name
  if (missing(path)) {
    full_path <- folder
  } else {
    full_path <- paste0(path, folder)
  }
  
  # If folder doesn't exist, create it in the specified location 
  # Return message if silent = FALSE
  if(!dir.exists(full_path)) {
    
    dir.create(full_path)
    if (silent == FALSE) {
      cat("Folder created")
    }
    
  } else {
    
    if (silent == FALSE) {
      cat("Folder already exists")
    }

  }
  
}
