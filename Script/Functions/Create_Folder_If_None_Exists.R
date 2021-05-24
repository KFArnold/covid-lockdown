#' Create folder in specified location if none exists
#'
#' @param folder Name of folder
#' @param path Path name (ends in "/")
#'
#' @return If folder doesn't exist, folder is created and message "Folder
#' created" is returned; else, message "Folder already exists" is returned
#'
#' @examples
#' Create_Folder_If_None_Exists(folder = "Simulations", path = "./Output/")
Create_Folder_If_None_Exists <- function(folder, path) {
  
  # Combine path and folder names into full path 
  full_path <- paste0(path, folder)
  
  # If folder doesn't exist, create it in the specified location 
  # Return message
  if(!dir.exists(full_path)) {
    
    dir.create(full_path)
    return("Folder created")
    
  } else {
    
    return("Folder already exists")
    
  }
  
}
