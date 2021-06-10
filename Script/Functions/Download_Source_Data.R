#' Download data from an online repository (e.g. Github).
#'
#' @param source Web address of repository where data is located
#' @param filenames Vector of filenames to download
#' @param out_folder Where to save downloaded files
#'
#' @return Specified \code{filenames} are saved as .csv files in \code{out_folder}.
#' Note that the data is not modified at all, except for any slashes and spaces 
#' in variable names being replaced by underscores.
#'
#' @examples
#' Download_Source_Data(source = "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/",
#' filenames = "OxCGRT_latest.csv", out_folder = ".Data/Unformatted/")
Download_Source_Data <- function(source, filenames, out_folder) {
  
  # Create specified folder for saving data if it doesn't already exist
  Create_Folder_If_None_Exists(folder = out_folder,
                               silent = TRUE)
  
  # Download data files and save to repository Data folder
  for (i in filenames) {
    
    # Define filename
    filenames_i <- i  
    
    # Download file
    data <- read_csv(url(paste0(source, filenames_i)),
                     col_types = cols(.default = col_character())) %>%
      type_convert
    
    # Replace slashes and spaces with underscores in variable names
    names(data) <- str_replace_all(names(data), c("/" = "_", " " = "_"))
    
    # Save .csv file to specified folder
    write_csv(x = data,
              file = paste0(out_folder, filenames_i))  
    
  }
  
  # Do not return anything
  return(invisible(NULL))
  
}
