#' Import unloaded .csv files into the global environment.
#' 
#' This function checks which of the specified \code{filenames} do not already
#' exist in the global environment, and imports these. Note that files may 
#' be located anywhere within the project directory.
#'
#' @param filenames List of filenames (without ".csv")
#' @param silent Whether to return message describing files loaded into
#' global environment (T/F, default is FALSE)
#'
#' @return If \code{verbose = TRUE}, prints message describing which of 
#' \code{filenames} were loaded into the global environment
#'
#' @examples
#' Import_Unloaded_CSV_Files(filenames = list("knots_best", "summary_eur"),
#' silent = FALSE)
Import_Unloaded_CSV_Files <- function(filenames, silent = FALSE) {
  
  # Determine which files are not already loaded in global environment
  filenames_missing <- setdiff(filenames, ls(.GlobalEnv)) 
  
  # Import missing flies into the global environment
  dataframes <- filenames_missing %>%
    map(., .f = ~list.files(path = ".",
                            recursive = TRUE, 
                            pattern = paste0(., ".csv"),
                            full.names = TRUE)) %>%
    map(., .f = ~read_csv(.)) %>%
    set_names(filenames_missing)
  list2env(dataframes, .GlobalEnv)
  
  # If specified, print names of files imported into global environment
  if (silent == FALSE) {
    if (length(filenames_missing) == 0) {
      cat("All files already exist in the global environment.")
    } else {
      cat("Files imported to global environment:", 
          paste(filenames_missing, collapse = ", "), sep = "\n")
    }
  }
  
}
