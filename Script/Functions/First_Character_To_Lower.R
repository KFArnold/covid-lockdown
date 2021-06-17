#' Convert first character of string to lowercase.
#'
#' @param string Vector of strings
#'
#' @return Vector of strings equal to \code{string}, except with the first 
#' element of each string as lowercase
#'
#' @examples
#' First_Character_To_Lower(string = c("Woman", "Cat", "park"))
First_Character_To_Lower <- function(string) {
  
  # Record first character of string(s)
  first_character <- substr(x = string, 
                            start = 1, stop = 1)
  
  # Convert first character to lowercase
  first_character_lower <- tolower(first_character)
  
  # Create new string(s) with lowercase first character
  new_string <- string %>% map2(., .y = first_character_lower,
                  .f = ~sub(pattern = "^.", 
                            replacement = .y,
                            x = .x)) %>% unlist
  
  # Return string
  return(new_string)
  
}
