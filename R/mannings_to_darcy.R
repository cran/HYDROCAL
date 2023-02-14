#' Convert Manning's n to Darcy-Weisbach f
#'
#' \code{mannings_to_darcy} converts Manning's n to Darcy-Weisbach f
#'
#' @param mannings Manning's n value
#' @param R hydraulic radius, R in meters (m)
#' @param restrict allows for function parameters to restrict certain values. Type boolean. Default TRUE.
#'
#' @return Darcy-Weisbach f 
#'
#' @examples
#' # Result: Darcy-Weisbach f of 0.0331
#' mannings_to_darcy(0.030, 10)
#' 
#' @export
mannings_to_darcy <- function(mannings, R, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(mannings)==1 && length(R)==1){
    if(R <= 0 && restrict == TRUE)
      conversion <- "Manning's n cannot be 0 for conversion to Chezy, Cz."
    else 
      conversion <- (8*9.91)*(mannings^2)/(R**(1/3))
  }
  else
    conversion <- "A parameter is missing."
  # Return n
  return(conversion)
}