#' Compute total channel roughness via direct field measurement
#'
#' \code{n_direct} calculate Manning's n using direct measurements 
#'
#' @param radius hydraulic radius (R) in meters
#' @param slope channel slope (S) in m/m
#' @param velocity average velocity (U) in meters per second
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE.
#'
#' @return Manning's n 
#'
#' @examples
#' # Result: Manning's n of 0.710 
#' n_direct(2,0.05,0.5)
#' 
#' # Result: Manning's n of 3.216
#' n_direct(10,0.03,0.25)
#' 
#' # Result: Manning's n of 5.440
#' n_direct(22,0.12,0.5)
#'
#' # Result: Hydraulic radius cannot be negative.
#' n_direct(-2,0.12,0.5)
#' 
#' @export
n_direct <- function(radius, slope, velocity, restrict = TRUE){
  # Compute and Error Handle for n 
  if(!radius || !slope || !velocity)
    roughness_coefficient <- "A parameter is missing."
  else if(restrict == FALSE){
    # Compute n
    roughness_coefficient <- radius**(2/3)*(slope**0.5)/velocity
  }
  else if(radius < 0)
    roughness_coefficient <- "Hydraulic radius cannot be negative."
  else if(slope < 0)
    roughness_coefficient <- "Slope cannot be negative."
  else if(velocity < 0)
    roughness_coefficient <- "Velocity cannot be negative." #not technically true. Kyle?
  else{
    # Compute n
    roughness_coefficient <- radius**(2/3)*(slope**0.5)/velocity
  }
  # Return n
  return(roughness_coefficient)
}
