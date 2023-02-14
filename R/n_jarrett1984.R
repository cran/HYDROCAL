#' Compute grain roughness via Jarrett (1984)
#'
#' \code{n_jarrett1984} calculate Manning's n using the Jarrett (1984) Method for
#'     estimating grain roughness
#'
#' @param radius hydraulic radius (R) in meters. The original model was calibrated for
#'      0.15 m < R < 1.68 m
#' @param slope channel slope (S) in m/m. The original model was calibrated for
#'      0.002 < S < 0.04.
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Jarrett, R. D. 1984. Hydraulics of High-Gradient Streams. Journal of Hydraulic 
#'      Engineering. American Society of Civil Engineers, Vol. 110 (11), pp. 1519-1539. 
#'
#' @examples
#' # Result: Manning's n of 0.102
#' n_jarrett1984(0.3,0.03)
#' 
#' # Result: Manning's n of 0.065
#' n_jarrett1984(1,0.015)
#' 
#' # Result: Slope must be within 0.002 and 0.04 m.
#' n_jarrett1984(12,0.05)
#'
#' @export
n_jarrett1984 <- function(radius, slope, restrict = TRUE){
  # Compute and Error Handle for n 
  if(restrict == FALSE){
    # Compute n
    roughness_coefficient <- 0.32*(slope**0.38)*(radius**-0.16)
  }
  else if(!radius || !slope)
    roughness_coefficient <- "A parameter is missing."
  else if(radius >= 1.68 || radius <= 0.15)
    roughness_coefficient <- "Hydraulic radius must be within 0.15 and 1.68 m."
  else if(slope >= 0.04 || slope <= 0.002)
    roughness_coefficient <- "Slope must be within 0.002 and 0.04 m."
  else{
    # Compute n
    roughness_coefficient <- 0.32*(slope**0.38)*(radius**-0.16)
  }
  # Return n
  return(roughness_coefficient)
}
