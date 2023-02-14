#' Compute grain roughness following Limerinos (1970)
#'
#' \code{n_limerinos1970} calculate Manning's n using the Limerinos (1970) method
#'      for estimating grain roughness
#'
#' @param radius hydraulic radius (R) in meters. The original model was calibrated
#'      for 0.31 m < R < 3.32 m.
#' @param grain grain size (d84) in millimeters. The original model was calibrated
#'      for 19 mm < d84 < 747 mm
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Limerinos, J. T. 1970. Determination of the Manning Coefficient from Measured 
#'      Bed Roughness in Natural Channels. Water Supply Paper 1898-B. USGS, Washington, DC. 
#'
#' @examples
#' # Result: Manning's n of 0.036
#' n_limerinos1970(1,100)
#' 
#' # Result: Manning's n of 0.031
#' n_limerinos1970(2.5,70)
#' 
#' # Result: Manning's n of 0.039
#' n_limerinos1970(3,200)
#' 
#' # Result: Manning's n of 0.039
#' n_limerinos1970(3,200)
#' 
#' # Result: Grain must be within 19 and 747 mm.
#' n_limerinos1970(3,1000)
#'
#' @export
n_limerinos1970 <- function(radius, grain, restrict = TRUE){
  # Compute and Error Handle for n 
  if(restrict == FALSE){
    # Compute n
    roughness_coefficient <- (0.1129*(radius**(1/6)))/(1.16+2*log10(radius/(grain/1000)))
  }
  else if(!radius || !grain)
    roughness_coefficient <- "A parameter is missing."
  else if(radius >= 3.32 || radius <= 0.31)
    roughness_coefficient <- "Hydraulic radius must be within 0.31 and 3.32 m."
  else if(grain >= 747 || grain <= 19)
    roughness_coefficient <- "Grain must be within 19 and 747 mm."
  else{
    # Compute n
    roughness_coefficient <- (0.1129*(radius**(1/6)))/(1.16+2*log10(radius/(grain/1000)))
  }
  # Return n
  return(roughness_coefficient)
}
