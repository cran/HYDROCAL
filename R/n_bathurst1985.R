#' Compute grain roughness via Bathurst (1985)
#'
#' \code{n_bathurst1985} calculate Manning's n using the Bathurst (1985) method 
#'      for estimating grain roughness
#'
#' @param depth flow depth (H) in meters. The original model was calibrated 
#'      for 0.102 m < H < 1.60 m.
#' @param grain grain size (d84) in millimeters. The original model was calibrated
#'      for 113 mm < d84 < 740 mm.
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Bathurst, J. C. 1985. Flow Resistance Estimation in Mountain Rivers. Journal 
#'      of Hydraulic Engineering. American Society of Civil Engineers, Vol. 111 (4), pp. 625-643. 
#'
#' @examples
#' # Result: Manning's n of 0.085
#' n_bathurst1985(0.15,250)
#' 
#' # Result: Manning's n of 0.036
#' n_bathurst1985(0.8,120)
#' 
#' # Result: Manning's n of 0.056
#' n_bathurst1985(1.32,600)
#' 
#' # Result: Grain must be within 113 and 740 mm.
#' n_bathurst1985(1.32,50)
#'
#' @export
n_bathurst1985 <- function(depth, grain, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(depth)==1 && length(grain)==1){
    if(restrict == FALSE){
      # Compute n
      roughness_coefficient <- (0.3194*(depth**(1/6)))/(4+5.62*log10(depth/(grain/1000)))
    }
    else if(depth >= 1.6 || depth <= 0.102)
      roughness_coefficient <- "Hydraulic depth must be within 0.102 and 1.6 m."
    else if(grain >= 740 || grain <= 113)
      roughness_coefficient <- "Grain must be within 113 and 740 mm."
    else{
      # Compute n
      roughness_coefficient <- (0.3194*(depth**(1/6)))/(4+5.62*log10(depth/(grain/1000)))
    }
  }
  else 
    roughness_coefficient <- "A parameter is missing."
  
  # Return n
  return(roughness_coefficient)
}
