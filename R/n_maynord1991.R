#' Compute grain roughness following Maynord (1991)
#'
#' \code{n_maynord1991} calculate Manning's n using the Maynord (1991) method of
#'      estimating grain roughness
#'
#' @param grain grain size (d90) in millimeters. The original model was calibrated
#'      for 4.57 mm < d90 < 134 mm.
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Maynord, S. T. 1991. Flow Resistance of Riprap. Journal of Hydraulic Engineering. 
#'      American Society of Civil Engineers, Vol. 117 (6), pp. 687-696. 
#'
#' @examples
#' # Result: Manning's n of 0.030
#' n_maynord1991(100)
#' 
#' # Result: Manning's n of 0.018
#' n_maynord1991(5)
#' 
#' # Result: Manning's n of 0.027
#' n_maynord1991(50)
#' 
#' # Result: Manning's n of Grain must be within 4.57 and 134 mm.
#' n_maynord1991(1)
#'
#' @export
n_maynord1991 <- function(grain, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(grain)==1){
    if(restrict == FALSE){
      # Compute n
      roughness_coefficient <- (0.0431*(grain/1000)**(1/6))
    }
    else if(grain >= 134 || grain <= 4.57)
      roughness_coefficient <- "Grain must be within 4.57 and 134 mm."
    else{
      # Compute n
      roughness_coefficient <- (0.0431*(grain/1000)**(1/6))
    }
  }
  else
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
