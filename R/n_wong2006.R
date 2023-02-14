#' Compute grain roughness following Wong and Parker (2006)
#'
#' \code{n_wong2006} calculate Manning's n using the Wong and Parker (2006) method
#'      for estimating grain roughness
#'
#' @param grain grain size (d90) in millimeters. The original model was calibrated
#'      for 0.38 mm < d90 < 28.65 mm
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Wong, M., and G. Parker. 2006. Reanalysis and Correction of Bed-load Relation 
#'      of Meyer-Peter and Muller Using Their Own Database. Journal of Hydraulic 
#'      Engineering. American Society of Civil Engineers, Vol. 132 (11), pp. 1159-1168. 
#'
#' @examples
#' # Result: Manning's n of 0.022
#' n_wong2006(20)
#' 
#' # Result: Manning's n of 0.013
#' n_wong2006(0.82)
#' 
#' # Result: Manning's n of 0.021
#' n_wong2006(12)
#' 
#' # Result: Grain must be within 0.38 and 28.65 mm.
#' n_wong2006(30)
#'
#' @export
n_wong2006 <- function(grain, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(grain)==1){
    if(restrict == FALSE){
      # Compute n
      roughness_coefficient <- (0.0431*(grain/1000)**(1/6))
    }
    else if(grain >= 28.65 || grain <= 0.38)
      roughness_coefficient <- "Grain must be within 0.38 and 28.65 mm."
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
