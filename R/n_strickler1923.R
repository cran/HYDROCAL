#' Compute grain roughness following Strickler (1923)
#'
#' \code{n_strickler1923} calculate Manning's n using the Strickler (1923) method
#'      for estimating grain roughness
#'
#' @param grain grain size (d50) in millimeters 
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#' 
#' @return Manning's n 
#'
#' @references
#' Strickler, A. 1923. Contributions to the Question of a Velocity formula and Roughness 
#'      Data for Streams, Channels Closed Pipelines, translated by T. Roesgan and W.R. 
#'      Brownlie. Translation T-10, W.M. Keck Lab of Hydraulics and Water Resources, 
#'      California Institute of Technology, Pasadena, CA. 
#'
#' @examples
#' # Result: Manning's n of 0.032
#' n_strickler1923(100)
#' 
#' # Result: Manning's n of 0.025
#' n_strickler1923(20)
#' 
#' # Result: Manning's n of 0.021
#' n_strickler1923(8)
#' 
#' # Result: Grain size (mm) must be positive
#' n_strickler1923(-8)
#'
#' @export
n_strickler1923 <- function(grain, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(grain)==1){
    if(restrict == FALSE){
      # Compute n
      roughness_coefficient <- (0.0474*(grain/1000)**(1/6))
    }
    else if(grain < 0)
      roughness_coefficient <- "Grain size (mm) must be positive."
    else{
      # Compute n
      roughness_coefficient <- (0.0474*(grain/1000)**(1/6))
    }
  }
  else
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
