#' Compute form roughness following Engelund and Hansen (1967)
#'
#' \code{n_engelund1967} calculate Manning's n using the Engelund-Hansen (1981) 
#'     method for estimating form roughness
#' 
#' @param depth flow depth (H) in meters
#' @param slope channel slope (S) in m/m
#' @param d50 grain size (d50) in millimeters
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Engelund, F., and E. Hansen. 1967. A Monograph on Sediment Transport in Alluvial Streams. 
#'      Technical University of Denmark, Copenhagen, Denmark. 
#'
#' @examples
#' # Result: Manning's n of 0.049
#' n_engelund1967(1, 0.025, 200)
#' 
#' # Result: Manning's n of 0.028
#' n_engelund1967(5, 0.08, 90)
#' 
#' # Result: Manning's n of 0.053
#' n_engelund1967(12, 0.025, 160)
#'
#' @export
n_engelund1967 <- function(depth, slope, d50, restrict = TRUE){
  # Convert from mm to m 
  d50 <- d50/1000
  
  # Compute and Error Handle for n 
  if(length(depth)==1 && length(slope)==1 && length(d50)==1){
    # Compute n
    ks <- 2.5*d50
    R <- 1.65 # Submerged specific gravity, R
    g <- 9.81 # gravity, g, m/s^2
    p <- 1000 # density, p, kg/m^3
    taub <- p*g*depth*slope 
    tau <- taub/(p*g*R*d50) # tau*
    taus <- 0.06+0.4*tau*tau # tau*s
    taubs <- taus*p*g*R*d50
    Hs <- taubs/(p*g*slope)
    Cfs <- (2.5*log(11*Hs/ks,base=exp(1)))**-2
    V <- sqrt(taubs/(p*Cfs)) # velocity
    
    roughness_coefficient <- (depth**(2/3))*(sqrt(slope)/V)
  }
  else
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
