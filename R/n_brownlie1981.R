#' Compute form roughness via Brownlie (1981)
#'
#' \code{n_brownlie1981} calculate Manning's n using the Brownlie (1981) Method 
#'      for estimating form roughness.
#' 
#' @param depth flow depth (H) in meters (m). The original model was calibrated
#'      for 0.025 m < H < 17 m.
#' @param slope channel slope (S) in (m/m). The original model was calibrated
#'      for 3*10^-6 < S < 0.037.
#' @param d16 grain size (d16) in millimeters. 
#' @param d50 grain size (d50) in millimeters. The original model was calibrated
#'     for 0.088 mm < d50 < 2.8 mm.
#' @param d84 grain size (d84) in millimeters.
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Brownlie, W. R. 1981. Prediction of flow depth and sediment discharge in open 
#'      channels. Report No. KH-R-43A. W.M. Keck Laboratory of Hydraulics and 
#'      Water Resources. California Institute of Technology. 
#'
#' @examples
#' # Result: Manning's n of 0.022
#' n_brownlie1981(10,0.02,1,1.1,1.2)
#' 
#' # Result: Manning's n of 0.018
#' n_brownlie1981(2.5,0.01,0.1,0.2,0.5)
#' 
#' # Result: Manning's n of 0.045
#' n_brownlie1981(15,0.003,0.6,0.9,1)
#' 
#' # Result: Depth must be within 0.025 and 17 m.
#' n_brownlie1981(20,0.003,0.6,0.9,1)
#'
#' @export
n_brownlie1981 <- function(depth, slope, d16, d50, d84, restrict = TRUE){
  # Convert from mm to m 
  d16 <- d16/1000
  d50 <- d50/1000
  d84 <- d84/1000
  
  # Compute and Error Handle for n 
  if(length(depth)==1 && length(slope)==1 && length(d16)==1 && length(d50)==1 && length(d84)==1){
    if(restrict == FALSE){
      # Compute n
      sigma <- sqrt(d84/d16) # Geometric standard deviation of bed material, sigma_g
      R <- 1.65 # Submerged specific gravity, R
      g <- 9.81 # gravity, g, m/s^2
      kin_visc <- 1.005*(10**-6) # Kinematic Viscosity, v, m^2/s
      fg <- sqrt(kin_visc/(R*g*d50)) # F_g
      fg_prime <- 1.74*slope**(-1/3) # F_g'
      w <- 0
      x <- 0
      y <- 0
      z <- 0
      if(fg > fg_prime || slope >= 0.006){
        w = 0.2836
        x = 0.6248
        y = -0.2877
        z = 0.08013
      }
      else{
        w = 0.3724
        x = 0.6539
        y = -0.2542
        z = 0.105
      }
      roughness_coefficient <- ((depth**(5/3))*sqrt(slope))/((sqrt(g*(d50**3)))*((depth/(w*d50))*(slope**-y)*(sigma**-z))**(1/x))
    }
    else if(depth >= 17 || depth <= 0.025)
      roughness_coefficient <- "Depth must be within 0.025 and 17 m."
    else if(slope >= 0.037 || slope <= 3*10**-6)
      roughness_coefficient <- "Slope must be within 3*10^-6 and 0.037."
    else if(d16 < 0 || d50 < 0 || d84 < 0)
      roughness_coefficient <- "Grain size (m) must be positive."
    else if(sqrt(d84/d16) > 5)
      roughness_coefficient <- "The geometric standard deviation of bed material must be no more than 5."
    else{
      # Compute n
      sigma <- sqrt(d84/d16)
      R <- 1.65
      g <- 9.81
      kin_visc <- 1.005*(10**-6)
      fg <- sqrt(kin_visc/(R*g*d50))
      fg_prime <- 1.74*slope**(-1/3)
      w <- 0
      x <- 0
      y <- 0
      z <- 0
      if(fg > fg_prime || slope >= 0.006){
        w = 0.2836
        x = 0.6248
        y = -0.2877
        z = 0.08013
      }
      else{
        w = 0.3724
        x = 0.6539
        y = -0.2542
        z = 0.105
      }
      roughness_coefficient <- ((depth**(5/3))*sqrt(slope))/((sqrt(g*(d50**3)))*((depth/(w*d50))*(slope**-y)*(sigma**-z))**(1/x))
    }
  }
  else 
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
