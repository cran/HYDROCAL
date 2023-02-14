#' Compute channel form roughness following Van Rijn (1984)
#'
#' \code{n_vanrijn1984} calculate Manning's n using the Van Rijn (1984) method for
#'      estimating roughness due to channel form
#' 
#' @param depth flow depth (H) in meters. The original model was calibrated for 0.1 < H < 16 m.
#' @param slope channel slope (S) in m/m
#' @param d50 grain size (d50) in millimeters. The original model was calibrated
#'      for 0.19 mm < d50 < 3.6 mm.
#' @param d90 grain size (d90) in millimeters.
#' @param velocity initial channel velocity estimate (U) in meters per second
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' van Rijn, L. C. 1984a. Sediment Transport, Part I: Bed Load Transport. Journal of Hydraulic Engineering. ASCE, 
#' Vol. 110 (10), pp. 1431-1456. 
#' 
#' van Rijn, L. C. 1984b. Sediment Transport, Part II: Suspended Load Transport. Journal of Hydraulic Engineering. 
#' ASCE, Vol. 110 (11), pp. 1613-1641. 
#' 
#' van Rijn, L. C. 1984c. Sediment Transport, Part III: Bed Forms and Alluvial Roughness. Journal of Hydraulic 
#' Engineering. ASCE, Vol. 110 (12), pp. 1733-1754. 
#'
#' @examples
#' # Result: Manning's n of 0.173
#' n_vanrijn1984(10,0.025,1,2,6)
#' 
#' # Result: Manning's n of 0.047
#' n_vanrijn1984(0.33,0.15,0.3,0.5,2)
#' 
#' # Result: Manning's n of 0.028
#' n_vanrijn1984(1.55,0.033,0.5,0.8,1)
#' 
#' # Result: Depth must be within 0.025 and 17 m.
#' n_vanrijn1984(0.01,0.033,0.5,0.8,1)
#'
#' @export
n_vanrijn1984 <- function(depth, slope, d50, d90, velocity, restrict = TRUE){
  # Convert from mm to m 
  d50 <- d50/1000
  d90 <- d90/1000
  
  # Compute and Error Handle for n 
  if(length(depth)==1 || length(slope)==1 || length(d50)==1 || length(d90)==1 || length(velocity)==1){
    if(restrict == FALSE){
      # Compute n
      R <- 1.65 # Submerged specific gravity, R, 
      g <- 9.81 # Gravity, g, m/s^2
      p <- 1000 # Density, p, kg/m^3
      kin_visc <- 1.005*(10**-6) # Kinematic Viscosity, v, m^2/s
      D <- d50*((R*g/(kin_visc**2))**(1/3)) #D*
      Rb <- depth # Hydraulic radius of the bed, Rb
      Cz <- 18*log10(12*Rb/(3*d90)) # Chezy's coefficient of Grains, Cz'
      u <- ((g**0.5)/Cz)*velocity # Bed Shear Velocity of Grains, u*'
      if(D <= 4)
        tau <- 0.24*(D**-1) # Critical Shield's Number, Tau*, =coeff*D**exp
      else if(D > 4 && D <= 10)
        tau <- 0.14*(D**-0.64)
      else if(D > 10 && D <= 20)
        tau <- 0.04*(D**-0.1)
      else if(D > 20 && D <= 150)
        tau <- 0.013*(D**0.29)
      else
        tau <- 0.055*D**0
      ucr <- sqrt(tau*g*R*d50) # Critical Shield's Velocity, u*cr
      t <- ((u**2)-(ucr**2))/(ucr**2) # T
      deltaH <- 0.11*((d50/depth)**0.3)*(1-exp(-0.5*t))*(25-t)
      delta <- deltaH*depth
      lambda <- 7.3*depth
      psi <- delta/lambda
      kgrain <- 3*d90 # k_(s.grain)
      kform <- 1.1*delta*(1-exp(-25*psi)) # k_(s.form)
      ktotal <- kgrain+kform # k_(s.total)
      Cz_coeff <- 18*log10(12*Rb/ktotal) # Chezy's Coefficient, Cz
      velocity_c <- Cz_coeff*sqrt(depth*slope) # Check velocity, u, (m/s)
      roughness_coefficient <- (Rb**(1/6))/Cz_coeff
    }
    else if(depth >= 16 || depth <= 0.1)
      roughness_coefficient <- "Depth must be within 0.025 and 17 m."
    else if(d50 < 0 || d90 < 0)
      roughness_coefficient <- "Grain size (m) must be positive."
    else if(d50 >= 3.6/1000 || d50 <= 0.19/1000)
      roughness_coefficient <- "The geometric standard deviation of bed material must be no more than 5."
    else if(velocity < 0)
      roughness_coefficient <- "The guess velocity cannot be negative."
    else{
      # Compute n
      R <- 1.65 # Submerged specific gravity, R
      g <- 9.81 # Gravity, g, m/s^2
      p <- 1000 # Density, p, kg/m^3
      kin_visc <- 1.005*(10^-6) # Kinematic Viscosity, v, m^2/s
      D <- d50*((R*g/(kin_visc**2))**(1/3)) #D*
      Rb <- depth # Hydraulic radius of the bed, Rb
      Cz <- 18*log10(12*Rb/(3*d90)) # Chezy's coefficient of Grains, Cz'
      u <- ((g**0.5)/Cz)*velocity # Bed Shear Velocity of Grains, u*'
      if(D <= 4)
        tau <- 0.24*(D**-1) # Critical Shield's Number, Tau*, =coeff*D**exp
      else if(D > 4 && D <= 10)
        tau <- 0.14*(D**-0.64)
      else if(D > 10 && D <= 20)
        tau <- 0.04*(D**-0.1)
      else if(D > 20 && D <= 150)
        tau <- 0.013*(D**0.29)
      else
        tau <- 0.055*(D**0)
      ucr <- sqrt(tau*g*R*d50) # Critical Shield's Velocity, u*cr
      t <- ((u**2)-(ucr**2))/(ucr**2) # T
      deltaH <- 0.11*((d50/depth)**0.3)*(1-exp(-0.5*t))*(25-t)
      delta <- deltaH*depth
      lambda <- 7.3*depth
      psi <- delta/lambda
      kgrain <- 3*d90 # k_(s.grain)
      kform <- 1.1*delta*(1-exp(-25*psi)) # k_(s.form)
      ktotal <- kgrain+kform # k_(s.total)
      Cz_coeff <- 18*log10(12*Rb/ktotal) # Chezy's Coefficient, Cz
      velocity_c <- Cz_coeff*sqrt(depth*slope) # Check velocity, u, (m/s)
      roughness_coefficient <- (Rb**(1/6))/Cz_coeff
    }
  }
  else
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
