#' Compute hydraulic roughness due to vegetation following Fischenich (2000)
#'
#' \code{n_fischenich2000} calculate Manning's n using the Fischenich (2000) method 
#'      for estimating vegetative roughness
#' 
#' @param depth flow depth (H) in meters. Assumes wide channel geometry where depth
#'      is approximately equal to hydraulic radius.
#' @param hp vegetation height (h_p) in meters. Vegetation is emergent.
#' @param seperate Allows user to choose whether to use separate (Cd and Ad) or 
#'      combined (CdAd). Type boolean. Default TRUE.
#' @param Cd stand drag coefficient (C_d), default 0
#' @param Ad vegetation area based on density (A_d), default 0
#' @param CdAd Combined Cd and Ad values, default 0
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Fischenich, J. C. 2000. Resistance due to Vegetation. ERDC TN-EMRRP-SR-07, 
#'      U.S. Army Engineer Research and Development Center, Vicksburg, Mississippi. 
#' 
#' Fischenich, J. C., and S. Dudley. 2000. Determining Drag Coefficients and Area 
#'      for Vegetation. ERDC TNEMRRP-SR-08, U.S. Army Engineer Research and Development 
#'      Center, Vicksburg, Mississippi. 
#'
#' @examples
#' # Result: Manning's n of 0.100
#' n_fischenich2000(6,2,TRUE,0.955,0.755)
#' 
#' # Result: Manning's n of 0.059
#' n_fischenich2000(6,2,FALSE,CdAd=0.0199)
#' 
#' # Result: Manning's n of 0.090
#' n_fischenich2000(3,1,TRUE,0.1806,0.1662)
#' 
#' # Result: Depth must be positive.
#' n_fischenich2000(-1,1,TRUE,0.1806,0.1662)
#'
#' @export
n_fischenich2000 <- function(depth, hp, seperate = TRUE, Cd = 0, Ad = 0, CdAd = 0, restrict = TRUE){
  
  # Compute and Error Handle for n 
  if(length(depth)==1 || length(hp)==1){
    if(restrict == FALSE){
      # Compute n 
      if(seperate == TRUE) # Calculate implemented CdAd
        CdAd_imp = Cd * Ad
      else
        CdAd_imp = CdAd
      # Calculate Submerged Vegetation (H>hp)
      X = 1.26*(hp**2)*(2*hp/(11*CdAd_imp))*(1-exp(-5.5*CdAd_imp))
      K = 0.13*exp(-((CdAd_imp-0.4)**2))
      Y = (depth-0.95*hp)*(log((depth/(K*hp))-(0.95/K),base=exp(1))-1)-(0.05*hp)*(log(0.05/K,base=exp(1))-1)
      U = (2.5/depth)*(X+Y) # U/u*
      n_submerged <- (depth**(1/6))/(U*sqrt(9.81))
      # Calculate Submerged Vegetation (H<hp)
      n_emergent <- depth**(2/3)*sqrt((CdAd_imp)/(2*9.81))
      #Calculate n 
      if(depth > hp)
        roughness_coefficient <- n_submerged
      else
        roughness_coefficient <- n_emergent
    }
    else if(depth < 0)
      roughness_coefficient <- "Depth must be positive."
    else if(hp < 0)
      roughness_coefficient <- "Vegetation height must be positive."
    else{
      # Compute n 
      if(seperate == TRUE) # Calculate implemented CdAd
        CdAd_imp = Cd * Ad
      else
        CdAd_imp = CdAd
      # Calculate Submerged Vegetation (H>hp)
      X = 1.26*(hp**2)*(2*hp/(11*CdAd_imp))*(1-exp(-5.5*CdAd_imp))
      K = 0.13*exp(-((CdAd_imp-0.4)**2))
      Y = (depth-0.95*hp)*(log((depth/(K*hp))-(0.95/K),base=exp(1))-1)-(0.05*hp)*(log(0.05/K,base=exp(1))-1)
      U = (2.5/depth)*(X+Y) # U/u*
      n_submerged <- (depth**(1/6))/(U*sqrt(9.81))
      # Calculate Submerged Vegetation (H<hp)
      n_emergent <- depth**(2/3)*sqrt((CdAd_imp)/(2*9.81))
      #Calculate n 
      if(depth > hp)
        roughness_coefficient <- n_submerged
      else
        roughness_coefficient <- n_emergent
    }
  }
  else
    roughness_coefficient <- "A parameter is missing."
  # Return n
  return(roughness_coefficient)
}
