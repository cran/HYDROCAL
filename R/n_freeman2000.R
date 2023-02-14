#' Compute hydraulic roughness due to vegetation following Freeman, Rahymeyer, and Copeland (2000)
#'
#' \code{n_freeman2000} calculate Manning's n using the Freeman, Rahymeyer, and 
#'      Copeland (2000) method for estimating vegetative roughness
#' 
#' @param depth flow depth (H) in meters. Assumes wide channel geometry where depth
#'      is approximately equal to hydraulic radius.
#' @param slope channel bed slope (S)
#' @param area sample vegetative plot bed area in square meters, a list of index 5
#' @param hp average plant height (h_p) in meters, a list of index 5
#' @param hlm average leaf mass height (hlm) in meters, a list of index 5
#' @param We average leaf mass width (We) in meters, a list of index 5
#' @param Ds average stem diameter (Ds) in meters, a list of index 5
#' @param pnum number of plants in the corresponding indices, a list of index 5
#' @param snum number of stems at H/4 in the corresponding indices, a list of index 5
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE. 
#'
#' @return Manning's n 
#'
#' @references
#' Freeman, G. E., W. H. Rahmeyer, and R. R. Copeland. 2000. Determination of 
#'      Resistance Due to Shrubs and Woody Vegetation. ERDC/CHL TR-00-25. U.S. 
#'      Army Engineer Research and Development Center, Vicksburg, Mississippi. 
#'
#' @examples
#' # Result: Manning's n of 0.013
#' depth <- 6
#' slope <- 0.005
#' area <- 100
#' hp <- c(0.51,0.71,0.2,0.97,0.71)
#' hlm <- c(0.45,0.71,0.16,0.9,0.62)
#' We <- c(0.229,0.356,0.254,0.482,0.178)
#' Ds <- c(0.0095,0.0095,0.0063,0.0252,0.0063)
#' pnum <- c(2,2,2,1,3)
#' snum <- c(1,1,2,2,6)
#' n_freeman2000(depth,slope,area,hp,hlm,We,Ds,pnum,snum)
#' 
#' # Result: Manning's n of 0.0183
#' depth <- 0.3
#' slope <- 0.005
#' area <- 100
#' hp <- c(0.51,0.71,0.2,0.97,0.71)
#' hlm <- c(0.45,0.71,0.16,0.9,0.62)
#' We <- c(0.229,0.356,0.254,0.482,0.178)
#' Ds <- c(0.0095,0.0095,0.0063,0.0252,0.0063)
#' pnum <- c(2,2,2,1,3)
#' snum <- c(1,1,2,2,6)
#' n_freeman2000(depth,slope,area,hp,hlm,We,Ds,pnum,snum)
#'
#' # Result: Vegetation height must be positive.
#' depth <- 0.3
#' slope <- 0.005
#' area <- 100
#' hp <- c(0.51,0.71,0.2,0.97,-0.23)
#' hlm <- c(0.45,0.71,0.16,0.9,0.62)
#' We <- c(0.229,0.356,0.254,0.482,0.178)
#' Ds <- c(0.0095,0.0095,0.0063,0.0252,0.0063)
#' pnum <- c(2,2,2,1,3)
#' snum <- c(1,1,2,2,6)
#' n_freeman2000(depth,slope,area,hp,hlm,We,Ds,pnum,snum)
#' 
#' @export
n_freeman2000 <- function(depth, slope, area, hp, hlm, We, Ds, pnum, snum, restrict = TRUE){
  
  # Compute and Error Handle for n 
  if(length(depth)==1 && length(slope)==1 && length(area)==1 && length(hp)>0 && length(hlm)>0 && length(We)>0 && length(Ds)>0 && length(pnum)>0 && length(snum)>0){
    if(restrict == FALSE){
      # Compute n 
      u <- sqrt(9.81*depth*slope) # shear velocity, u*, m/s
      t <- 20 # temperature, T, deg C
      p <- 998 # fluid density, p, kg/m^3
      v <- 1.00501002004008*(10**-06) # fluid kinematic viscosity, v, m^2/s
      # Calculate submerged
      Mt <- sum(pnum)/area # total plat density, M_t, m^-2
      Es <- c(0,0,0,0,0)
      Mi <- c(0,0,0,0,0)
      M <- c(0,0,0,0,0)
      Ai <- c(0,0,0,0,0)
      As <- c(0,0,0,0,0)
      Ai_avg <- c(0,0,0,0,0)
      Asi_avg <- c(0,0,0,0,0)
      Esi_avg <- c(0,0,0,0,0)
      hpi_avg <- c(0,0,0,0,0)
      hlmi_avg <- c(0,0,0,0,0)
      for(i in 1:length(hp)){ # determine average properties
        Es[i] <- (7.648*10**6)*(hp[i]/Ds[i])+(2.174*10**4)*((hp[i]/Ds[i])**2)+(1.809*10**3)*((hp[i]/Ds[i])**3) # modulus of plant stiffness, E_s, N/m**2
        Mi[i] <- pnum[i]/area # plant density, M_i, m^-2
        M[i] <- Mi[i]/Mt # relative plant density, Mi/Mt
        Ai[i] <- We[i]*hlm[i] # area, A_i*, m^2
        As[i] <- snum[i]*pi*Ds[i]*(Ds[i]/4) # area, A_s, m^2
        Ai_avg[i] <- M[i]*Ai[i] # A_i* (M_i/M_t)
        Asi_avg[i] <- M[i]*As[i] # A_s (M_i/M_t)
        Esi_avg[i] <- M[i]*Es[i] # E_s (M_i/M_t)
        hpi_avg[i] <- M[i]*hp[i] # h_p,i (M_i/M_t)
        hlmi_avg[i] <- M[i]*hlm[i] # h_lm,i (M_i/M_t)
      }
      n_submerged <- 0.183*((sum(Esi_avg)*sum(Asi_avg)/(p*sum(Ai_avg)*u*u))**0.183)*((sum(hpi_avg)/depth)**0.243)*((Mt*sum(Ai_avg))**0.273)*((v/(u*depth))**0.115)*(depth**(2/3))*((slope**0.5)/u)
      # Calculate Emergent
      for(i in 1:length(hp)){ # determine average properties
        Ai[i] <- (depth-(hp[i]-hlm[i]))*We[i] # area, A_i*, m^2
        Ai_avg[i] <- M[i]*Ai[i] # A_i* (M_i/M_t)
        Asi_avg[i] <- M[i]*As[i] # A_s (M_i/M_t)
      }
      n_emergent <- (3.487*(10**-5))*((sum(Esi_avg)*sum(Asi_avg)/(p*sum(Ai_avg)*u*u))**0.15)*((Mt*sum(Ai_avg))**0.166)*((u*depth/v)**0.622)*(depth**(2/3))*((slope**0.5)/u)
      # Calculate n 
      if(depth > sum(hpi_avg))
        roughness_coefficient <- n_submerged
      else
        roughness_coefficient <- n_emergent
    }
    else if(depth < 0)
      roughness_coefficient <- "Depth must be positive."
    else if(min(hp) < 0)
      roughness_coefficient <- "Vegetation height must be positive."
    else{
      # Compute n 
      u <- sqrt(9.81*depth*slope) # shear velocity, u*, m/s
      t <- 20 # temperature, T, deg C
      p <- 998 # fluid density, p, kg/m^3
      v <- 1.00501002004008*(10**-06) # fluid kinematic viscosity, v, m^2/s
      # Calculate submerged
      Mt <- sum(pnum)/area # total plat density, M_t, m^-2
      Es <- c(0,0,0,0,0)
      Mi <- c(0,0,0,0,0)
      M <- c(0,0,0,0,0)
      Ai <- c(0,0,0,0,0)
      As <- c(0,0,0,0,0)
      Ai_avg <- c(0,0,0,0,0)
      Asi_avg <- c(0,0,0,0,0)
      Esi_avg <- c(0,0,0,0,0)
      hpi_avg <- c(0,0,0,0,0)
      hlmi_avg <- c(0,0,0,0,0)
      for(i in 1:length(hp)){ # determine average properties
        Es[i] <- (7.648*10**6)*(hp[i]/Ds[i])+(2.174*10**4)*((hp[i]/Ds[i])**2)+(1.809*10**3)*((hp[i]/Ds[i])**3) # modulus of plant stiffness, E_s, N/m**2
        Mi[i] <- pnum[i]/area # plant density, M_i, m^-2
        M[i] <- Mi[i]/Mt # relative plant density, Mi/Mt
        Ai[i] <- We[i]*hlm[i] # area, A_i*, m^2
        As[i] <- snum[i]*pi*Ds[i]*(Ds[i]/4) # area, A_s, m^2
        Ai_avg[i] <- M[i]*Ai[i] # A_i* (M_i/M_t)
        Asi_avg[i] <- M[i]*As[i] # A_s (M_i/M_t)
        Esi_avg[i] <- M[i]*Es[i] # E_s (M_i/M_t)
        hpi_avg[i] <- M[i]*hp[i] # h_p,i (M_i/M_t)
        hlmi_avg[i] <- M[i]*hlm[i] # h_lm,i (M_i/M_t)
      }
      n_submerged <- 0.183*((sum(Esi_avg)*sum(Asi_avg)/(p*sum(Ai_avg)*u*u))**0.183)*((sum(hpi_avg)/depth)**0.243)*((Mt*sum(Ai_avg))**0.273)*((v/(u*depth))**0.115)*(depth**(2/3))*((slope**0.5)/u)
      # Calculate Emergent
      for(i in 1:length(hp)){ # determine average properties
        Ai[i] <- (depth-(hp[i]-hlm[i]))*We[i] # area, A_i*, m^2
        Ai_avg[i] <- M[i]*Ai[i] # A_i* (M_i/M_t)
        Asi_avg[i] <- M[i]*As[i] # A_s (M_i/M_t)
      }
      n_emergent <- (3.487*(10**-5))*((sum(Esi_avg)*sum(Asi_avg)/(p*sum(Ai_avg)*u*u))**0.15)*((Mt*sum(Ai_avg))**0.166)*((u*depth/v)**0.622)*(depth**(2/3))*((slope**0.5)/u)
      # Calculate n
      if(depth > sum(hpi_avg))
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
