#' Compute hydraulic roughness following Cowan (1956)
#'
#' \code{n_cowan1956} calculate Manning's n using the Cowan Method (1956) for
#'      estimating total channel roughness
#'
#' @param material channel material (e.g. earth, rock cut, fine gravel, coarse Gravel)
#' @param irregularity degree of bed irregularity (e.g. smooth, minor, moderate, severe)
#' @param cross variations of channel cross section (e.g. gradual, alternating occasionally, alternating frequently)
#' @param obstructions relative effect of obstructions (e.g. negligible, minor, appreciable, severe)
#' @param vegetation vegetation (e.g. low, medium, high, very high)
#' @param meandering degree of meandering (e.g. minor, appreciable, severe)
#' @param restrict allows for function parameters to restrict certain values. Type bool. Default TRUE.
#'
#' @return Manning's n 
#'
#' @references
#' Cowan, W. L. 1956. Estimating Hydraulic Roughness Coefficients. Agricultural 
#'      Engineering. ASAE, August, 1956. 
#' Phillips, J. V., and S. Tadayon. 2007. Selection of Manning’s Roughness Coefficient for Natural 
#'      and Constructed Vegetated and Non-Vegetated Channels, and Vegetation Maintenance 
#'      Plan Guidelines for Vegetated Channels in Central Arizona. Scientific Investigations 
#'      Report 2006–5108. USGS, Reston, Virginia.
#' 
#' @examples
#' # Result: Manning's n of 0.028
#' material <- 'Earth'
#' irregularity <- 'Smooth'
#' cross <- 'Gradual'
#' obstructions <- 'Negligible'
#' vegetation <- 'Low'
#' meandering <- 'Minor'
#' n_cowan1956(material,irregularity,cross,obstructions,vegetation,meandering)
#' 
#' # Result: Manning's n of 0.075
#' material <- 'Rock Cut'
#' irregularity <- 'Minor'
#' cross <- 'Alternating occasionally'
#' obstructions <- 'Minor'
#' vegetation <- 'Medium'
#' meandering <- 'Appreciable'
#' n_cowan1956(material,irregularity,cross,obstructions,vegetation,meandering)
#' 
#' # Result: Manning's n of 0.142
#' material <- 'Fine Gravel'
#' irregularity <- 'Moderate'
#' cross <- 'Alternating frequently'
#' obstructions <- 'Appreciable'
#' vegetation <- 'High'
#' meandering <- 'Severe'
#' n_cowan1956(material,irregularity,cross,obstructions,vegetation,meandering)
#'
#' @export
n_cowan1956 <- function(material, irregularity, cross, obstructions, vegetation, meandering, restrict = TRUE){
  # Compute and Error Handle for n 
  if(length(material)==1 && length(irregularity)==1 && length(cross)==1 && length(obstructions)==1 && length(vegetation)==1 && length(meandering)==1){
  # Compute material's contribution to n
    roughness_coefficient <- 0
    
    if(restrict == FALSE){
      
      if(material == 'Earth')
        roughness_coefficient = 0.0200
      else if(material == 'Rock Cut')
        roughness_coefficient = 0.0250
      else if(material == 'Fine Gravel')
        roughness_coefficient = 0.0240
      else if(material == 'Coarse Gravel')
        roughness_coefficient = 0.0280
      
      # Compute irregularity's contribution to n
      if(irregularity == 'Smooth')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(irregularity == 'Minor')
        roughness_coefficient = roughness_coefficient + 0.0050
      else if(irregularity == 'Moderate')
        roughness_coefficient = roughness_coefficient + 0.0100
      else if(irregularity == 'Severe')
        roughness_coefficient = roughness_coefficient + 0.0200
      
      # Compute cross's contribution to n
      if(cross == 'Gradual')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(cross == 'Alternating occasionally')
        roughness_coefficient = roughness_coefficient + 0.0050
      else if(cross == 'Alternating frequently')
        roughness_coefficient = roughness_coefficient + 0.0125
      
      # Compute obstructions' contribution to n
      if(obstructions == 'Negligible')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(obstructions == 'Minor')
        roughness_coefficient = roughness_coefficient + 0.0125
      else if(obstructions == 'Appreciable')
        roughness_coefficient = roughness_coefficient + 0.0250
      else if(obstructions == 'Severe')
        roughness_coefficient = roughness_coefficient + 0.0500
      
      # Compute vegetation's contribution to n
      if(vegetation == 'Low')
        roughness_coefficient = roughness_coefficient + 0.0075
      else if(vegetation == 'Medium')
        roughness_coefficient = roughness_coefficient + 0.0175
      else if(vegetation == 'High')
        roughness_coefficient = roughness_coefficient + 0.0375
      else if(vegetation == 'Very High')
        roughness_coefficient = roughness_coefficient + 0.0750
      
      # Compute meandering's contribution to n
      if(meandering == 'Minor')
        roughness_coefficient = roughness_coefficient * 1.0000
      else if(meandering == 'Appreciable')
        roughness_coefficient = roughness_coefficient * 1.1500
      else if(meandering == 'Severe')
        roughness_coefficient = roughness_coefficient * 1.3000
    }
    else{
      # Compute material's contribution to n
      if(material == 'Earth')
        roughness_coefficient = 0.0200
      else if(material == 'Rock Cut')
        roughness_coefficient = 0.0250
      else if(material == 'Fine Gravel')
        roughness_coefficient = 0.0240
      else if(material == 'Coarse Gravel')
        roughness_coefficient = 0.0280
      else
        roughness_coefficient <- "Input for material does not match options."
      
      # Compute irregularity's contribution to n
      if(irregularity == 'Smooth')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(irregularity == 'Minor')
        roughness_coefficient = roughness_coefficient + 0.0050
      else if(irregularity == 'Moderate')
        roughness_coefficient = roughness_coefficient + 0.0100
      else if(irregularity == 'Severe')
        roughness_coefficient = roughness_coefficient + 0.0200
      else
        roughness_coefficient <- "Input for material does not match options."
      
      # Compute cross's contribution to n
      if(cross == 'Gradual')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(cross == 'Alternating occasionally')
        roughness_coefficient = roughness_coefficient + 0.0050
      else if(cross == 'Alternating frequently')
        roughness_coefficient = roughness_coefficient + 0.0125
      else
        roughness_coefficient <- "Input for material does not match options."
      
      # Compute obstructions' contribution to n
      if(obstructions == 'Negligible')
        roughness_coefficient = roughness_coefficient + 0.0000
      else if(obstructions == 'Minor')
        roughness_coefficient = roughness_coefficient + 0.0125
      else if(obstructions == 'Appreciable')
        roughness_coefficient = roughness_coefficient + 0.0250
      else if(obstructions == 'Severe')
        roughness_coefficient = roughness_coefficient + 0.0500
      else
        roughness_coefficient <- "Input for material does not match options."
      
      # Compute vegetation's contribution to n
      if(vegetation == 'Low')
        roughness_coefficient = roughness_coefficient + 0.0075
      else if(vegetation == 'Medium')
        roughness_coefficient = roughness_coefficient + 0.0175
      else if(vegetation == 'High')
        roughness_coefficient = roughness_coefficient + 0.0375
      else if(vegetation == 'Very High')
        roughness_coefficient = roughness_coefficient + 0.0750
      else
        roughness_coefficient <- "Input for material does not match options."
      
      # Compute meandering's contribution to n
      if(meandering == 'Minor')
        roughness_coefficient = roughness_coefficient * 1.0000
      else if(meandering == 'Appreciable')
        roughness_coefficient = roughness_coefficient * 1.1500
      else if(meandering == 'Severe')
        roughness_coefficient = roughness_coefficient * 1.3000
      else
        roughness_coefficient <- "Input for material does not match options."
    }
  }
  else{
    roughness_coefficient <- "A parameter is missing."
  }
  # Return n
  return(roughness_coefficient)
}
