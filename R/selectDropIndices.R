#' Make a list of selected indicators that should be dropped from an index
#'
#' This function gives some more control over indices with pre-defined sets of
#' indicators. 
#' 
#' When DropIndMode is set to "pre-defined", the list of indicators to drop will
#' be a default set that is specified within the function. At present, this 
#' includes the indicators "Lavhei" in forest and wetlands ecosystems and "Sei"
#' in ocean ecosystem. These have previously been dropped from the calculation 
#' of the Nature Index for the relevant ecosystem was they were deemed little
#' representative for the ecosystem. 
#' This list may be extended somewhat when updating the Nature Index in 2025 as
#' there are some additional indicators that may have to be dropped due to 
#' insufficient data for updating. 
#' 
#' When DropIndMode is set to "custom", one may pass a list of any length 
#' containing the integer IDs of indicators one wishes to drop from calculations. 
#' The indicator IDs can currently only be found in the Nature Index database
#' (https://naturindeks.nina.no/) and login credentials are required. It would
#' be good to display indicator IDs publicly, or make it possible to supply
#' indicator names to this function instead in the future. 
#' 
#' @param DropIndMode character. If "pre-defined" (default), lists a default set
#' of indicators from indicator sets for Nature Index and Ecological Condition
#' outputs. If "custom", returns the same indicator list as provided through the
#' "customList" argument. 
#' @param OutputType character. The type of output the workflow will create, 
#' here an optional argument. Can be one of 
#' c("NatureIndex", "EcologicalCondition", "ThematicIndex", "CustomIndex"). 
#' @param ecosystem character. The ecosystem for which to calculate the index. 
#' Optional argument required when OutputType = "NatureIndex" or 
#' "EcologicalCondition". Can be one of c("Skog", "Fjell", "Våtmark", 
#' "Åpent lavland", "Ferskvann", "Kystvann", "Hav"). Note that only the first
#' three are relevant for OutputType = "EcologicalCondition" so far. 
#' @param customList a vector containing integer IDs for indicators to drop. 
#' Optional argument that needs to be provided when DropIndMode = "custom". 
#'
#' @return a vector containing integer IDs for indicators to drop.
#' @export
#'
#' @examples

selectDropIndicators <- function(DropIndMode = "pre-defined", OutputType = NULL, ecosystem = NULL, customList = NULL){

  DropIndices <- NULL
  
  ## Check for validity of argument combinations
  if(!(DropIndMode %in% c("pre-defined", "custom"))){
    stop("Invalid Mode specified. The supported options are 'pre-defined' and 'custom'.")
  }
  
  if(DropIndMode == "custom" & is.null(customList)){
    stop("customList missing. When running DropIndMode = 'custom', a list of indicator id's has to be provided via the argumen 'customList'.")
  }
  
  if(OutputType %in% c("ThematicIndex", "CustomIndex") & DropIndMode == "custom"){
    warning("You have specified additional indices to drop from a custom or thematic index. This may not be what you intended.")
  }
  
  ## Write corresponding lists of indicator id's to drop
  
  # TODO: Update exclusions for Nature Index prior to 2025 update
  # TODO: Update exclusions for Ecological condition after discussions with Anders
  
  if(DropIndMode == "custom"){
    
    DropIndices <- customList
    
  }else{
    
    if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
      
      # Nature index forest
      if(OutputType == "NatureIndex" & ecosystem == "Skog"){
        DropIndices <- 155 # (lavhei)
      }
      
      # Nature index mountain
      if(OutputType == "NatureIndex" & ecosystem == "Fjell"){
        DropIndices <- NULL
      }
      
      # Nature index wetlands
      if(OutputType == "NatureIndex" & ecosystem == "Våtmark"){
        DropIndices <- 155 # (lavhei)
      }
      
      # Nature index open lowlands
      if(OutputType == "NatureIndex" & ecosystem == "Åpent lavland"){
        DropIndices <- NULL
      }
      
      # Nature index freshwater
      if(OutputType == "NatureIndex" & ecosystem == "Ferskvann"){
        DropIndices <- NULL
      }
      
      # Nature index coast
      if(OutputType == "NatureIndex" & ecosystem == "Kystvann"){
        DropIndices <- NULL
      }
      
      # Nature index ocean
      if(OutputType == "NatureIndex" & ecosystem == "Hav"){
        DropIndices <- 165 # (sei)
      }
      
      
      # Ecological condition forest
      if(OutputType == "EcologicalCondition" & ecosystem == "Skog"){
        DropIndices <- 155 # (lavhei)
      }
      
      # Ecological condition mountain
      if(OutputType == "EcologicalCondition" & ecosystem == "Fjell"){
        DropIndices <- NULL
      }
      
      # Ecological condition wetlands
      if(OutputType == "EcologicalCondition" & ecosystem == "Våtmark"){
        DropIndices <- 155 # (lavhei)
      }
      
      
    }else{
      DropIndices <- NULL
    }
    
  }
  
  return(DropIndices)
  
}
