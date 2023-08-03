
selectDropIndices <- function(mode = "pre-defined", OutputType = NULL, ecosystem = NULL, customList = NULL){
  
  ## Check for validity of argument combinations
  if(!(mode %in% c("pre-defined", "custom"))){
    stop("Invalid Mode specified. The supported options are 'pre-defined' and 'custom'.")
  }
  
  if(mode == "custom" & is.null(customList)){
    stop("customList missing. When running mode = 'custom', a list of indicator id's has to be provided via the argumen 'customList'.")
  }
  
  if(OutputType %in% c("ThematicIndex", "CustomIndex") & mode == "custom"){
    warning("You have specified additional indices to drop from a custom or thematic index. This may not be what you intended.")
  }
  
  ## Write corresponding lists of indicator id's to drop
  
  # TODO: Update exclusions for Nature Index prior to 2025 update
  # TODO: Update exclusions for Ecological condition after discussions with Anders
  
  if(mode == "custom"){
    
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