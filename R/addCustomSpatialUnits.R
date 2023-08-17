#' Add custom spatial units to input data for thematic indices
#'
#' A subset of thematic indices use their own specific spatial units. 
#' At present, this is the case for thematic indices "Acidification", "Eutrophication",
#' "AlpinePasserines", "CoastalSeabirds", and "PelagicSeabirds". 
#' 
#' For the first three, custom spatial units get added to the data right after
#' import and the argument importData needs to be provided. 
#' For the seabird indices, custom spatial units get added to the assembled
#' NiObject and the argument NIObject needs to be provided. 
#' 
#' @param theme character. Argument specifying which thematic index 
#' should be produced. Required when OutputType = "ThematicIndex". For currently
#' supported thematic indices, see documentation of listIndicators_thematicIndex().
#' @param NIObject list containing assembled NiObject. 
#' @param importData list containing imported data. 
#'
#' @return imported dataset or assembled NiObject with custom spatial units. 
#' @export
#'
#' @examples

addCustomSpatialUnits <- function(theme, NIObject = NULL, importData = NULL){
  
  ## Error if thematic index does not use custom spatial units
  if(!(theme %in% c("Acidification", "AlpinePasserines", "CoastalSeabirds", "Eutrophication", "PelagicSeabirds"))){
    stop("No custom spatial units need to be added for the selected thematic index.")
  }
  
  # Acidification / eutrophication #
  #--------------------------------#
  
  if(theme %in% c("Acidification", "Eutrophication")){
    NIunitsCustom <- read.table(file = "data/NIunits_AcidificationEutrophication.txt", header = TRUE, sep="\t")
    
    return(NIunitsCustom)
  }
  
  # Alpine passerines #
  #-------------------#
  
  if(theme == "AlpinePasserines"){
    
    if(is.null(importData)){
      stop("importData not provided. For thematic index 'AlpinePasserines', custom spatial units are assembled as an independent dataframe based off importData.")
    }
    
    BSunitId <- importData$BSunits$id
    NIunitName <- rep("x", length(BSunitId))
    NIunitName[BSunitId < 1800] <- "Sør-Norge"
    NIunitName[BSunitId >= 1800] <- "Nord-Norge"
    
    NIunitsCustom <- data.frame(NIunitName = NIunitName, BSunitId = BSunitId)
    
    return(NIunitsCustom)
  }
  
  
  # Coastal / pelagic seabirds #
  #----------------------------#
  
  if(theme %in% c("CoastalSeabirds", "PelagicSeabirds") & is.null(NIObject)){
    stop("NIObject not provided. For thematic indices 'CoastalSeabirds' and 'PelagicSeabirds', custom spatial units get added to the NIObject.")
  }
  
  if(theme == "CoastalSeabirds"){
    NIObject$BSunits$equalWeight <- rowSums(!is.na(NIObject$ICunits))
    NIObject$BSunits$equalWeight[NIObject$BSunits$`Kystvann-bunn`== 0] <- 0
    
    return(NIObject)
  }
    
  if(theme == "PelagicSeabirds"){
    NIObject$BSunits$equalWeight <- rowSums(!is.na(NIObject$ICunits))
    NIObject$BSunits$equalWeight[NIObject$BSunits$`Kystvann-pelagisk`== 0] <- 0
    
    return(NIObject)
  }

  

  
}