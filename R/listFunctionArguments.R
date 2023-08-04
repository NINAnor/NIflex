
listFunctionArguments <- function(OutputType, theme = NULL, 
                                  predefNIunits = NULL, indexType = NULL, part = NULL, total = NULL, partOfTotal = NULL,
                                  #maxit = NULL,
                                  awBSunit = NULL){
  
  ## Checks
  if(!(OutputType %in% c("ThematicIndex", "CustomIndex"))){
    stop("Additional flexible function arguments are currently not required/implemented for OutputTypes 'NatureIndex' and 'EcologicalCondition'. Default values are used for these, see calculateCustomNI().")
  }
  
  if(OutputType == "ThematicIndex" & is.null(theme)){
    stop("Argument theme is missing. When OutputType = ThematicIndex, a supported theme needs to be supplied.")
  }
  
  
  #****************************************************************************#
  # CUSTOM INDICES #
  #****************#
  
  if(OutputType == "CustomIndex"){
    
    ## Check for correct format of function arguments
    nChecks <- 9 
    checklist <- matrix(NA, nrow = nChecks, ncol = 2)
    
    checklist[1,] <- c(length(predefNIunits) == 3, "predefNIunits")
    checklist[2,] <- c(all(names(predefNIunits) == c("allArea", "parts", "counties")), "predefNIunits")
    checklist[3,] <- c(is.logical(predefNIunits), "predefNIunits")
    checklist[4,] <- c(indexType %in% c("ecosystem", "thematic", "thematic per ecosystem"), "indexType")
    checklist[5,] <- c(part %in% c("ecosystem", "marine", "terrestrial"), "part")
    checklist[6,] <- c(total %in% c("total", "marine", "terrestrial"), "total")
    checklist[7,] <- c(is.numeric(partOfTotal), "partOfTotal")
    checklist[8,] <- c(dplyr::between(partOfTotal, 0, 1), "partOfTotal")
    checklist[9,] <- c(is.null(awBSunit) | awBSunit %in% c("Skog", "Fjell", "Våtmark", "Åpent lavland", "Ferskvann", "Kystvann", "Hav"), "awBSunits") 
    
    if(!all(checklist[,1] == TRUE)){
      testFails <- paste(unique(checklist[which(checklist[,1] == FALSE),2]), collapse = ", ")
      stop(paste0("The following function arguments are incorrectly specified: ", testFails, ". Please refer to the function documentation for correct specification and supported values."))
    }
    
    ## Assemble function arguments into a list and return
    funArguments <- list(predefNIunits = predefNIunits,
                         indexType = indexType,
                         part = part,
                         total = total,
                         partOfTotal = partOfTotal,
                         awBSunit = awBSunit)
    
    return(funArguments)
  }
  
  
  
  #****************************************************************************#
  # ESTABLISHED  THEMATIC INDICES #
  #*******************************#
  
  if(OutputType == "ThematicIndex"){
    
    # Freshwater acidification #
    #---------------------------#
    if(theme == "Acidification"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = FALSE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Ferskvann")
    }
    
    # Alpine passerines #
    #-------------------#
    if(theme == "AlpinePasserines"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = FALSE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "terrestrial",
                           partOfTotal = 0.2,
                           awBSunit = "Fjell")
    }
    
    # Amphibians #
    #------------#
    if(theme == "Amphibians"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Ferskvann")
    }
    
    # Coastal seabirds #
    #------------------#
    if(theme == "CoastalSeabirds"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "equalWeight")
    }
    
    # Freshwater eutrophication #
    #---------------------------#
    if(theme == "Eutrophication"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = FALSE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Ferskvann")
    }
    
    # Forestry effects #
    #------------------#
    if(theme == "ForestryEffects"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Skog")
    }
    
    
    # Pelagic communities #
    #---------------------#
    if(theme == "PelagicCommunities"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Hav")
      # TODO: Settings for this thematic index need to be double-checked once I have found the code
    }
    
    # Pelagic seabirds #
    #------------------#
    if(theme == "PelagicSeabirds"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "equalWeight")
    }
    
    
    # Wetland vascular plants #
    #-------------------------#
    if(theme == "VascularPlants"){
      funArguments <- list(predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE),
                           indexType = "thematic",
                           part = "ecosystem",
                           total = "total",
                           partOfTotal = 0,
                           awBSunit = "Våtmark")
    }
    
    return(funArguments)
  }
}