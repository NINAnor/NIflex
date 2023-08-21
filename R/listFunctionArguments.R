#' List additional function arguments for thematic and custom indices
#'
#' This is a helper function which collates arguments used by other functions
#' from the NIcalc packages when calculating thematic and fully custom indices. 
#' For thematic indices, only arguments "OutputType" and "theme" need to be 
#' provided. For fully custom indices, other arguments need to be provided as
#' well. 
#' 
#' The arguments specified here get passed on to several functions from the 
#' NIcalc package, including assembleNiObject() and calculateNIunitWeight() and 
#' we refer to the NIcalc documentation for more details. 
#' 
#' @param OutputType character. The type of output the workflow will create. Can
#' be "ThematicIndex" or "CustomIndex". This function is currently not needed 
#' for OutputType = "NatureIndex" and OutputType = "EcologicalCondition". 
#' @param theme character. Optional argument specifying which thematic index 
#' should be produced. Required when OutputType = "ThematicIndex". For currently
#' supported thematic indices, see documentation of listIndicators_thematicIndex().
#' @param predefNIunits vector of length 3 containing logical objects named 
#' "allAreas", "parts", and "counties". Optional argument, only required when 
#' OutputType = "CustomIndex". 
#' @param indexType character. Optional argument passed on to 
#' NIcalc::assembleNiObject specifying whether input data should be structured 
#' according to ecosystem (options "ecosystem", "thematicPerEcosystem") or not
#' (option "thematic"). Needs to be specified only for fully custom indices. 
#' @param part character one of c("ecosystem","marine","terrestrial"). Optional 
#' argument passed on to NIcalc::assembleNiObject specifying the numerator 
#' variable in criterion for selecting BSunits.
#' @param total character one of c("ecosystem","marine","terrestrial"). Optional 
#' argument passed on to NIcalc::assembleNiObject specifying the denominator 
#' variable in criterion for selecting BSunits.
#' @param partOfTotal numeric, between 0 and 1. Optional argument passed on to
#' NIcalc::assembleNiObject which can be used to restrict the input data set to 
#' BSunits satisfying: part/total > partOfTotal. 
#' @param awBSunit character. Optional argument passed on to
#' NIcalc::calculateNIunitWeigth via NIcalc::calculateIndex. Specifies which 
#' ecosystem's BSunits should be used to calculate area weights. 
#'
#' @return a list of arguments to pass on to downstream functions from NIcalc. 
#' @export
#'
#' @examples

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