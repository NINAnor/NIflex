#' Make indicator list for a Nature Index to be used in ecological condition 
#' assessments
#'
#' The Nature Index is used in the framework for ecological condition assessment
#' as an indicator of biodiversity (representing species abundance and trends).
#' To fulfill that role, the index for a specific ecosystem will not necessarily
#' be made up of the full set of indicators that are used in its standalone 
#' implementation as part of the reporting of the Norwegian Nature Index. 
#' This function writes ecosystem-specific lists of indicators for making
#' versions of the Nature Index for ecological condition assessments. 
#' 
#' NOTE: This function is incomplete and needs to be updated once discussions
#' about which indicators should be part of the different products have been
#' concluded (expected late 2023).
#' 
#' @param ecosystem character string specifying the ecosystem of interest. 
#' Currently supported are "Forest", "Mountain", and "Wetland". 
#'
#' @return
#' @export
#'
#' @examples

listIndicators_NIforEC <- function(ecosystem){
  
  # Forest ecosystem #
  #------------------#
  if(ecosystem == "Forest"){
    indicatorList <- c("TBA")
  }
  
  # Mountain ecosystem #
  #--------------------#
  if(ecosystem == "Mountain"){
    indicatorList <- c("TBA")
  }
  
  # Wetland ecosystem #
  #-------------------#
  if(ecosystem == "Wetland"){
    indicatorList <- c("TBA")
  }
  
  
  
  if(exists("indicatorList")){
    return(indicatorList)
  }else{
    stop("The specified ecosystem does not correspond to an ecosystem nature index that is currently supported. Please check the function documentation for supported options.")
  }
  
}