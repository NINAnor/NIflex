#' Calculate custom index based on data in the Nature Index Database
#'
#' This function is a wrapper for downloading select data from the Nature Index
#' database and calculating a choice index from it according to the Nature Index
#' methodology. 
#' 
#' The steps that the function goes through are as follows:
#' 1) Data download via NIcalc::importDatasetApi
#' 2) Assembly of input data for calculations via NIcalc::assembleNiObject
#' 3) Optional: run dataset diagnostics via NIcalc::imputeDiagnostics
#' 4) Optional: impute missing data via NIcalc::imputeData (which uses mice::mice)
#' 5) Index calculation via NIcalc::calculateIndex
#' 
#' There are four groups of indices that can be calculated using this function: 
#' Nature Index for any ecosystem (forest, mountain, wetlands, open lowland,
#' freshwater, coast, and ocean), modified Nature Index for ecological condition
#' assessments (for ecosystems forest, mountain, and wetlands), a selection of
#' pre-defined thematic indices (see listIndicators_thematicIndex for supported
#' thematic indices), and fully customized indices. 
#' 
#' The function allows a high degree of control over the calculation via a range
#' of control arguments. That way, a user can choose the indicators and years 
#' to include, whether and how to perform indicator weighting, whether or not to
#' impute missing values, etc. 
#' 
#' Running this function requires valid user credentials for the Nature Index 
#' database (https://naturindeks.nina.no/). Credentials may be requested from 
#' NINA by contacting the project manager for the Nature Index. Currently 
#' Chloé R. Nater: chloe.nater(at)nina.no.  
#' 
#' @param ecosystem character. The ecosystem for which to calculate the index. 
#' Optional argument required when OutputType = "NatureIndex" or 
#' "EcologicalCondition". Can be one of c("Skog", "Fjell", "Våtmark", 
#' "Åpent lavland", "Ferskvann", "Kystvann", "Hav"). Note that only the first
#' three are relevan for OutputType = "EcologicalCondition" so far. 
#' @param indicators character vector containing Norwegian names of indicators 
#' to include in the custom index. Optional argument that has to be provided
#' when calculating thematic or fully custom indices.
#' @param theme character. Optional argument specifying which thematic index 
#' should be produced. Required when OutputType = "ThematicIndex". For currently
#' supported thematic indices, see documentation of listIndicators_thematicIndex().
#' @param dropInd a vector containing integer IDs for indicators to drop.
#' @param username character. Username for the Nature Index database. 
#' @param password character. Password for the Nature Index database.
#' @param KeyIndicators logical. If TRUE, applies key indicator weighting, i.e. 
#' the indicators categorized as key indicators will together make up a 
#' proportion of the index equal to "KeyWeight". 
#' @param KeyWeight numeric, between 0 and 1. Proportion of the index that is 
#' allocated to key indicators. Only in effect if KeyIndicators = TRUE. 
#' @param AreaWeights logical. If TRUE, weights indicators according to area. 
#' @param TrophicWeights logical. If TRUE, weighs indicators according to 
#' trophic group. 
#' @param NAImputation logical. If TRUE, imputes missing values in dicator data
#' using MICE (Multivariate Imputation by Chained Equations). If FALSE, ignores
#' missing values. 
#' @param years integer vector specifying years for which to calculate index. 
#' Note that at present, data in the Nature Index database is only available for
#' years 1990, 2020, 2010, 2011, 2012, 2013, 2014, and 2019. 
#' @param OutputType character. The type of output the workflow will create, 
#' here an optional argument. Can be one of 
#' c("NatureIndex", "EcologicalCondition", "ThematicIndex", "CustomIndex").
#' @param funArguments a list of arguments to pass on to downstream functions 
#' from NIcalc. Required for output types "ThematicIndex" and "CustomIndex". 
#' Output of listFunctionArguments(). 
#' @param Diagnostics logical. If TRUE, calculates diagnostics for the dataset
#' prior to imputation. If FALSE, skips diagnostics calculation. 
#' @param TestRun logical. If TRUE, runs a shorter version of the workflow with 
#' only 10 iterations for imputation and calculation. If FALSE, performs a full
#' run with 1000 iterations instead. 
#' @param norwegianNames logical. If TRUE (default), data and results are returned
#' with Norwegian indicator and ecosystem names. 
#' @param saveSteps logical. If TRUE (default), saves results at each step of
#' the workflow as .rds files into the working directory. 
#'
#' @return a list of lists containing general information on the calculated 
#' index ("indexInfo"), the raw dataset downloaded form the Nature Index database 
#' ("importData"), the formatted input dataset underlying index calculations 
#' ("InputData"), results of dataset diagnostics ("Diagnostics", only if 
#' Diagnostics = TRUE), the imputed values for missing data points ("NAImputes,
#' only if NAImputation = TRUE), and the calculated custom Index ("CustomIndex").
#' @export
#'
#' @examples

calculateCustomNI <- function(ecosystem = NULL, indicators = NULL, theme = "None",
                              dropInd = NULL,
                              username, password,
                              KeyIndicators, KeyWeight, 
                              AreaWeights, TrophicWeights,
                              NAImputation, years, OutputType,
                              funArguments = NULL,
                              Diagnostics, TestRun,
                              norwegianNames = TRUE, saveSteps = TRUE){
  
  ## Determine if imputation has to be skipped (currently the case for a few thematic indices)
  if(OutputType == "ThematicIndex" & theme %in% c("Amphibians", "VascularPlants")){
    forceSkipImputation <- TRUE
    # TODO: This seems to be relevant when there are no missing values in indicator set. 
    # This could probably be generalised to skip imputation whenever there are no missing values instead.  
  }else{
    forceSkipImputation <- FALSE
  }
  
  ## List implementation characteristics
  indexInfo <- list(OutputType = OutputType,
                    ecosystem = ecosystem,
                    indicators = indicators, 
                    KeyIndicators = KeyIndicators,
                    KeyWeight = ifelse(KeyIndicators, KeyWeight, NA),
                    AreaWeights = AreaWeights,
                    TrophicWeights = TrophicWeights,
                    NAImputation = NAImputation,
                    years = years)
  
  ## Set simulation parameter
  nSim_run <- ifelse(TestRun, 10, 1000)
  
  
  #****************************************************************************#
  # DATA IMPORT #
  #*************#
  
  ## Import data
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){

    if(ecosystem %in% c("Kystvann", "Hav")){
      
      ecosystem_part <- c(ifelse(ecosystem == "Kystvann", "Kystvann-bunn", "Havbunn"),
                          paste0(ecosystem, "-pelagisk"))
      
      importData <- importMergeDataset_CoastOcean(ecosystem_part = ecosystem_part,
                                                  ecosystem = ecosystem,
                                                  username = username,
                                                  password = password,
                                                  year = years,
                                                  norwegian = norwegianNames,
                                                  refYearCode = 0)
      
    }else{
      
      importData <- NIcalc::importDatasetApi(username = username,
                                             password = password,
                                             eco = ecosystem,
                                             year = years,
                                             norwegian = norwegianNames,
                                             refYearCode = 0)
    }

  }else{
    importData <- NIcalc::importDatasetApi(username = username,
                                           password = password,
                                           indic = indicators,
                                           year = years,
                                           norwegian = norwegianNames,
                                           refYearCode = 0)
  }
  
  ## Selective removal of indicators
  if(!is.null(dropInd)){
    importData$ICunits <- importData$ICunits[!(importData$ICunits$indId %in% dropInd), ]
    importData$referenceValues$referenceValues <- importData$referenceValues$referenceValues[!(importData$referenceValues$referenceValues$indId %in% dropInd), ]
    importData$indicatorObservations$indicatorValues <- importData$indicatorObservations$indicatorValues[!(importData$indicatorObservations$indicatorValues$indId %in% dropInd), ]
    importData$indicators <- importData$indicators[!(importData$indicators$id %in% dropInd), ]
  }
  
  ## Optional: save step
  if(saveSteps){
    stepData <- list(indexInfo = indexInfo,
                     importData = importData)
    saveRDS(stepData, file = "stepData.rds")
  }
  
  
  #****************************************************************************#
  # NI OBJECT ASSEMBLY #
  #********************#
  
  ## Nature Index and Ecological Condition data assembly
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    NIObject <- NIcalc::assembleNiObject(inputData = importData,
                                         predefNIunits = c(allArea = T, parts = T, counties = F),
                                         indexType = "ecosystem",
                                         part = "ecosystem",
                                         total = ifelse(ecosystem == "Fjell", "terrestrial", "total"),
                                         partOfTotal = ifelse(ecosystem == "Fjell", 0.2, 0))
  }

  ## Thematic index data assembly
  if(OutputType == "ThematicIndex"){
    
    # Check for presence of additional function arguments
    if(is.null(funArguments)){
      stop("NiObject assembly for thematic indices requires additional function arguments that have to be specified via 'funArguments'.")
    }
    
    # Adjust NI units for select thematic indices
    if(theme %in% c("Acidification", "Eutrophication", "AlpinePasserines")){
      NIunitsCustom <- addCustomSpatialUnits(theme = theme,
                                             importData = importData)
    }else{
      NIunitsCustom <- NULL
    }
    
    # Assemble NIObject
    NIObject <- NIcalc::assembleNiObject(inputData = importData,
                                         NIunits = NIunitsCustom,
                                         predefNIunits = funArguments$predefNIunits,
                                         indexType = funArguments$indexType,
                                         part = funArguments$part,
                                         total = funArguments$total,
                                         partOfTotal = funArguments$partOfTotal)
    
    ## Adjust NIunits for seabird thematic indices
    if(theme %in% c("CoastalSeabirds", "PelagicSeabirds")){
      NIObject <- addCustomSpatialUnits(theme = theme,
                                        NIObject = NIObject)
    }
  }
  
  ## Custom index data assembly
  if(OutputType == "CustomIndex"){
    #stop("NiObject assembly for for custom indices has not been streamlined yet")
    # TODO: Write assembleNiObject() call for custom indices. Some of the function
    # arguments likely need to be controllable from the master script. 
    
    # Check for presence of additional function arguments
    if(is.null(funArguments)){
      stop("NiObject assembly for custom indices requires additional function arguments that have to be specified via 'funArguments'.")
    }
    
    # Assemble NIObject
    NIObject <- NIcalc::assembleNiObject(inputData = importData,
                                         predefNIunits = funArguments$predefNIunits,
                                         indexType = funArguments$indexType,
                                         part = funArguments$part,
                                         total = funArguments$total,
                                         partOfTotal = funArguments$partOfTotal)
  }
  
  ## Ecosystem-level outputs: collapse generalists and specialists
  if(OutputType %in% c("NatureIndex", "Ecological Condition") | 
     (OutputType == "ThematicIndex" & theme == "ForestryEffects")){
    
    xxx <- yyy <- NIObject$indicators$functionalGroup
    xxxId <- yyyId <- NIObject$indicators$functionalGroupId
    
    yyy[xxxId %in% c(1,2)] <- "Mellompredator"
    yyyId[xxxId %in% c(1,2)] <- 1
    yyy[xxxId %in% c(6,7)] <- "Primærprodusent"
    yyyId[xxxId %in% c(6,7)] <- 6
    yyy[xxxId %in% c(8,9)] <- "Topp-predator"
    yyyId[xxxId %in% c(8,9)] <- 8
    
    NIObject$indicators$functionalGroup <- yyy
    NIObject$indicators$functionalGroupId <- yyyId
    
  }
  
  if(saveSteps){
    stepData$InputData <- NIObject
    saveRDS(stepData, file = "stepData.rds")
  }
  
  
  #****************************************************************************#
  # DIAGNOSTICS IMPUTATION #
  #************************#
  
  if(Diagnostics){
    DataDiagnostics <- NIcalc::imputeDiagnostics(x = NIObject,
                                                 nSim = 10,
                                                 transConst = 0.01,
                                                 maxit = 20) # TODO: Check with Bård why this number varies for different indices
    #DataDiagnostics$diagnostics$convergencePlot
    
    if(saveSteps){
      stepData$Diagnostics <- DataDiagnostics
      saveRDS(stepData, file = "stepData.rds")
    }
  }
  
  
  #****************************************************************************#
  # NA IMPUTATION #
  #***************#

  if(NAImputation & !forceSkipImputation){
    NAImputes <- NIcalc::imputeData(x = NIObject,
                                    nSim = nSim_run,
                                    transConst = 0.01,
                                    maxit = 20, # TODO: Check with Bård why this number varies for different indices
                                    printFlag = TRUE)
    
    stepData$NAImputes <- NAImputes
    
    if(saveSteps){
      saveRDS(stepData, file = "stepData.rds")
    }
    
  }else{
    NAImputes <- NULL
  }
  

  
  #****************************************************************************#
  # INDEX CALCULATION #
  #*******************#
  
  ## Set parameters awBSunits
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    awBSunit_use <- ecosystem
  }else{
    awBSunit_use <- funArguments$awBSunit
  }
  
  ## Calculate custom index
  CustomIndex <- NIcalc::calculateIndex(x = NIObject,
                                        imputations = NAImputes,
                                        awBSunit = awBSunit_use, 
                                        nsim = nSim_run,
                                        fids = FALSE, 
                                        tgroups = TrophicWeights, # Trophic weighing
                                        awbs = AreaWeights, # Area weighing
                                        keys = ifelse(KeyIndicators, "specialWeight", "ignore"), # Use of key indicators
                                        w = ifelse(KeyIndicators, KeyWeight, NULL)) # Weight of key indicators
  
  
  ## Save and return results
  if(saveSteps){
    stepData$CustomIndex <- CustomIndex
  }else{
    stepData <- list(indexInfo = indexInfo,
                     CustomIndex = CustomIndex)
  }
  saveRDS(stepData, file = "CustomIndex_Data.rds")
  
  return(stepData)
}