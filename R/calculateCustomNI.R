

calculateCustomNI <- function(ecosystem = NULL, indicators = NULL, theme = "None",
                              dropIdx = NULL,
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
                                                  username = NIdb_username,
                                                  password = NIdb_password,
                                                  eco = ecosystem,
                                                  year = years,
                                                  norwegian = norwegianNames,
                                                  refYearCode = 0)
      
    }else{
      
      importData <- importDatasetApi(username = NIdb_username,
                                     password = NIdb_password,
                                     eco = ecosystem,
                                     year = years,
                                     norwegian = norwegianNames,
                                     refYearCode = 0)
    }

  }else{
    importData <- importDatasetApi(username = NIdb_username,
                                   password = NIdb_password,
                                   indic = indicators,
                                   year = years,
                                   norwegian = norwegianNames,
                                   refYearCode = 0)
  }
  
  ## Selective removal of indicators
  if(!is.null(dropIdx)){
    importData$ICunits <- importData$ICunits[!(importData$ICunits$indId %in% dropIdx), ]
    importData$referenceValues$referenceValues <- importData$referenceValues$referenceValues[!(importData$referenceValues$referenceValues$indId %in% dropIdx), ]
    importData$indicatorObservations$indicatorValues <- importData$indicatorObservations$indicatorValues[!(importData$indicatorObservations$indicatorValues$indId %in% dropIdx), ]
    importData$indicators <- importData$indicators[!(importData$indicators$id %in% dropIdx), ]
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
    NIObject <- assembleNiObject(inputData = importData,
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
    
    ## Adjust NI units for select thematic indices
    if(theme %in% c("Acidification", "Eutrophication", "AlpinePasserines")){
      NIunitsCustom <- addCustomSpatialUnits(theme = theme,
                                             importData = importData)
    }else{
      NIunitsCustom <- NULL
    }
    
    NIObject <- assembleNiObject(inputData = importData,
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
    stop("NiObject assembly for for custom indices has not been streamlined yet")
    # TODO: Write assembleNiObject() call for custom indices. Some of the function
    # arguments likely need to be controllable from the master script. 
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
    DataDiagnostics <- imputeDiagnostics(x = NIObject,
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
    NAImputes <- imputeData(x = NIObject,
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
  CustomIndex <- calculateIndex(x = NIObject,
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