

calculateCustomNI <- function(ecosystem = NULL, indicators = NULL, theme = "None",
                              KeyIndicators, KeyWeight, 
                              AreaWeights, TrophicWeights,
                              NAImputation, years, OutputType,
                              Diagnostics, TestRun,
                              norwegianNames = TRUE, saveSteps = TRUE){
  
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
    # TODO: Add 2-step data import for ecosystems "Hav" and "Kystvann"
    
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
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    
    if(ecosystem %in% c("Skog", "Våtmark")){
      dropIdx <- 155 # (lavhei)
    }
    
    if(ecosystem == "Hav"){
      dropIdx <- 165 # (sei)
    }
    
    if(exists("dropIdx")){
      importData$ICunits <- importData$ICunits[importData$ICunits$indId != dropIdx, ]
      importData$referenceValues$referenceValues <- importData$referenceValues$referenceValues[importData$referenceValues$referenceValues$indId != dropIdx, ]
      importData$indicatorObservations$indicatorValues <- importData$indicatorObservations$indicatorValues[importData$indicatorObservations$indicatorValues$indId != dropIdx, ]
      importData$indicators <- importData$indicators[importData$indicators$id != dropIdx, ]
    }
  }

  # TODO: Implement a routine for dropping a selection of indicators (list passed to function)
  # from Nature Index and Ecological Condition data sets. 
  
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
    stop("NiObject assembly for for thematic indices has not been streamlined yet")
    # TODO: Write assembleNiObject() call for thematic indices. Function arguments
    # vary among thematic indices, and the specific combinations need to be 
    # programmed in a helper function. 
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

  if(NAImputation){
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
    # TODO: Allow for feeding in of additional information "awBSunit" to calculateIndex(). 
    stop("Thematic and custom indices require feeding in of additional information via yet-to-be implemented helper functions. Information required: awBSunit argument for calculateIndex().")
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