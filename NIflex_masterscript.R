#*******#
# SETUP #
#*******#

## Install NIcalc if not available
if(system.file(package = 'NIcalc') == ""){
  devtools::install_github("NINAnor/NIcalc", build_vignettes = FALSE)
}


## Load packages
library(NIcalc)
library(gamlss)
library(msm)
library(plyr)
library(mice)
library(RJSONIO)
library(tibble)
library(distr)
library(lattice)
library(sf)
library(tmap)


## Source all functions in "R" folder
sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
    if(trace) cat(nm,":")
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
sourceDir('R')

## Set database credentials
NIdb_username <- Sys.getenv("NIDB_USRNAME")
NIdb_password <- Sys.getenv("NIDB_PASSWORD")


#******************#
# WORKFLOW OPTIONS #
#******************#

## Years
years <- c("1990", "2000", "2010", "2014", "2019")

## Missing value imputation
NAImputation <- TRUE # Yes
#NAImputation <- FALSE # No

## Key indicator weighing
KeyIndicators <- TRUE # Yes
#KeyIndicators <- FALSE # No

KeyWeight <- 0.5

## Area weighing
AreaWeights <- TRUE # Yes
#AreaWeights <- FALSE # No

## Trophic weighing
TrophicWeights <- TRUE # Yes
#TrophicWeigts <- FALSE # No

## Norwegian indicator names
norwegianNames <- TRUE # Yes
#norwegianNames <- FALSE # No (English names instead)

## Output type
OutputType <- "NatureIndex"
#OutputType <- "ThematicIndex"
#OutputType <- "CustomIndex"
#OutputType <- "EcologicalCondition"

## Nature Index ecosystem (optional)
if(OutputType == "NatureIndex"){
  #Ecosystem <- c("Forest", "Skog")
  #Ecosystem <- c("Mountain", "Fjell")
  #Ecosystem <- c("Wetlands", "Våtmark")
  #Ecosystem <- c("OpenLowland", "Åpent lavland")
  #Ecosystem <- c("Freshwater", "Ferskvann")
  #Ecosystem <- c("Coast", "Kystvann")
  Ecosystem <- c("Ocean", "Hav")
}

## Thematic index (optional)
if(OutputType == "ThematicIndex"){
  theme <- "Eutrophication"
  IndicatorSet <- listIndicators_thematicIndex(theme = theme)
}else{
  theme <- "None"
}

## Custom indicator set (optional)
if(OutputType == "CustomIndex"){
  IndicatorSet <- c("Lirype", "Fjellrev", "Aure",
                    "Oter ferskvannsbestand", "Blåstrupe", "Grønlandssel")
}

## Nature Index for Ecological Condition
if(OutputType == "EcologicalCondition"){
  #EC_ecosystem <- c("Forest", "Skog")
  #EC_ecosystem <- c("Mountain", "Fjell")
  EC_ecosystem <- c("Wetlands", "Våtmark")
  #IndicatorSet <- listIndicators_NIforEC(ecosystem = EC_ecosystem)  # No longer needed as we'll likely go with an NI + drop indicators approach instead
}else{
  EC_ecosystem <- NULL
}

## Options for dropping indicators
DropIdxMode <- "pre-defined"
if(DropIdxMode == "pre-defined"){DropIdxList <- NULL}

#DropIdxMode <- "custom"
#DropIdxList <- c(1, 92, 360)

## Set function arguments (for thematic and custom indices)
if(OutputType == "ThematicIndex"){
  funArguments <- listFunctionArguments(OutputType = OutputType,
                                        theme = theme)
}

if(OutputType == "CustomIndex"){
  funArguments <- listFunctionArguments(OutputType = OutputType,
                                        predefNIunits = c(allArea = TRUE, parts = TRUE, counties = FALSE), 
                                        indexType = "thematic", 
                                        part = "ecosystem", 
                                        total = "total", 
                                        partOfTotal = 0,
                                        awBSunit = "Fjell")
}

if(!exists("funArguments")){
  funArguments <- NULL
}

## Diagnostics imputation
Diagnostics <- TRUE # Yes
#Diagnostics <- FALSE # No

## Test run (10 vs. 1000 iterations in simulation)
TestRun <- TRUE # Yes
#TestRun <- FALSE # No


#*******************#
# INDEX CALCULATION #
#*******************#

## Set ecosystem name
if(OutputType %in% "NatureIndex"){
  ecosystem_use <- Ecosystem[2]
}else{
  ecosystem_use <- EC_ecosystem[2]
}

## Assign indicator set
if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
  indicators_use <- NULL
}else{
  indicators_use <- IndicatorSet
}

## Make list of indicators to drop
dropIdx <- selectDropIndices(DropIdxMode = DropIdxMode,
                             OutputType = OutputType,
                             ecosystem = ifelse(OutputType %in% c("NatureIndex", "EcologicalCondition"), ecosystem_use, NULL),
                             customList = DropIdxList)

## Calculate index for specified ecosystem/indicators
CustomNI <- calculateCustomNI(ecosystem = ecosystem_use,
                              indicators = indicators_use,
                              theme = theme,
                              dropIdx = dropIdx,
                              KeyIndicators = KeyIndicators,
                              KeyWeight = KeyWeight,
                              AreaWeights = AreaWeights,
                              TrophicWeights = TrophicWeights,
                              NAImputation = NAImputation,
                              years = years,
                              OutputType = OutputType,
                              funArguments = funArguments,
                              Diagnostics = Diagnostics,
                              TestRun = TestRun,
                              norwegianNames = norwegianNames,
                              saveSteps = TRUE)
  



#*************#
# INDEX PLOTS #
#*************#

Index <- CustomNI$CustomIndex

## Plot time series
plot(Index$wholeArea, main = "Custom index", cex = 1, lwd = 2, shade = TRUE)
summary(Index$wholeArea)

# TODO: Add code from NIviz for density ridge time-series plot

## Plot map

shapeLibraryPath <- "P:/41201611_naturindeks_2021_2023_vitenskapelig_ansvar/Shapefiles"

if(OutputType %in% c("NatureIndex", "EcologicalConditions")){
  plotNI_Map(Index = Index, 
             year = 2019, 
             OutputType = OutputType, 
             ecosystem = ecosystem_use,
             shapeLibraryPath = shapeLibraryPath,
             plotMedian = TRUE, plotCI = TRUE, plotDisplacement = FALSE, 
             interactiveMap = FALSE)
}

if(OutputType == "ThematicIndex"){
  plotNI_Map(Index = Index, 
             year = 2019, 
             OutputType = OutputType, 
             theme = theme,
             shapeLibraryPath = shapeLibraryPath,
             plotMedian = TRUE, plotCI = TRUE, plotDisplacement = FALSE, 
             interactiveMap = FALSE)
}

if(OutputType == "CustomIndex"){
  plotNI_Map(Index = Index, 
             year = 2019, 
             OutputType = OutputType, 
             awBSunit = funArguments$awBSunit,
             shapeLibraryPath = shapeLibraryPath,
             plotMedian = TRUE, plotCI = TRUE, plotDisplacement = FALSE, 
             interactiveMap = FALSE)
}


## Plot indicator weights
plotWeights(Index$wholeArea$'2019')
plotWeights(Index$wholeArea$'2019', group = "troph")
summaryWeights(Index$wholeArea)






