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
  Ecosystem <- c("Coast", "Kystvann")
  #Ecosystem <- c("Ocean", "Hav")
}

## Thematic index (optional)
if(OutputType == "ThematicIndex"){
  theme <- "AlpinePasserines"
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
  EC_ecosystem <- "Forest"
  #EC_ecosystem <- "Mountain"
  #EC_ecosystem <- "Wetland"
 IndicatorSet <- listIndicators_NIforEC(ecosystem = EC_ecosystem)  # TODO: Change to function that lists indicators to drop
}else{
  EC_ecosystem <- NULL
}

## Diagnostics imputation
Diagnostics <- TRUE # Yes
#Diagnostics <- FALSE # No

## Test run (10 vs. 1000 iterations in simulation)
TestRun <- TRUE # Yeas
#TestRun <- FALSE # No


#*******************#
# INDEX CALCULATION #
#*******************#

## Calculate index for specified ecosystem/indicators
if(OutputType == "NatureIndex"){
  ecosystem_use <- Ecosystem[2]
  indicators_use <- NULL
}else{
  ecosystem_use <- EC_ecosystem
  indicators_use <- IndicatorSet
}

  
CustomNI <- calculateCustomNI(ecosystem = ecosystem_use,
                              indicators = indicators_use,
                              theme = theme,
                              KeyIndicators = KeyIndicators,
                              KeyWeight = KeyWeight,
                              AreaWeights = AreaWeights,
                              TrophicWeights = TrophicWeights,
                              NAImputation = NAImputation,
                              years = years,
                              OutputType = OutputType,
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

# TODO: Add code from NIviz for plotting values and uncertainty to map 

## Plot indicator weights
plotWeights(Index$wholeArea$'2019')
plotWeights(Index$wholeArea$'2019', group = "troph")
summaryWeights(Index$wholeArea)






