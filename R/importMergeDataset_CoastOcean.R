#' Import merged datasets for coast and ocean ecosystems
#'
#' This is a helper function for importing data for the marine ecosystems "Kyst"
#' (coast) and "Hav" (ocean). 
#' Data for these ecosystems needs to be imported stepwise as they are stored in 
#' the Nature Index database as two datasets each: one for seafloor and one for 
#' open water (pelagic). This function imports both data sets and combines them
#' into one for further analysis. 
#' 
#' @param ecosystem_part character vector of length two with the names of the 
#' sub-ecosystems to combine. c("Kystvann-bunn", "Kystvann-pelagisk") for coast and
#' c("Havbunn", "Hav-pelagisk") for ocean. 
#' @param ecosystem character. The ecosystem for which to import a merged data
#' file. Can be either "Kystvann" (coast) or "Hav" (ocean).
#' @param username character. Username for the Nature Index database. 
#' @param password character. Password for the Nature Index database.
#' @param year integer vector specifying years for which to import data. 
#' Note that at present, data in the Nature Index database is only available for
#' years 1990, 2020, 2010, 2011, 2012, 2013, 2014, and 2019. 
#' @param norwegian logical. If TRUE (default), data and results are returned
#' with Norwegian indicator and ecosystem names. 
#' @param refYearCode integer. The year to use as reference year, where 0 = 
#' reference year as set in the database.  
#'
#' @return a list containing a combined dataset for ecosystems coast or ocean.
#' @export
#'
#' @examples

importMergeDataset_CoastOcean <- function(ecosystem_part, ecosystem, username, password, year, norwegian, refYearCode){
  
  ## Import all listed datasets
  importData <- list()
  for(i in 1:length(ecosystem_part)){
    importData[[i]] <- importDatasetApi(username = NIdb_username,
                                        password = NIdb_password,
                                        eco = ecosystem_part[i],
                                        year = years,
                                        norwegian = norwegianNames,
                                        refYearCode = refYearCode)
  }
  
  
  ## Combine indicator data
  
  # Indicator list
  indicators <- merge(importData[[1]]$indicators, importData[[2]]$indicators,
                      by = intersect(names(importData[[1]]$indicators), names(importData[[2]]$indicators)),
                      all = TRUE)
  
  indicators$mergedData <- NA
  colnames(indicators)[which(colnames(indicators) == "mergedData")] <- ecosystem
  
  indicators[, which(colnames(indicators) == ecosystem)] <- rowSums(data.frame(eval(parse(text = paste0("indicators$`", ecosystem_part[[1]], "`"))),
                                                                               eval(parse(text = paste0("indicators$`", ecosystem_part[[2]], "`")))), 
                                                                    na.rm = TRUE)
  
  indicators <- indicators[,!(names(indicators) %in% ecosystem_part)]
  
  # ICunits
  ICunits <- unique.data.frame(rbind(importData[[1]]$ICunits,
                                     importData[[2]]$ICunits))
  
  # Reference values
  referenceValues1 <- unique.data.frame(rbind(importData[[1]]$referenceValues$referenceValues,
                                              importData[[2]]$referenceValues$referenceValues))
  
  referenceValues2 <- c(importData[[1]]$referenceValues$customDistributions,
                        importData[[2]]$referenceValues$customDistributions)
  
  referenceValues <- list("referenceValues" = referenceValues1,
                          "customDistributions" = referenceValues2)
  
  remove(referenceValues1, referenceValues2)
  
  # Indicator observations
  indicatorObservations1 <- unique.data.frame(rbind(importData[[1]]$indicatorObservations$indicatorValues,
                                                    importData[[2]]$indicatorObservations$indicatorValues))
  
  indicatorObservations2 <- c(importData[[1]]$indicatorObservations$customDistributions,
                              importData[[2]]$indicatorObservations$customDistributions)
  
  indicatorObservations <- list("indicatorValues" = indicatorObservations1,
                                "customDistributions" = indicatorObservations2)
  
  remove(indicatorObservations1, indicatorObservations2)
  
  # BSunits
  BSunits <- merge(importData[[1]]$BSunits, importData[[2]]$BSunits,
                   by = intersect(names(importData[[1]]$BSunits), names(importData[[2]]$BSunits)),
                   all = TRUE)
  
  BSunits$mergedData <- NA
  colnames(BSunits)[which(colnames(BSunits) == "mergedData")] <- ecosystem
  
  BSunits[, which(colnames(BSunits) == ecosystem)] <- rowMeans(data.frame(eval(parse(text = paste0("BSunits$`", ecosystem_part[[1]], "`"))),
                                                                          eval(parse(text = paste0("BSunits$`", ecosystem_part[[2]], "`")))),
                                                      na.rm = TRUE)
  
  BSunits <- BSunits[, !(names(BSunits) %in% ecosystem_part)]
  
  # Ecosystem info
  ecosystems <- importData[[1]]$ecosystems
  ecosystems$id <- ifelse(ecosystem == "Kystvann", 200, 100)
  ecosystems$name <- ecosystem
  
  ## Reassemble data
  ImportData <- niDataImport(indicators = indicators,
                             referenceValues = referenceValues,
                             indicatorObservations = indicatorObservations,
                             ICunits = ICunits,
                             BSunits = BSunits,
                             ecosystems = ecosystems)
  
  ## Coast only: adjust fidelities (?)
  if(ecosystem == "Kystvann"){
    ImportData$indicators$Kystvann <- (ImportData$indicators$Kystvann * 0) + 100
    ImportData$indicators$Kystvann[ImportData$indicators$name == "Bløtbunn artsmangfold fauna kyst"] <- 50
  }
  
  ## Return merged data
  return(ImportData)
}