#' Map index summary statistics to geospatial shapes
#'
#' This function links a range of summary metrics for the index to the relevant
#' geospatial information. Metrics include median, spread of the confidence 
#' interval (= a measure of uncertainty), mean, standard deviation, and 
#' statistical displacement (an artifact resulting from indicator scaling that 
#' may affect the index estimates.)
#' 
#' Geomapping requires shapefiles for the spatial units used by the Nature
#' Index for the different ecosystems and the different thematic indices. 
#' These are deposited in NINA's internal file system under
#' "P:/41201611_naturindeks_2021_2023_vitenskapelig_ansvar/Shapefiles".
#' Alternatively, the files can be downloaded in JSON format from the NI 
#' database API here: https://ninweb08.nina.no/NaturindeksAPI/index.html. Files
#' downloaded from here first need to be converted into shapefiles.  
#'
#' 
#' @param Index a list ontaining all information on the custom index. Object
#' "CustomIndex" in the output of calculateCustomNI(). 
#' @param year integer. Which year to map data for. 
#' @param OutputType character. The type of index to be plotted. Can be one of 
#' c("NatureIndex", "EcologicalCondition", "ThematicIndex", "CustomIndex").
#' @param ecosystem character. The ecosystem of the index. Optional argument 
#' required when OutputType = "NatureIndex" or "EcologicalCondition". Can be one 
#' of c("Skog", "Fjell", "Våtmark", "Åpent lavland", "Ferskvann", "Kystvann", 
#' "Hav"). Note that only the first three are relevant for OutputType = 
#' "EcologicalCondition" so far. 
#' @param theme character. Optional argument specifying which thematic index 
#' should be plotted. Required when OutputType = "ThematicIndex". For currently
#' supported thematic indices, see documentation of listIndicators_thematicIndex().
#' @param awBSunit character. Optional argument that needs to be provided when 
#' OutputType = "CustomIndex" to determine which spatial units should be used
#' for plotting. Can be any of c("Fjell", "Skog", "Våtmark", "Åpent lavland", 
#' "Ferskvann"), but note that the outcome is the same for all terrestrial 
#' ecosystems. 
#' @param shapeLibraryPath character. Path to the folder in which the shapefiles
#' are deposited. 
#'
#' @return an sf object containing geomapped summary statistics for the index in 
#' the specified year.
#'
#' @examples

geomapNI <- function(Index, year, OutputType, 
                     ecosystem = NULL, theme = NULL, awBSunit = NULL,
                     shapeLibraryPath){
  
  #-----------------------#
  # Shapefile preparation #
  #-----------------------#
  
  ## Complete shapefile path
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    
    ecosystem_paste <- dplyr::case_when(ecosystem %in% c("Fjell", "Skog", "Våtmark", "Åpent lavland", "Ferskvann") ~ "Terrestrial",
                                        ecosystem == "Kystvann" ~ "Coast",
                                        ecosystem == "Hav" ~ "Ocean")
    
    shapeFolder <- paste0("/NI_ecosystems/NI_ecosystem_", ecosystem_paste)
  }
  
  if(OutputType == "ThematicIndex"){
    
    if(theme %in% c("CoastalSeabirds", "OceanSeabirds")){
      stop("Seabird thematic indices are not spatially explicit and can therefore not be plotted as maps.")
    }
    
    if(theme %in% c("Acidification", "Eutrophication")){
      stop("Correct shapefiles for thematic indices 'Acidification' and 'Eutrophication' are missing at present. Plotting maps for these thematic indices is therefore not currently possible.")
    }
    
    shapeFolder <- dplyr::case_when(theme %in% c("Acidification", "Eutrophication") ~ NA,
                                    theme == "AlpinePasserines" ~ "/NI_ecosystems/NI_ecosystem_Terrestrial",
                                    theme %in% c("Amphibians", "ForestryEffects", "VascularPlants") ~ "/NI_ecosystems/NI_ecosystem_Terrestrial",
                                    theme == "PelagicCommunities" ~ "/NI_ecosystems/NI_ecosystem_Ocean")
  }
  
  if(OutputType == "CustomIndex"){
    if(is.null(awBSunit)){
      stop("Missing argument 'awBSunit'. Currently, ecosystem shapefiles are used for custom indices and the argument 'awBSunit' is required for selecting the relevan shapefile.")
    }else{
      warning("Currently, ecosystem shapefiles are used for custom indices.")
      ecosystem_paste <-  dplyr::case_when(awBSunit %in% c("Fjell", "Skog", "Våtmark", "Åpent lavland", "Ferskvann") ~ "Terrestrial",
                                           awBSunit == "Kystvann" ~ "Coast",
                                           awBSunit == "Hav" ~ "Ocean")
      
      shapeFolder <- paste0("/NI_ecosystems/NI_ecosystem_", ecosystem_paste)
    }
  }
  
  shapePath <- paste0(shapeLibraryPath, shapeFolder)
  
  
  ## Load and reformat relevant shapefile
  shp <- sf::read_sf(shapePath)
  shp <- sf::st_make_valid(shp)
  
  ## Overlay shapefile with outline of Norway
  #nor <- sf::read_sf("data/outlineOfNorway_EPSG25833.shp")
  #nor <- st_transform(nor, crs = st_crs(shp))
  #shp <- st_intersection(shp, nor)
  
  
  #-------------------------#
  # Index data reformatting #
  #-------------------------#
  
  ## List areas and abbreviations
  areaNames <- data.frame(index_output = c(rep(c("E", "S", "W", "C", "N"), 2), 
                                           "N", "C", "S", "E",
                                           rep("Sør-Norge", 4), "Nord-Norge"),
                          shapefiles = c("Østlandet", "Sørlandet", "Vestlandet", "Midt-Norge", "Nord-Norge",
                                         "Østlandet Hav", "Sørlandet Hav", "Vestlandet Hav", "Midt-Norge Hav", "Nord-Norge Hav",
                                         "Barentshavet", "Norskehavet", "Nordsjøen", "Skagerrak", # TODO: Double-check matching of ocean areas to single letter abbreviations in NI output
                                         "Østlandet", "Sørlandet", "Vestlandet", "Midt-Norge", "Nord-Norge"), 
                          ecosystem = c(rep(c("Terrestrial", "Coast"), each = 5), 
                                        rep("Ocean", 4), 
                                        rep(NA, 5)),
                          theme = c(rep(NA, 5 + 5 + 4), 
                                    rep("AlpinePasserines", 5)))
  
  ## Subset to contain relevant areas only
  if(shapeFolder == "/NI_ecosystems/NI_ecosystem_Terrestrial"){
    
    if(OutputType == "ThematicIndex"){
      
      if(theme == "AlpinePasserines"){
        areaNames <- subset(areaNames, theme == "AlpinePasserines")
      }else{
        areaNames <- subset(areaNames, ecosystem == "Terrestrial")
      }
      
    }else{
      areaNames <- subset(areaNames, ecosystem == "Terrestrial")
    }
    
  }
  
  if(shapeFolder %in% c("/NI_ecosystems/NI_ecosystem_Coast")){
    areaNames <- subset(areaNames, ecosystem == "Coast")
  }
  
  
  if(shapeFolder %in% c("/NI_ecosystems/NI_ecosystem_Ocean")){
    areaNames <- subset(areaNames, ecosystem == "Ocean")
  }
  
  
  ## Summarise index data
  sumIndex <- summary(Index)
  
  ## Calculate and add mean and standard deviation
  for(i in 1:length(Index)){
    
    mean <- sd <- rep(NA, length(Index[[i]]))

    for(t in 1:length(Index[[i]])){
      mean[t] <- mean(Index[[i]][[t]]$index)
      sd[t] <- sd(Index[[i]][[t]]$index)
    }

    sumIndex[[i]] <- cbind(sumIndex[[i]], mean, sd)
  }
  
  ## List available areas
  indexAreas <- names(sumIndex)[which(names(sumIndex)!="wholeArea")]
  
  ## Assemble index data for selected year
  indexData <- subset(areaNames, areaNames$index_output%in%indexAreas)[, c("index_output", "shapefiles")]
  indexData$displacement <- indexData$standardDev <- indexData$meanValue <- indexData$widthCI <- indexData$medianValue <- NA
  
  for(i in 1:nrow(indexData)){
    sumIndex_temp <- sumIndex[which(names(sumIndex) == indexData$index_output[i])][[1]]
    yearIdx <- which(grepl(paste0(year), rownames(sumIndex_temp), fixed = TRUE))
    
    indexData$medianValue[i] <- sumIndex_temp[yearIdx, "median"]
    indexData$widthCI[i] <- sumIndex_temp[yearIdx, 3] - sumIndex_temp[yearIdx, 1]
    indexData$meanValue[i] <- sumIndex_temp[yearIdx, "mean"]
    indexData$standardDev[i] <- sumIndex_temp[yearIdx, "sd"]
    indexData$displacement[i] <- sumIndex_temp[yearIdx, "displacement"]
  }
  
  indexData$index_output <- NULL
  
  
  #-------------------------#
  # Spatial mapping of data #
  #-------------------------#
  
  ## Add summarised index data to map data
  shp$medianValue <- indexData$medianValue[match(shp$area, indexData$shapefiles)]
  shp$widthCI <- indexData$widthCI[match(shp$area, indexData$shapefiles)]
  shp$meanValue <- indexData$meanValue[match(shp$area, indexData$shapefiles)]
  shp$standardDev <- indexData$standardDev[match(shp$area, indexData$shapefiles)]
  shp$displacement <- indexData$displacement[match(shp$area, indexData$shapefiles)]
  
  ## Return geomapped data
  return(shp)
}
