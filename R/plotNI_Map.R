#' Plot maps of custom index median, confidence interval, and displacement
#'
#' This function was developed based on materials in the NIviz repository 
#' (https://github.com/NINAnor/NIviz). 
#' 
#' It allows to plot three types of summary metrics of calculated custom indices
#' onto a map: median, spread of the confidence interval (= a measure of 
#' uncertainty), and the statistical displacement (an artifact resulting from
#' indicator scaling that may affect the index estimates.)
#' At present, the function can plot any of these separately, or in pairs of two
#' (but not all three simultaneously). 
#' 
#' Plotting to map requires shapefiles for the spatial units used by the Nature
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
#' @param year integer. Which year to plot maps for. 
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
#' @param plotMedian logical. If TRUE (default), plots index median. 
#' @param plotCI logical. If TRUE (default), plots index confidence interval. 
#' @param plotDisplacement logical. If TRUE (default = FALSE) plots index 
#' dislocation. 
#' @param interactiveMap logical. If FALSE (default) plots a static map. If TRUE,
#' plots an interactive (zoomable) map. 
#'
#' @return one or two static or interactive maps of index summary metrics.
#' @export
#'
#' @examples

plotNI_Map <- function(Index, year, OutputType, 
                       ecosystem = NULL, theme = NULL, awBSunit = NULL,
                       shapeLibraryPath,
                       plotMedian = TRUE, plotCI = TRUE, plotDisplacement = FALSE, interactiveMap = FALSE){
  
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
  
  ## List available areas
  indexAreas <- names(sumIndex)[which(names(sumIndex)!="wholeArea")]
  
  ## Assemble index data for selected year
  indexData <- subset(areaNames, index_output%in%indexAreas)[, c("index_output", "shapefiles")]
  indexData$displacement <- indexData$widthCI <- indexData$medianValue <- NA
  
  for(i in 1:nrow(indexData)){
    sumIndex_temp <- sumIndex[which(names(sumIndex) == indexData$index_output[i])][[1]]
    yearIdx <- which(grepl(paste0(year), rownames(sumIndex_temp), fixed = TRUE))
    
    indexData$medianValue[i] <- sumIndex_temp[yearIdx, "median"]
    indexData$widthCI[i] <- sumIndex_temp[yearIdx, 3] - sumIndex_temp[yearIdx, 1]
    indexData$displacement[i] <- sumIndex_temp[yearIdx, "displacement"]
  }
  
  indexData$index_output <- NULL
  
  
  #------------------------------------#
  # Spatial mapping of data & plotting #
  #------------------------------------#
  
  ## Add summarised index data to map data
  shp$medianValue <- indexData$medianValue[match(shp$area, indexData$shapefiles)]
  shp$widthCI <- indexData$widthCI[match(shp$area, indexData$shapefiles)]
  shp$displacement <- indexData$displacement[match(shp$area, indexData$shapefiles)]

  ## Define color palette for Nature Index maps
  IndMap_cols <- c("#A44B4B", "#EA4B4B", "#FD7F4B", "#FDC44B", "#F0FD58",
                   "#A9FD9F", "#4BCFFD", "#4B8AFD", "#4B4BF6", "#4B4BAF")

  ## Set up colour palettes with 10 colours
  pal1 <- grDevices::colorRampPalette(IndMap_cols)(10)
  pal2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Reds"))(5)
  pal3 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Purples"))(5)
  
  ## Plot map of median values
  Map1 <- tm_shape(shp) +
    tm_polygons(col = "medianValue", 
                border.col = ifelse(interactiveMap, "white", "black"),
                #style = "cont",
                breaks = seq(0, 1, length.out = 11),
                palette = pal1)
  
  ## Plot map of CI widths (uncertainty)
  Map2 <- tm_shape(shp) +
    tm_polygons(col = "widthCI", 
                border.col = "black",
                breaks = seq(0, 0.25, length.out = 6),
                palette = pal2)
  
  ## Plot map of displacement
  Map3 <- tm_shape(shp) +
    tm_polygons(col = "displacement", 
                border.col = "black",
                breaks = seq(-0.105, 0, length.out = 8),
                palette = pal3)
  
  ## Optional: activate interactive map mode
  if(interactiveMap){
    tmap_mode("view")
  }else{
    tmap_mode("plot")
  }
  
  ## Count number of maps to plot
  mapCount <- length(which(c(plotMedian, plotCI, plotDisplacement) == TRUE))
  if(mapCount > 2){
    stop("Plotting of up to two maps together is supported at the moment. Please set FALSE for at least one of the arguments 'plotMedian', 'plotCI', and 'plotDisplacement'.")
  }
  
  ## Write map title
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    mapTitle <- paste0(ecosystem, " (", year, ")")
  }
  if(OutputType == "ThematicIndex"){
    mapTitle <- paste0(theme, " (", year, ")")
  }
  if(OutputType == "CustomIndex"){
    mapTitle <- paste0("Custom index (", year, ")")
  }
  
  ## Return specified combination of maps (single map scenario)
  if(mapCount == 1){
    outMap <- dplyr::case_when(plotMedian ~ Map1,
                               plotCI ~ Map2,
                               plotDisplacement ~ Map3)
    
    outMap <- outMap + tm_layout(title = mapTitle)
  }

  
  ## Return specified combination of maps (double map scenario)
  if(mapCount == 2){
    outMap <- dplyr::case_when(plotMedian & plotCI ~ list(Map1, Map2),
                               plotMedian & plotDisplacement ~ list(Map1, Map3),
                               plotCI & plotDisplacement ~ list(Map2, Map3))
    
    outMap <- tmap_arrange(outMap[[1]] + tm_layout(title = mapTitle), outMap[[2]] + tm_layout(title = mapTitle), 
                           sync = TRUE,
                           widths = c(1, 1),
                           heights = c(1, 1))
  }
  
  return(outMap)
  
}
