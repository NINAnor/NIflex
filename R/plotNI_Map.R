

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
    
    if(theme %in% c("Acidification", "Eutrophication", "AlpinePasserines")){
      stop("Shapefiles for thematic indices 'Acidification', 'Eutrophication', and 'AlpinePasserines have not been added yet. Plotting maps for these thematic indices is therefore not possible.")
    }
    
    shapeFolder <- dplyr::case_when(theme %in% c("Acidification", "Eutrophication") ~ NA,
                                    theme == "AlpinePasserines" ~ NA,
                                    theme %in% c("Amphibians", "ForestryEffects", "VascularPlants") ~ "/NI_ecosystems/NI_ecosystem_Terrestrial",
                                    theme == "PelagicCommunities" ~ "/NI_ecosystems/NI_ecosystem_Ocean")
    
    # TODO: Download and convert shapefiles for missing thematic indices, then update code for selection
  }
  
  if(OutputType == "CustomIndex"){
    if(is.null(awBSunit)){
      stop("Missing argument 'awBSunit'. Currently, ecosystem shapefiles are used for custom indices and the argument 'awBSunit' is required for selecting the relevan shapefile.")
    }else{
      warning("Currently, ecosystem shapefiles are used for custom indices.")
      shapeFolder <-  dplyr::case_when(awBSunit %in% c("Fjell", "Skog", "Våtmark", "Åpent lavland", "Ferskvann") ~ "Terrestrial",
                                       awBSunit == "Kystvann" ~ "Coast",
                                       awBSunit == "Hav" ~ "Ocean")
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
  areaNames <- data.frame(index_output = c(rep(c("E", "S", "W", "C", "N"), 2), "N", "C", "S", "E"),
                          shapefiles = c("Østlandet", "Sørlandet", "Vestlandet", "Midt-Norge", "Nord-Norge",
                                         "Østlandet Hav", "Sørlandet Hav", "Vestlandet Hav", "Midt-Norge Hav", "Nord-Norge Hav",
                                         "Barentshavet", "Norskehavet", "Nordsjøen", "Skagerrak"),
                          ecosystem = c(rep(c("Terrestrial", "Coast"), each = 5), rep("Ocean", 4)),
                          theme = c(rep(NA, 5 + 5 + 4)))
  
  ## Subset to contain relevant areas only
  if(shapeFolder == "/NI_ecosystems/NI_ecosystem_Terrestrial"){
    areaNames <- subset(areaNames, ecosystem == "Terrestrial")
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


  ## Set up colour palettes with 10 colours
  pal1 <- grDevices::colorRampPalette(NIviz_colours[["IndMap_cols"]])(10)
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
    paste0("Custom index (", year, ")")
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
    
    outMap <- tmap_arrange(outMap[[1]], outMap[[2]], 
                           sync = TRUE,
                           widths = c(1, 1),
                           heights = c(1, 1))
  }
  
  return(outMap)
  
}
