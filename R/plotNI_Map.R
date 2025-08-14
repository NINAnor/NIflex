#' Plot maps of custom index median, confidence interval, and displacement
#'
#' This function allows to plot three types of summary metrics of calculated 
#' custom indices onto a map: median, spread of the confidence interval (= a 
#' measure of uncertainty), and the statistical displacement (an artifact 
#' resulting from indicator scaling that may affect the index estimates.)
#' At present, the function can plot any of these separately, or in pairs of two
#' (but not all three simultaneously). 
#' 
#' #' This function was developed based on materials in the NIviz repository 
#' (https://github.com/NINAnor/NIviz). 
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
#' @param shp an sf object containing geomapped summary statistics for the index in 
#' the specified year.
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

plotNI_Map <- function(shp, year, OutputType, 
                       ecosystem = NULL, theme = NULL, 
                       plotMedian = TRUE, plotCI = TRUE, plotDisplacement = FALSE, interactiveMap = FALSE){
  
  
  #-----------------#
  # Plotting to map #
  #-----------------#

  ## Define color palette for Nature Index maps
  IndMap_cols <- c("#A44B4B", "#EA4B4B", "#FD7F4B", "#FDC44B", "#F0FD58",
                   "#A9FD9F", "#4BCFFD", "#4B8AFD", "#4B4BF6", "#4B4BAF")

  ## Set up colour palettes with 10 colours
  pal1 <- grDevices::colorRampPalette(IndMap_cols)(10)
  pal2 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "Reds"))(5)
  pal3 <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = "PRGn"))(5)
  
  ## Plot map of median values
  Map1 <- tmap::tm_shape(shp) +
    tmap::tm_polygons(col = "medianValue", 
                border.col = "black",
                #style = "cont",
                breaks = seq(0, 1, length.out = 11),
                palette = pal1) 

  ## Plot map of CI widths (uncertainty)
  Map2 <- tmap::tm_shape(shp) +
    tmap::tm_polygons(col = "widthCI", 
                border.col = "black",
                breaks = seq(0, 0.25, length.out = 6),
                palette = pal2) 

  ## Plot map of displacement
  Map3 <- tmap::tm_shape(shp) +
    tmap::tm_polygons(col = "displacement", 
                border.col = "black",
                breaks = seq(-0.105, 0, length.out = 8),
                palette = pal3) 
  
  ## Fix legend positioning for plot mode
  legend.coord <- c(0.65, 0.6)
  
  if(!interactiveMap){
    Map1 <- Map1 +
      tmap::tm_layout(legend.position = legend.coord)
    
    Map2 <- Map2 +
      tmap::tm_layout(legend.position = legend.coord)
    
    Map3 <- Map3 +
      tmap::tm_layout(legend.position = legend.coord)
  }
  
  ## Optional: activate interactive map mode
  if(interactiveMap){
    tmap::tmap_mode("view")
  }else{
    tmap::tmap_mode("plot")
  }
  
  ## Count number of maps to plot
  mapCount <- length(which(c(plotMedian, plotCI, plotDisplacement) == TRUE))
  
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
    outMap <- dplyr::case_when(plotMedian ~ list(Map1),
                               plotCI ~ list(Map2),
                               plotDisplacement ~ list(Map3))
    
    outMap <- outMap[[1]] + tmap::tm_title(mapTitle)
  }

  
  ## Return specified combination of maps (double map scenario)
  if(mapCount == 2){
    outMap <- dplyr::case_when(plotMedian & plotCI ~ list(Map1, Map2),
                               plotMedian & plotDisplacement ~ list(Map1, Map3),
                               plotCI & plotDisplacement ~ list(Map2, Map3))
    
    outMap <- tmap::tmap_arrange(outMap[[1]] + tmap::tm_title(mapTitle), 
                                 outMap[[2]] + tmap::tm_title(mapTitle), 
                           sync = TRUE,
                           widths = c(1, 1),
                           heights = c(1, 1))
  }
  
  ## Return specified combination of maps (triple map scenario)
  if(mapCount == 3){
    outMap <- list(Map1, Map2, Map3)
    
    outMap <- tmap::tmap_arrange(outMap[[1]] + tmap::tm_title(mapTitle), 
                                 outMap[[2]] + tmap::tm_title(mapTitle), 
                                 outMap[[3]] + tmap::tm_title(mapTitle), 
                                 sync = TRUE,
                                 widths = c(1, 1, 1),
                                 heights = c(1, 1, 1))
  }
  
  return(outMap)
  
}
