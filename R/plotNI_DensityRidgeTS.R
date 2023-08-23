#' Plot a density ridge time series for a custom index
#'
#' This function was developed based on materials in the NIviz repository 
#' (https://github.com/NINAnor/NIviz). 
#' 
#' It is an alternative approach for visualizing time series of indicator 
#' values including uncertainty. Using the "ggridges" package, the function
#' plots not just summary statistics but the entire index value distributions. 
#' The color code is set to match the same scale as on the Nature Index webpage. 
#'
#' @param Index a list ontaining all information on the custom index. Object
#' "CustomIndex" in the output of calculateCustomNI(). 
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
#' @param allAreas logical. If FALSE (default), makes a single-panel plot for 
#' the area specified via the argument "selectedArea". If TRUE, makes a multi-
#' panel plot with one panel per area available. 
#' @param selectedArea character. Name of the area for which to plot. Can be any
#' of the list object names of argument "Index". The default is "wholeArea". 
#'
#' @return a density ridge time-series plot. 
#' @export
#'
#' @examples

plotNI_DensityRidgeTS <- function(Index, OutputType, 
                                  ecosystem = NULL, theme = NULL,
                                  allAreas = FALSE, selectedArea  = "wholeArea"){
  
  #-------------------#
  # Data reformatting #
  #-------------------#
  
  ## List areas
  areas <- names(Index)
  
  ## Remove potential spaces in area names
  areas <- stringr::str_replace_all(areas, pattern = " ", replacement = "_")
  areas <- stringr::str_replace_all(areas, pattern = "-", replacement = "_")
  
  names(Index) <- areas
  
  ## List years
  years <- names(Index[[1]])
  
  ## Collate index value draws for each year-area combination
  IndexData <- data.frame()
  
  for(a in 1:length(areas)){
    for(t in 1:length(years)){
      
      IndexData.temp <- data.frame(Area = areas[a], 
                                   Year = years[t],
                                   Index = eval(parse(text = paste0("Index$", areas[a], "$`", years[t], "`$index"))))
      
      IndexData <- rbind(IndexData, IndexData.temp)
    }
  }
  
  
  ## Write plot title
  if(OutputType %in% c("NatureIndex", "EcologicalCondition")){
    plotTitle <- ecosystem
  }
  if(OutputType == "ThematicIndex"){
    plotTitle <- theme
  }
  if(OutputType == "CustomIndex"){
    plotTitle <- "Custom index"
  }
  
  ## Define color palette for Nature Index maps
  IndMap_cols <- c("#A44B4B", "#EA4B4B", "#FD7F4B", "#FDC44B", "#F0FD58",
                   "#A9FD9F", "#4BCFFD", "#4B8AFD", "#4B4BF6", "#4B4BAF")
  
  #------------------#
  # Single area plot #
  #------------------#
  
  if(!allAreas){
    
    ## Check if selected area is in data
    if(!(selectedArea %in% IndexData$Area)){
      stop("Selected area is not available for index. You can check for available areas using names(Index).")
    }
    
    ## Subset data to selected area
    IndexData.sub <- subset(IndexData, IndexData$Area == selectedArea)
    
    ## Plot density ridge time series for data subset
    
    # Set mapping for custom color scale
    valuesMap <- c(-0.1, seq(0, 1, length.out = 10))
    colorMap <- c("#1F8C81", IndMap_cols)
    
    # Plot densities
    outPlot <- ggplot2::ggplot(IndexData.sub, ggplot2::aes(x = .data$Index, y = .data$Year, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) + 
      ggplot2::scale_fill_gradientn(colours = colorMap,
                           values = valuesMap,
                           limits = c(0, 1)) +
      ggplot2::scale_y_discrete(limits = rev) + 
      ggplot2::ggtitle(paste0(plotTitle, " (area: ", selectedArea, ")")) + 
      ggplot2::xlab("Index value") + ggplot2::ylab("") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(legend.title = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5),
            axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "grey80"))
    
  }
  
  
  #-----------------#
  # Multi-area plot #
  #-----------------#
  
  if(allAreas){
    
    ## Plot density ridge time series per area
    
    # Set mapping for custom color scale
    valuesMap <- c(-0.1, seq(0, 1, length.out = 10))
    colorMap <- c("#1F8C81", IndMap_cols)
    
    # Plot densities
    outPlot <- ggplot2::ggplot(IndexData, ggplot2::aes(x = .data$Index, y = .data$Year, fill = after_stat(x))) +
      ggridges::geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) + 
      ggplot2::scale_fill_gradientn(colours = colorMap,
                           values = valuesMap,
                           limits = c(0, 1)) +
      ggplot2::scale_y_discrete(limits = rev) + 
      ggplot2::facet_wrap(~ Area) +
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab("Index value") + ggplot2::ylab("") + 
      ggplot2::theme_classic() + 
      ggplot2::theme(legend.title = ggplot2::element_blank(), plot.title = ggplot2::element_text(hjust = 0.5),
            axis.line.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank(),
            panel.grid.major.y = ggplot2::element_line(color = "grey80"))
    
  }
  
  return(outPlot)
}
