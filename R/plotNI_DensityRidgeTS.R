plotNI_DensityRidgeTS <- function(Index, OutputType, 
                                  ecosystem = NULL, theme = NULL,
                                  allAreas = FALSE, selectedArea  = "wholeArea"){
  
  #-------------------#
  # Data reformatting #
  #-------------------#
  
  ## List areas
  areas <- names(Index)
  
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
  
  
  #------------------#
  # Single area plot #
  #------------------#
  
  if(!allAreas){
    
    ## Check if selected area is in data
    if(!(Area %in% IndexData$Area)){
      stop("Selected area is not available for index. You can check for available areas using names(Index).")
    }
    
    ## Subset data to selected area
    IndexData.sub <- subset(IndexData, Area == selectedArea)
    
    ## Plot density ridge time series for data subset
    
    # Set mapping for custom color scale
    valuesMap <- c(-0.1, seq(0, 1, length.out = 10))
    colorMap <- c("#1F8C81", NIviz_colours$IndMap_cols)
    
    # Plot densities
    outPlot <- ggplot(IndexData.sub, aes(x = Index, y = Year, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) + 
      scale_fill_gradientn(colours = colorMap,
                           values = valuesMap,
                           limits = c(0, 1)) +
      scale_y_discrete(limits = rev) + 
      ggtitle(paste0(plotTitle, " (area: ", selectedArea, ")")) + 
      xlab("Index value") + ylab("") + 
      theme_classic() + 
      theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5),
            axis.line.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey80"))
    
  }
  
  
  #-----------------#
  # Multi-area plot #
  #-----------------#
  
  if(allAreas){
    
    ## Plot density ridge time series per area
    
    # Set mapping for custom color scale
    valuesMap <- c(-0.1, seq(0, 1, length.out = 10))
    colorMap <- c("#1F8C81", NIviz_colours$IndMap_cols)
    
    # Plot densities
    outPlot <- ggplot(IndexData, aes(x = Index, y = Year, fill = stat(x))) +
      geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01, quantile_lines = TRUE, quantiles = 2) + 
      scale_fill_gradientn(colours = colorMap,
                           values = valuesMap,
                           limits = c(0, 1)) +
      scale_y_discrete(limits = rev) + 
      facet_wrap(~ Area) +
      ggtitle(plotTitle) + 
      xlab("Index value") + ylab("") + 
      theme_classic() + 
      theme(legend.title = element_blank(), plot.title = element_text(hjust = 0.5),
            axis.line.y = element_blank(), axis.ticks.y = element_blank(),
            panel.grid.major.y = element_line(color = "grey80"))
    
  }
  
  return(outPlot)
}
