#' Plot area-specific and area-aggregated time series of calculated indices
#'
#' This function is an extended version of NIcalc::plot.NiSeries() which simultaneously plots indices calculated for multiple areas.
#' 
#' @param Index a list ontaining all information on the custom index. Object
#' "CustomIndex" in the output of calculateCustomNI(). 
#' @param addAverage logical. If TRUE (default), a line for the area-averaged index is plotted together with area-specific estimates. 
#' @param truncateY logical. If TRUE, the y axis of the plot will be truncated to only visualize the range of values relevant to the specific index. If FALSE (default), the y-axis will span the entire possible range from 0 to 1. 
#'
#' @returns a time series plot. 
#' @export
#'
#' @examples
#' 
plotNI_StandardTS <- function(Index, addAverage = TRUE, truncateY = FALSE){
  
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
  
  ## Summarise index distributions
  IndexData_sum <- IndexData %>%
    dplyr::group_by(Area, Year) %>%
    dplyr::summarise(median = median(Index),
                     lCI = quantile(Index, probs = 0.025),
                     uCI = quantile(Index, probs = 0.975), 
                     .groups = "keep") %>%
    dplyr::ungroup()
  
  ## Assign area names and convert year to number
  IndexData_sum <- IndexData_sum %>%
    dplyr::mutate(Area = dplyr::case_when(Area == "wholeArea" ~ "All Norway",
                                          Area == "E" ~ "Eastern Norway",
                                          Area == "S" ~ "Southern Norway",
                                          Area == "W" ~ "Western Norway",
                                          Area == "N" ~ "Northern Norway",
                                          Area == "C" ~ "Central Norway",
                                          TRUE ~ Area),
                  Year = as.numeric(Year))
  
  ## Drop/flag whole country average
  if(!addAverage){
    IndexData_sum <- IndexData_sum %>%
      dplyr::filter(Area != "All Norway")
  }
  
  IndexData_sum <- IndexData_sum %>%
    dplyr::mutate(areaAgg = ifelse(Area == "All Norway", TRUE, FALSE))

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
  
  ## Define color palette
  IndTS_cols <- c("#006964", paletteer::paletteer_c("grDevices::Sunset", length(areas))[-1])

  #IndTS_cols <- c("#006964", paletteer::paletteer_c("pals::kovesi.isoluminant_cgo_80_c38", length(areas)-1))

  if(!addAverage){
    IndTS_cols <- IndTS_cols[-1]
  }
  
  #-----------------#
  # Multi-area plot #
  #-----------------#
  
  ## Plot time series per area

  availYears <- unique(IndexData_sum$Year)
  
  outPlot <- ggplot2::ggplot(IndexData_sum, ggplot2::aes(x = .data$Year, y = .data$median, color = Area)) +
    ggplot2::geom_line(ggplot2::aes(linetype = areaAgg)) + 
    ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), alpha = 0.5, width = 0.5) + 
    ggplot2::geom_point(ggplot2::aes(shape = areaAgg), size = 3) +
    ggplot2::scale_color_manual(values = IndTS_cols) + 
    ggplot2::scale_shape_manual(values = c(19, 15), name = "", labels = c("area-specific", "area-aggregated")) + 
    ggplot2::scale_linetype_manual(values = c("dashed", "solid"), name = "", labels = c("area-specific", "area-aggregated")) + 
    ggplot2::ggtitle(plotTitle) + 
    ggplot2::xlab("Year") + ggplot2::ylab("Index value") + 
    ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
    ggplot2::theme_classic() + 
    ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                   panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted"))
  
  ## Return plot with correct axis
  if(truncateY){
    return(outPlot)
  }else{
    return(outPlot + ggplot2::scale_y_continuous(limits = c(0, 1)))
  }
}
