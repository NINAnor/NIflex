#' Plot area-specific and area-aggregated time series of calculated indices
#'
#' This function is an extended version of NIcalc::plot.NiSeries() which simultaneously plots indices calculated for multiple areas.
#' 
#' @param Index a list ontaining all information on the custom index. Object
#' "CustomIndex" in the output of calculateCustomNI(). 
#' @param plotTitle character. Title to display on top of plot.
#' @param addAverage logical. If TRUE (default), a line for the area-averaged index is plotted together with area-specific estimates. 
#' @param onlyAverage logical. If TRUE, returns a plot of only average values (not including area-specific index values). The default is FALSE.
#' @param addRibbon logical. If TRUE (default), display a semi-transparent ribbon in addition to error bar for visualizing uncertainty. 
#' @param truncateY logical. If TRUE, the y axis of the plot will be truncated to only visualize the range of values relevant to the specific index. If FALSE (default), the y-axis will span the entire possible range from 0 to 1. 
#'
#' @returns a time series plot. 
#' @export
#'
#' @examples
#' 
plotNI_StandardTS <- function(Index, plotTitle, 
                              addAverage = TRUE, onlyAverage = FALSE,
                              addRibbon = TRUE, truncateY = FALSE){
  
  #-------------------#
  # Data reformatting #
  #-------------------#
  
  ## List areas
  areas <- names(Index)
  
  ## Remove potential spaces in area names
  areas <- stringr::str_replace_all(areas, pattern = " ", replacement = "_")
  areas <- stringr::str_replace_all(areas, pattern = "-", replacement = "_")
  
  names(Index) <- areas
  
  ## Check if ecosystem = ocean
  oceanEco <- ifelse(Index$wholeArea[[1]]$calculationParameters$awBSunit == "Hav", TRUE, FALSE)
  
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
  if(oceanEco){
    IndexData_sum <- IndexData_sum %>%
      dplyr::mutate(Area = dplyr::case_when(Area == "wholeArea" ~ "All seas",
                                            Area == "E" ~ "Skagerrak",
                                            Area == "S" ~ "North Sea",
                                            Area == "C" ~ "Norwegian Sea",
                                            Area == "N" ~ "Barents Sea",
                                            TRUE ~ Area),
                    Year = as.numeric(Year))
  }else{
    IndexData_sum <- IndexData_sum %>%
      dplyr::mutate(Area = dplyr::case_when(Area == "wholeArea" ~ "All Norway",
                                            Area == "E" ~ "Eastern Norway",
                                            Area == "S" ~ "Southern Norway",
                                            Area == "W" ~ "Western Norway",
                                            Area == "N" ~ "Northern Norway",
                                            Area == "C" ~ "Central Norway",
                                            TRUE ~ Area),
                    Year = as.numeric(Year))
  }

  
  ## Drop/flag whole country average
  if(!addAverage){
    IndexData_sum <- IndexData_sum %>%
      dplyr::filter(!(Area %in% c("All Norway", "All seas")))
  }
  
  IndexData_sum <- IndexData_sum %>%
    dplyr::mutate(areaAgg = ifelse(Area %in% c("All Norway", "All seas"), TRUE, FALSE))

  ## Define color palette
  #IndTS_cols <- c("#006964", paletteer::paletteer_c("grDevices::Sunset", length(areas))[-1])
  IndTS_cols <- c("#006964", paletteer::paletteer_c("pals::kovesi.isoluminant_cgo_80_c38", length(areas)-1))

  if(!addAverage){
    IndTS_cols <- IndTS_cols[-1]
  }
  
  #-----------------#
  # Multi-area plot #
  #-----------------#
  
  ## Plot time series per area

  # Set years for axis drawing
  availYears <- unique(IndexData_sum$Year)
  
  # Set transparency parameters
  transp_errorbar <- ifelse(addRibbon, 1, 0.5)
  transp_ribbon <- ifelse(addRibbon, 0.1, 0)
  
  # Plot
  
  if(addAverage){
    
    outPlot <- ggplot2::ggplot(IndexData_sum, ggplot2::aes(x = .data$Year, y = .data$median, color = Area)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI, fill = Area, alpha = areaAgg), color = NA) + 
      ggplot2::geom_line(ggplot2::aes(linetype = areaAgg)) + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), alpha = transp_errorbar, width = 0.5) + 
      ggplot2::geom_point(ggplot2::aes(shape = areaAgg), size = 3) +
      ggplot2::scale_color_manual(values = IndTS_cols) + 
      ggplot2::scale_fill_manual(values = IndTS_cols) + 
      ggplot2::scale_alpha_manual(values = c(transp_ribbon, transp_ribbon*2), guide = "none") + 
      ggplot2::scale_shape_manual(values = c(19, 15), name = "", labels = c("area-specific", "area-aggregated")) + 
      ggplot2::scale_linetype_manual(values = c("dashed", "solid"), name = "", labels = c("area-specific", "area-aggregated")) + 
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab("Year") + ggplot2::ylab("Index value") + 
      ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted"))

  }else{
    outPlot <- ggplot2::ggplot(IndexData_sum, ggplot2::aes(x = .data$Year, y = .data$median, color = Area)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI, fill = Area), alpha = transp_ribbon, color = NA) + 
      ggplot2::geom_line() + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), alpha = transp_errorbar, width = 0.5) + 
      ggplot2::geom_point(shape = 19, size = 3) +
      ggplot2::scale_color_manual(values = IndTS_cols) + 
      ggplot2::scale_fill_manual(values = IndTS_cols) + 
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab("Year") + ggplot2::ylab("Index value") + 
      ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted"))
    
  }
  
  #---------------------#
  # Aggregate-only plot #
  #---------------------#
  
  if(addAverage & onlyAverage){
    outPlot <- ggplot2::ggplot(subset(IndexData_sum, areaAgg), ggplot2::aes(x = .data$Year, y = .data$median)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), fill = "#006964", alpha = transp_ribbon*2, color = NA) + 
      ggplot2::geom_line(color = "#006964") + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), color = "#006964", alpha = transp_errorbar, width = 0.5) + 
      ggplot2::geom_point(shape = 15, size = 3) +
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab("Year") + ggplot2::ylab("Index value") + 
      ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
      ggplot2::ylim(min(IndexData_sum$lCI), max(IndexData_sum$uCI)) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted"))
  }
   
  ## Return plot with correct axis
  
  if(truncateY){
    return(outPlot)
  }else{
    return(outPlot + ggplot2::scale_y_continuous(limits = c(0, 1)))
  }
}
