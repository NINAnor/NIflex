#' Plot area-specific and area-aggregated time series of calculated indices
#'
#' This function is an extended version of NIcalc::plot.NiSeries() which simultaneously plots indices calculated for multiple areas.
#' 
#' @param Index a list ontaining all information on the custom index. Object
#' "CustomIndex" in the output of calculateCustomNI(). 
#' @param plotTitle character. Title to display on top of plot.
#' @param plotYears numerical vector. Optional list of years for which to show data in plot. If not provided, plots all years present in input.
#' @param addAverage logical. If TRUE (default), a line for the area-averaged index is plotted together with area-specific estimates. 
#' @param onlyAverage logical. If TRUE, returns a plot of only average values (not including area-specific index values). The default is FALSE.
#' @param addRibbon logical. If TRUE (default), display a semi-transparent ribbon in addition to error bar for visualizing uncertainty. 
#' @param truncateY logical. If TRUE, the y axis of the plot will be truncated to only visualize the range of values relevant to the specific index. If FALSE (default), the y-axis will span the entire possible range from 0 to 1. 
#' @param norwegian logical. If TRUE, uses Norwegian language for plot annotation. If FALSE (default), uses English. 
#'
#' @returns a time series plot. 
#' @export
#'
#' @examples
#' 
plotNI_StandardTS <- function(Index, plotTitle, plotYears = NULL,
                              addAverage = TRUE, onlyAverage = FALSE,
                              addRibbon = TRUE, truncateY = FALSE,
                              norwegian = FALSE){
  
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
  
  if(Index$wholeArea[[1]]$calculationParameters$awBSunit == "sumEco"){
    if(any(c("Havbunn", "Hav-pelagisk") %in% colnames(Index$wholeArea[[1]]$indicatorData))){
      oceanEco <- TRUE
    }
  }
  
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
  
  
  ## If specified: subset to relevant years
  if(!is.null(plotYears)){
    IndexData_sum <- IndexData_sum %>%
      dplyr::filter(Year %in% plotYears)
  }
  
  ## Drop NAs camouflaged as 0's (assumes that there are no "TRUE" 0's)
  IndexData_sum <- IndexData_sum %>%
    dplyr::filter(!(median == 0 & lCI == 0 & uCI == 0))
  
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
    
    if("W" %in% IndexData_sum$Area){
      IndexData_sum <- IndexData_sum %>%
        dplyr::mutate(Area = dplyr::case_when(Area == "W" ~ "North Sea (W)",
                                              Area == "North Sea" ~ "North Sea (S)",
                                              TRUE ~ Area))
    }
    
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
  #IndTS_cols <- c(paletteer::paletteer_c("grDevices::Sunset", length(areas))[-1])
  IndTS_cols <- c(paletteer::paletteer_c("pals::kovesi.isoluminant_cgo_80_c38", length(areas)-1))

  allNorway_idx <- which(unique(sort(IndexData_sum$Area)) %in% c("All Norway", "All seas"))
  
  if(length(allNorway_idx) == 1){
    
    if(allNorway_idx == 1){
      IndTS_cols <- c("#006964", IndTS_cols)
    }else{
      IndTS_cols <- c(IndTS_cols[1:(allNorway_idx-1)],
                      "#006964",
                      IndTS_cols[(allNorway_idx):length(IndTS_cols)])
    }
  }
  
  ## Optional: translate area names to Norwegian
  if(norwegian){
    
    # Translate names
    IndexData_sum <- IndexData_sum %>%
      dplyr::mutate(Area = dplyr::case_when(Area == "All Norway" ~ "Hele Norge",
                                            Area == "Eastern Norway" ~ "Øst",
                                            Area == "Southern Norway" ~ "Sør",
                                            Area == "Western Norway" ~ "Vest",
                                            Area == "Northern Norway" ~ "Nord",
                                            Area == "Central Norway" ~ "Midt",
                                            Area == "All seas" ~ "Alle havområder",
                                            Area == "Skagerrak" ~ "Skagerrak",
                                            Area == "North Sea" ~ "Nordsjøen",
                                            Area == "Norwegian Sea" ~ "Norskehavet",
                                            Area == "Barents Sea" ~ "Barentshavet",
                                            TRUE ~ Area))
  }
  
  ## Set strings for plot annotation
  
  # Area legend title
  areaLegend <- dplyr::case_when(!norwegian ~ "Area",
                                 oceanEco ~ "Havområde",
                                 TRUE ~ "Landsdel")
  # Axis labels
  xlab_name <- ifelse(norwegian, "År", "Year")
  ylab_name <- ifelse(norwegian, "Indeksverdi", "Index value")
  
  # Index types
  if(norwegian){
    indType_names <- c("areal-spesifikk", "areal-aggregert")
  }else{
    indType_names <- c("area-specific", "area-aggregated")
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
      ggplot2::scale_color_manual(values = IndTS_cols, name = areaLegend) + 
      ggplot2::scale_fill_manual(values = IndTS_cols, name = areaLegend) + 
      ggplot2::scale_alpha_manual(values = c(transp_ribbon, transp_ribbon*2), guide = "none") + 
      ggplot2::scale_shape_manual(values = c(19, 15), name = "", labels = indType_names) + 
      ggplot2::scale_linetype_manual(values = c("dashed", "solid"), name = "", labels = indType_names) + 
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab(xlab_name) + ggplot2::ylab(ylab_name) + 
      ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted")) + 
      ggplot2::guides(color = guide_legend(order = 1),
                      fill = guide_legend(order = 1),
                      shape = guide_legend(order = 2),
                      linetype = guide_legend(order = 2))

  }else{
    outPlot <- ggplot2::ggplot(IndexData_sum, ggplot2::aes(x = .data$Year, y = .data$median, color = Area)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI, fill = Area), alpha = transp_ribbon, color = NA) + 
      ggplot2::geom_line() + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), alpha = transp_errorbar, width = 0.5) + 
      ggplot2::geom_point(shape = 19, size = 3) +
      ggplot2::scale_color_manual(values = IndTS_cols, name = areaLegend) + 
      ggplot2::scale_fill_manual(values = IndTS_cols, name = areaLegend) + 
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab(xlab_name) + ggplot2::ylab(ylab_name) + 
      ggplot2::scale_x_continuous(breaks = availYears, labels = availYears) + 
      ggplot2::theme_classic() + 
      ggplot2::theme(panel.grid.major.y = ggplot2::element_line(color = "grey80"),
                     panel.grid.major.x = ggplot2::element_line(color = "grey80", linetype = "dotted")) + 
      ggplot2::guides(color = guide_legend(order = 1),
                      fill = guide_legend(order = 1),
                      shape = guide_legend(order = 2),
                      linetype = guide_legend(order = 2))
    
  }
  
  #---------------------#
  # Aggregate-only plot #
  #---------------------#
  
  if(addAverage & onlyAverage){
    outPlot <- ggplot2::ggplot(subset(IndexData_sum, areaAgg), ggplot2::aes(x = .data$Year, y = .data$median)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), fill = "#006964", alpha = transp_ribbon*2, color = NA) + 
      ggplot2::geom_line(color = "#006964") + 
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lCI, ymax = .data$uCI), color = "#006964", alpha = transp_errorbar, width = 0.5) + 
      ggplot2::geom_point(shape = 15, size = 3, color = "#006964") +
      ggplot2::ggtitle(plotTitle) + 
      ggplot2::xlab(xlab_name) + ggplot2::ylab(ylab_name) + 
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
