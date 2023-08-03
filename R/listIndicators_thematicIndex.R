#' Make indicator lists based on thematic indices 
#'
#' A range of thematic indices have been developed and calculated as part of the
#' Nature Index work. Thematic indices are versions of the Nature Index 
#' including only a subset of pre-selected indicators.
#' This function writes the indicator lists corresponding to established 
#' thematic indices. 
#' 
#' 
#' @param theme character string specifying the thematic index of interest. 
#' Currently supported are "Acidification", "AlpinePasserines", "Amphibians", 
#' "CoastalSeabirds", "Eutrophication", "ForestryEffects", "PelagicCommunities", 
#' "PelagicSeabirds", "VascularPlants". 
#'
#' @return
#' @export
#'
#' @examples

listIndicators_thematicIndex <- function(theme){
  
  
  # Freshwater acidification #
  #---------------------------#
  if(theme == "Acidification"){
    indicatorList <- c("Aure","Begroing elver forsurings indeks",
                       "Bunndyr-forsuringsindeks (Raddum 1)",
                       "Dyreplankton artssammensetning")
  }
  
  # Alpine passerines #
  #-------------------#
  if(theme == "AlpinePasserines"){
    indicatorList <- c("Blåstrupe","Fjellerke","Heipiplerke",
                       "Lappspurv","Ringtrost","Snøspurv","Steinskvett")
  }
  
  # Amphibians #
  #------------#
  if(theme == "Amphibians"){
    indicatorList <- c("Småsalamander","Buttsnutefrosk","Storsalamander")
  }
  
  # Coastal seabirds #
  #------------------#
  if(theme == "CoastalSeabirds"){
    indicatorList <- c("Fiskemåke kyst","Gråmåke","Makrellterne",
                       "Rødnebbterne","Sildemåke ssp fuscus",
                       "Sildemåke ssp intermedius", "Storjo",
                       "Storskarv ssp carbo","Storskarv ssp sinensis",
                       "Svartbak","Teist","Toppskarv","Ærfugl")
  }
  
  # Freshwater eutrophication #
  #---------------------------#
  if(theme == "Eutrophication"){
    indicatorList <- c("Vannplanter innsjø",
                       "Begroing elver eutrofierings indeks",
                       "Bunndyr-eutrofieringsindeks (ASPT)",
                       "Planteplankton innsjøer")
  }
  
  # Forestry effects #
  #------------------#
  if(theme == "ForestryEffects"){
    indicatorList <- c("Alm", "Begerfingersopp", "Bjørkefink",
                       "Blåbær", "Bokfink", "Dagsommerfugler i skog",
                       "Dompap", "Duetrost", "Eldre lauvsuksesjon (MiS)",          
                       "Elg", "Fakkeltvebladmose","Fiolgubbe",
                       "Flaggspett", "Flekkhvitkjuke", "Fossenever",
                       "Fuglekonge", "Gamle trær (MiS)", "Gammel skog",
                       "Gjerdesmett", "Granmeis", "Grønnspett",
                       "Grå fluesnapper", "Gulsanger", "Hagesanger",
                       "Hjort", "Huldresmeller", "Huldretorvmose",
                       "Humler i skog", "Hønsehauk", "Isterviersumpskog",
                       "Jernspurv", "Lappkjuke", "Liggende død ved – mengde",
                       "Liggende død ved (MiS) – arealandel", "Munk", "Måltrost",                           
                       "Nøtteskrike", "Olavsstake", "Orrfugl",                            
                       "Pelsblæremose", "Reliktbukk", "Ringdue",
                       "Rogn-Osp-Selje", "Rødstjert", "Rødstrupe",
                       "Rødvingetrost", "Rådyr", "Setertrompetmose",
                       "Sinoberbille", "smågnagere - skogbestander", "Storfugl",
                       "Storpiggslekten (sopp)", "Stående død ved – mengde", "Stående død ved (MiS) – arealandel",
                       "Svarthvit fluesnapper", "Svartmeis", "Svartsonekjuke",                     
                       "Svartspett", "Svarttrost", "Svøpfellmose",                       
                       "Toppmeis", "Trekryper", "Trepiplerke",                        
                       "Trær med hengelav (MiS)")
  }
  

  # Pelagic communities #
  #---------------------#
  if(theme == "PelagicCommunities"){
    indicatorList <- c("Blåkveite", "Grønlandssel", "Klappmyss",
                       "Kolmule", "Kveite", "Lodde",
                       "Makrell", "Øyepål", "Polartorsk",
                       "Reker hav", "Snabeluer", "Tobis",
                       "Vanlig uer", "Vågehval", "Havhest",
                       "Havsule", "Storjo", "Alke",
                       "Lomvi", "Lunde", "Laks i havet",
                       "Sild - havgående bestander", "Torsk - havsbestander", "Ungsild (1-2 år)")
  }
  
  # Pelagic seabirds #
  #------------------#
  if(theme == "PelagicSeabirds"){
    indicatorList <- c("Alke","Havhest","Havsule","Krykkje",
                       "Lomvi","Lunde","Polarlomvi")
  }
  
  
  # Wetland vascular plants #
  #-------------------------#
  if(theme == "VascularPlants"){
    indicatorList <- c("Brunmyrak","Dikesoldogg",
                       "Engmarihand","Hvitmyrak","Myrtelg",
                       "Sennegras","Smalsoldogg","Sveltstarr")
  }
  
  
  
  if(exists("indicatorList")){
    return(indicatorList)
  }else{
    stop("The specified theme does not correspond to a thematic index that is currently supported. Please check the function documentation for supported themes.")
  }
}