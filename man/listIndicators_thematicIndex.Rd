% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/listIndicators_thematicIndex.R
\name{listIndicators_thematicIndex}
\alias{listIndicators_thematicIndex}
\title{Make indicator lists based on thematic indices}
\usage{
listIndicators_thematicIndex(theme)
}
\arguments{
\item{theme}{character string specifying the thematic index of interest.
Currently supported are "Acidification", "AlpinePasserines", "Amphibians",
"CoastalSeabirds", "Eutrophication", "ForestryEffects", "PelagicCommunities",
"PelagicSeabirds", "VascularPlants".}
}
\value{
a character vector containing Norwegian names for the indicators that
form part of the specified thematic index.
}
\description{
A range of thematic indices have been developed and calculated as part of the
Nature Index work. Thematic indices are versions of the Nature Index
including only a subset of pre-selected indicators.
This function writes the indicator lists corresponding to established
thematic indices.
}
