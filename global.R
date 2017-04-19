#################################################################
## global parameters
#################################################################

## app name
APPNAME <- sub('.*/','',getwd())



library(heatmaply)

library(RColorBrewer)

library(googleVis)
library(shiny)
library(shinydashboard)

library(maptools)

library(plotly)

library(colorspace)

library(dplyr)
library(ape)
library(ggrepel)

source("pheatmap.r")
source("ssGSEA.R")

require(grid)
require(gtable)
require(scales)

# Read the available gene set database

gmt <- dir(pattern = ".gmt")
if(length(gmt == 1)){
genesets <- strsplit(readLines(gmt),"\t") # read as a list and access features
}else
warning("No or multiple gene sets.") 


generate.GSEAplot <- function(feature.exp,feature.geneset){
# Generates GSEAplot for a given feature (treatment, condition)
# feature.exp: gene-named vector of expression values for a given feature
# feature.geneset: data.frame contains the selected gene sets along with their NES, FDR and P-val
        
        
}