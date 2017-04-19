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


####################
# Dev. purpose only
####################
feature.index <- 1 # Only one value, selected feature
gene.set.index <- 1:3 # Can be multiple values, selected genesets

feature.exp <- input.gct[,feature.index]; names(feature.exp) <- row.names(input.gct)
feature.geneset <- data.frame(gset = row.names(results.gct)[gene.set.index],
                              NES = results.gct[gene.set.index,feature.index],
                              P.value = p.values.gct[gene.set.index,feature.index],
                              FDR = fdr.gct[gene.set.index,feature.index]
                              )
feature.name <- names(input.gct)[1]

generate.GSEAplot <- function(feature.name,feature.exp,feature.geneset){
# Generates GSEAplot for a given feature (treatment, condition)
# feature.name: name of the user-selected feature        
# feature.exp: gene-named vector of expression values for a given feature
# feature.geneset: data.frame contains the selected gene sets along with their NES, FDR and P-val

# For the base plot
feature.exp <- feature.exp[!is.na(feature.exp)] # remove missing values
feature.exp <- feature.exp[order(feature.exp,decreasing = TRUE)]

x <- 1:length(feature.exp)
        
}