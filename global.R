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

require(grid)
require(gtable)
require(scales)
