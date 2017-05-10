#################################################################
## global parameters
#################################################################

## app name
APPNAME <- sub('.*/','',getwd())

library(sendmailR)
library(shinyjs)
library(heatmaply)

library(RColorBrewer)

library(googleVis)
library(shiny)
library(shinydashboard)

library(maptools)

library(plotly)

library(colorspace)

library(ggplot2)
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




generate.GSEAplot <- function(feature.name,feature.exp,feature.geneset,genesets){
##################################################################################################
# Generates GSEAplot for a given feature (treatment, condition)
#        
# feature.name: name of the user-selected feature        
# feature.exp: gene-named vector of expression values for a given feature
# feature.geneset: data.frame contains the selected gene sets along with their NES, FDR and P-val
# genesets: the available geneset database        
##################################################################################################
        
# For the base plot
feature.exp <- feature.exp[!is.na(feature.exp)] # remove missing values
feature.exp <- feature.exp[order(feature.exp,decreasing = TRUE)]

x <- 1:length(feature.exp)
y <- unname(feature.exp)  

# positive expression
x.pos <- x[y>0]
y.pos <- y[y>0]

# negative expression
x.neg <- x[y<0]
y.neg <- y[y<0]

par(mfrow=c(2,1), mar=c(1,12,2,7))
plot(x, y, type='l', ylab= "Expression ratio",axes=F, xlab='Rank',
     main = paste0("ssGSEA feature plot for ",feature.name),
     xlim=c(1, length(x)), lwd = 2)
axis(2, las=2, lwd = 2)
axis(1, cex=.9, at=seq(0, length(x), 1e3), lwd = 2)
polygon(c(x.pos, 1),c(y.pos,0), col='red')
polygon(c(x.neg, length(x)),c(y.neg,0), col='blue')
legend('topright', 
       legend=c(paste('upregulated (n=', length(y.pos),')', sep=''), 
                paste('downregulated (n=', length(y.neg),')', sep='')), 
       fill=c('red', 'blue'), bty='n', ncol=1, cex=1.2)

##############################################
## add gene set stick diagrams
##############################################

# Get the relevant genesets
geneset.names <- sapply(genesets,function(x)x[[1]][1])
geneset.subset <- genesets[geneset.names %in% feature.geneset$gset ]

cc=0
for(gs in seq_along(geneset.subset)){
        
        ypos <- 1-cc*1/length(geneset.subset)
        
        geneset.subset.genes <- geneset.subset[[gs]][-c(1,2)]
        ##########################################
        ## match gene sets to data ranks
        locs <- match(geneset.subset.genes, names(feature.exp))
        
        ## colors
        cols <- rep('red', length(locs))
        cols[y[locs] < 0] <- 'blue'
        
        if(cc == 0)
                plot(x[locs], rep(ypos, length(locs)), ylim=c(0, 1), axes=F, ylab='', xlab='', pch='|', col=cols, xlim=c(1, length(x)) )
        else
                points(x[locs], rep(ypos, length(locs)), ylim=c(0, 1),ylab='', xlab='', pch='|', col=cols )
        
        ## gene set name
        mtext(geneset.subset[[gs]][1], side=2, at=ypos, las=2, cex=.7)
        ## ssGSEA NES,P.values and FDR
        w <- which(feature.geneset$gset == geneset.subset[[gs]][1])
        mtext(round(feature.geneset$NES[w],3), side=4, at=ypos, las=2, cex=.7, line=0, col="black")
        mtext(round(feature.geneset$P.value[w],3), side=4, at=ypos, las=2, cex=.7, line=2, col="black")
        mtext(round(feature.geneset$FDR[w],3), side=4, at=ypos, las=2, cex=.7, line=4, col="black")
        mtext('NES', side=4, at=1.1, cex=.9, las=2, line =0)
        mtext('p-val', side=4, at=1.1, cex=.9, las=2, line =2)
        mtext('FDR', side=4, at=1.1, cex=.9, las=2, line=4)
        
        cc=cc+1
}

}


####################################

generate.ssGSEAheatmap <- function(sub.results.gct, cluster.rows = FALSE, 
                                   cluster.columns = FALSE, scale = "none",
                                   FDR.cut.off = ""){
        ##################################################################################################
        # Generates GSEAheatmap of NES for a given features (treatment, condition)
        #
        # sub.results.gct: subset of results.gct based on user preferences (defaults to results.gct)
        # FDR.cut.off: character vector specifying the FDR.cut.off applied by the user
        ################################################################################################## 
        x <- as.matrix(sub.results.gct) 
        scaling <- scale
        
        trimmed.gene.set.names <- sapply(row.names(sub.results.gct), function(x){
                if(nchar(x) <45){
                        return(x)
                }else{
                        return(substr(x,1,45))
                }
        })
        
        row.names(x) <- trimmed.gene.set.names
        annt<-data.frame(Features = colnames(sub.results.gct))
        row.names(annt)<- colnames(sub.results.gct) #pheatmap requires the row names 
        limits <-  c(seq(min(x,na.rm = TRUE),0,length.out = 500),
                    seq(0.0001,(max(x,na.rm = TRUE)),length.out = 500))
        
        
        colfuncUPDN <- colorRampPalette(c("blue","white","Red"))
        
        if(scaling == "none"){
                par(mar=c(1,20,2,7))
                pheatmap(x, color=colfuncUPDN(1000), border_color = "white", cluster_rows = cluster.rows, scale = scale,
                         cluster_cols = cluster.columns,fontsize_number = 3, fontsize_row = 10, breaks = limits, 
                         fontsize_col = 10,annotation_col =annt,#annotation_colors = ann_colors, 
                         na_col = "darkgray",treeheight_col =20, main = paste0("ssGSEA heatmap, FDR.p.val < ",FDR.cut.off))
        }else{
                par(mar=c(1,20,2,7))
                pheatmap(x, color=colfuncUPDN(1000), border_color = "white", cluster_rows = cluster.rows, scale = scale,
                         cluster_cols = cluster.columns,fontsize_number = 3, fontsize_row = 10,  
                         fontsize_col = 10,annotation_col =annt,#annotation_colors = ann_colors, 
                         na_col = "darkgray",treeheight_col =20, main = paste0("ssGSEA heatmap, FDR.p.val < ",FDR.cut.off))
        }
        
        
}




