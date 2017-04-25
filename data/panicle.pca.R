library(shiny)
library(d3heatmap)
library(gplots)
library(reshape)
library(dplyr)
library(RColorBrewer)
library(ggplot2)
library(shinythemes)
library(shinyRGL)
library(scatterplot3d)
library(rgl)

setwd('/Users/mgehan/Documents/github/quinoa-shiny-2017/quinoa-trait/data')

panicle <- read.csv(file="cqPanicle-Yield_20170421.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)

pca_format=function(data){
  data_sub=subset(data, select=-c(Genotype,ID,Observations))
  return(data_sub)
}

pca_nonzero=function(data_sub){
  col_sum=colSums(data_sub)
  channel_data=data_sub[,col_sum!=0]
  return(channel_data)
}

go_pca=function(channel_data){
  channel_pca=prcomp(channel_data,center=TRUE, scale=TRUE)
}

panicle1=pca_format(panicle)
pca.non=pca_nonzero(panicle1)
panicle.pca=go_pca(pca.non)
summary(panicle.pca)

panicle_scores<-as.data.frame(panicle.pca$x)
panicle_scores$Genotype=panicle$Genotype
panicle_scores$ID=panicle$ID
panicle_scores$DaysAtHarvest=panicle$DaysAtHarvest
panicle_scores$SeedWT_All_g=panicle$SeedWT_All_g
panicle_scores$Density=panicle$Density
panicle_scores$Shape=panicle$Shape

write.table(panicle_scores, file="panicle_pca_scores.csv",quote=FALSE,sep=',')
write.table(panicle.pca$rotation, file="panicle_pca_loading.csv",quote=FALSE,sep=',')