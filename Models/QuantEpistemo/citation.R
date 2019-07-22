setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/QuantEpistemo'))

library(dplyr)
library(igraph)
library(glue)

source('functions.R')

edges <- read.csv('data/export/persp_20190628_links.csv',sep=";",header=F,colClasses = c('character','character'))
nodes <- as.tbl(read.csv('data/export/persp_20190628.csv',sep=";",stringsAsFactors = F,quote = '"',colClasses = rep('character',23)))

for(j in 10:23){nodes[,j]=as.numeric(unlist(nodes[,j]))}
