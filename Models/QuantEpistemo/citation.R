setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/QuantEpistemo'))

library(dplyr)
library(igraph)
library(glue)

source('functions.R')

#edge_file = 'data/export/persp_20190628_links.csv'
#node_file = 'data/export/persp_20190628.csv'
edge_file = 'test_links.csv'
node_file = 'test.csv'

edges <- read.csv(edge_file,sep=";",header=F,colClasses = c('character','character'))
nodes <- as.tbl(read.csv(node_file,sep=";",stringsAsFactors = F,quote = '"',colClasses = rep('character',23)))

# numeric hdepth values (incl. NA)
for(j in 10:23){nodes[,j]=as.numeric(unlist(nodes[,j]))}
kws = names(nodes)[10:23]

# check that full nw has an hdepth
#summary(apply(nodes[,10:23],1,function(r){length(which(is.na(r)))<length(r)}))

elabels = unique(c(edges$V1,edges$V2))
empty=rep("",length(which(!elabels%in%nodes$id)))
nodes=rbind(nodes,data.frame(title=empty,id=elabels[!elabels%in%nodes$id],year=empty))#,abstract=empty,authors=empty))

nodes = cbind(nodes,numHorizontalDepth=apply(nodes[,10:23],1,function(r){min(r,na.rm=T)}))
nodes = nodes[is.finite(nodes$numHorizontalDepth),]

# add lang


citation <- graph_from_data_frame(edges,vertices = nodes) #nodes[,c(2,1,3)])#3:7)])

#components(citation)$csize

citation = induced_subgraph(citation,which(components(citation)$membership==1))

citationcorehigher = induced_subgraph(citation,which(degree(citation)>1))
while(length(which(degree(citationcorehigher)==1))>0){citationcorehigher = induced_subgraph(citationcorehigher,which(degree(citationcorehigher)>1))}
citation = citationcorehigher

# csv export
export_csv(citation,'processed/core_full_edges.csv','processed/core_full_nodes.csv',V(citation)$numHorizontalDepth)

