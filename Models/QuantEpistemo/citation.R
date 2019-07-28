setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/QuantEpistemo'))

library(dplyr)
library(igraph)
library(glue)
library(reshape2)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))
source('functions.R')


edge_file = 'data/export/persp_20190724_links.csv'
node_file = 'data/export/persp_20190724.csv'

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
lang <- as.tbl(read.csv('data/export/persp_20190724_lang.csv',sep=';',header = T,colClasses = c('character','character','numeric','character')))
lang$id = trim(lang$id)
nodes <- left_join(nodes,lang[,c('id','lang')])

citation <- graph_from_data_frame(edges,vertices = nodes) #nodes[,c(2,1,3)])#3:7)])

#components(citation)$csize

citation = induced_subgraph(citation,which(components(citation)$membership==1))

#V(citation)$reduced_title = sapply(V(citation)$title,function(s){paste0(substr(s,1,30),"...")})
#V(citation)$reduced_title = ifelse(degree(citation)>50,V(citation)$reduced_title,rep("",vcount(citation)))

citationcorehigher = induced_subgraph(citation,which(degree(citation)>1))
while(length(which(degree(citationcorehigher)==1))>0){citationcorehigher = induced_subgraph(citationcorehigher,which(degree(citationcorehigher)>1))}

# csv export
#export_csv(citationcorehigher,'processed/core_edges.csv','processed/core_nodes.csv',V(citationcorehigher)$numHorizontalDepth)
export_gml(citationcorehigher,'processed/core.gml')
save(citationcorehigher,file='processed/core.RData')
save(citation,file='processed/full.RData')

#######
## modularity / communities

A=as_adjacency_matrix(citationcorehigher,sparse = T)
M = A+t(A)
undirected_core = graph_from_adjacency_matrix(M,mode="undirected")

# communities
set.seed(666)

com = cluster_louvain(undirected_core)
directedmodularity(com$membership,A)


d=degree(citationcorehigher,mode='in')
for(c in unique(com$membership)){
  show(paste0("Community ",c, " ; corpus prop ",length(which(com$membership==c))/vcount(undirected_core)))
  currentd=d[com$membership==c];dth=sort(currentd,decreasing = T)[10]
  show(data.frame(titles=V(citationcorehigher)$title[com$membership==c&d>dth],degree=d[com$membership==c&d>dth]))
  #show(V(rawcore)$title[com$membership==c])
}

# expert knowledge naming

citcomnames = list('23'='chinese','7'='social geography','10'='economic geography','9'='complexity',
                   '12'='regional science','4'='planning/governance', # planning / climate change / power / global city network
                   '2'= 'pattern design','27'='demography','14'='mobility',
                   '24'='transport networks','17'='microdemographics',
                   '8'='procedural modeling','3'='aquaculture',# noise
                   '6'='spatial analysis','21'='smart cities/big data',
                   '13'='french','15'='german','19'='microeconomics', # noise
                   '20'='settlement data','26'='economic development',
                   '22'='chinese housing market', # negligible
                   '18'='ireland','5'='sociology',
                   '16'='corporate political science', # noise
                   '11'='power','25'='noise','1'='trade'
                   )

# size distrib
comsizes=list()
for(k in names(citcomnames)){comsizes[k]=length(which(com$membership==as.numeric(k)))}
comsizes=unlist(comsizes)
#table(unlist(comsizes))

plot(log(1:length(comsizes)),log(sort(comsizes,decreasing = T)))

cumsum(sort(comsizes/sum(comsizes)))

summary(lm(data=data.frame(y=log(sort(comsizes[comsizes>5000],decreasing = T)),x=log(1:length(comsizes[comsizes>5000]))),y~x))


# most important communities

d=degree(citationcorehigher,mode='in')
for(k in names(sort(comsizes[comsizes>5000],decreasing = T))){
  show(paste0(k," ; ",citcomnames[k]," ; ",length(which(com$membership==k))/vcount(undirected_core)))
  #currentd=d[com$membership==k];dth=sort(currentd,decreasing = T)[10]
  #show(data.frame(titles=V(citationcorehigher)$title[com$membership==k&d>dth],degree=d[com$membership==k&d>dth]))
}

# Q : where is the evolutive urban theory ?
grep('Pour une th',V(citationcorehigher)$title,fixed=T) # 48912 91160
V(citationcorehigher)$title[91160]
com$membership[91160] # -> within complexity ! (ouf)

# redo intracom clustering for largest coms ?

largestcoms = c(12,4,10,7,9) # up to complexity which should be interesting

for(lk in largestcoms){
  export_gml(induced.subgraph(citationcorehigher,which(com$membership==lk)),paste0('processed/core_20190724_com',lk,'.gml'))
}

for(lk in largestcoms){
  A=as_adjacency_matrix(induced.subgraph(citationcorehigher,which(com$membership==lk)),sparse = T)
  M = A+t(A)
  current = graph_from_adjacency_matrix(M,mode="undirected")
  set.seed(42)
  currentcom = cluster_louvain(current)
  show(paste0(lk," ; ",citcomnames[as.character(lk)]," ; ",directedmodularity(currentcom$membership,A)))
}

#[1] "12 ; regional science ; 0.493388803903234"
#[1] "4 ; mixed ; 0.631183609526042"
#[1] "10 ; economic geography ; 0.58951263362473"
#[1] "7 ; health ; 0.660660386301358"
#[1] "9 ; complexity ; 0.630465673412567"



#####
## relation matrix between communities

# proportion of outlinks in each other com

A=as_adjacency_matrix(citationcorehigher,sparse = T)

citprops = matrix(0,length(largestcoms),length(largestcoms))
for(i in 1:length(largestcoms)){
  alloutcits = sum(A[com$membership==largestcoms[i],])
  for(j in 1:length(largestcoms)){
    show(paste0(i,",",j))
    citprops[i,j] = sum(A[com$membership==largestcoms[i],com$membership==largestcoms[j]])/alloutcits
  }
}

#             [,1]       [,2]       [,3]        [,4]        [,5]
#[1,] 0.892475893 0.02592497 0.04915187 0.004230893 0.002682433
#[2,] 0.051480001 0.80323693 0.01959401 0.043524832 0.017072916
#[3,] 0.057678991 0.01899551 0.84120626 0.010753710 0.031799481
#[4,] 0.012626166 0.05663683 0.02045325 0.788649781 0.042715673
#[5,] 0.008820328 0.02694892 0.06868596 0.047948805 0.773843036

# 1-rowSums(citprops)
# 0.02553394 0.06509131 0.03956605 0.07891830 0.07375296
# 



#######
# Language statistics
# (full netrwork)

# TODO


#######
# Composition of chapters in terms of communities / overlaps

load('processed/core.RData')

resdir = paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Results/QuantEpistemo/Analysis/')

chapters = c("ecogeo","southafrica","butterflies","topology","scalingurban","scalinglaws",
"econophysics","cage","simpopnet","china","complexities","bubble","definingcomplexity","urbansprawl")

# 1) is there a significant unbalance in coverage ?

#cit = citation
cit=citationcorehigher

length(which(V(cit)$depth==2))
# citation : 391
# citationcorehigher : 386

# layer filling
100*length(which(V(cit)$depth=='2'&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==2))
100*length(which(V(cit)$depth=='1'&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==1))
# citation : 96.16368 ; 33.83287
# citationcorehigher : 96.11399 ; 44.21122

#100*length(which(V(cit)$depth==0&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==0))
#100*length(which(V(cit)$depth==-1&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==-1))
# 0 (OK - getUnfilled always takes depth greater than 0)

# chapter balance

for(chapter in chapters){
  show(chapter)
  show(100*length(which(V(cit)$depth>=1&!is.na(get.vertex.attribute(cit,chapter))&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth>=1&!is.na(get.vertex.attribute(cit,chapter)))))
}

# citation
# "ecogeo" 33.47486 ; "southafrica"  38.08844 ; "butterflies" 38.35497 ; "topology" 37.35292 ; "scalingurban" 35.8332 ; "scalinglaws" 34.70147 "econophysics" 37.2807
# "cage" 37.55958  "simpopnet" 38.55374 "china" 38.32114 "complexities" 39.72612 "bubble" 37.3112 "definingcomplexity" 34.00956 "urbansprawl" 38.8705

# citationcorehigher
#"ecogeo" 39.5998 "southafrica" 42.18203 "butterflies" 40.93395 "topology" 40.35152 "scalingurban" 40.39993"scalinglaws" 39.5489 "econophysics" 40.04453
# "cage" 40.28425 "simpopnet" 40.57152 "china" 41.36232 "complexities" 42.09915 "bubble" 40.37388 "definingcomplexity" 39.52496 "urbansprawl" 40.74315


###
# 2) overlap matrix (in terms of subnetworks by propagation) between chapters

cit=citationcorehigher

# relative vs absolute ? (Jaccard sim / counts)

overlaps = matrix(0,length(chapters),length(chapters))
reloverlaps = matrix(0,length(chapters),length(chapters))
for(ch1 in 1:length(chapters)){
  for(ch2 in 1:length(chapters)){
    ch1nodes = !is.na(get.vertex.attribute(cit,chapters[ch1]))
    ch2nodes = !is.na(get.vertex.attribute(cit,chapters[ch2]))
    reloverlaps[ch1,ch2] =2 * length(which(ch1nodes&ch2nodes)) / (length(which(ch1nodes))+length(which(ch2nodes)))
    overlaps[ch1,ch2] = length(which(ch1nodes&ch2nodes))
  }
}
rownames(overlaps)<-chapters;colnames(overlaps)<-chapters
rownames(reloverlaps)<-chapters;colnames(reloverlaps)<-chapters

# heatmaps
g=ggplot(data=melt(overlaps[,rev(chapters)]),aes(x=Var1,y=Var2,fill=value))
g+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name="Absolute\nOverlap")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_overlaps.png'),width=30,height=28,units='cm')


g=ggplot(data=melt(reloverlaps[,rev(chapters)]),aes(x=Var1,y=Var2,fill=value))
g+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name="Relative\nOverlap")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_reloverlaps.png'),width=30,height=28,units='cm')


###
# 3) coverage / composition (give absolute coverage ?)
# interdisciplinarity index ?

# -> construct probability matrix
# counts of papers with hdepth not na within each community

comnums = unique(com$membership)

probasall = matrix(0,length(chapters),length(comnums))
probasd1 = matrix(0,length(chapters),length(comnums))
for(chapter in 1:length(chapters)){
  show(chapters[chapter])
  for(k in 1:length(comnums)){
    probasall[chapter,k] = length(which(com$membership==comnums[k]&!is.na(get.vertex.attribute(cit,chapters[chapter]))))/length(which(!is.na(get.vertex.attribute(cit,chapters[chapter]))))
    probasd1[chapter,k] = length(which(com$membership==comnums[k]&!is.na(get.vertex.attribute(cit,chapters[chapter]))&as.numeric(V(cit)$depth)>=1))/length(which(!is.na(get.vertex.attribute(cit,chapters[chapter]))&as.numeric(V(cit)$depth)>=1))
  }
}
rownames(probasall)<-chapters;colnames(probasall)<-citcomnames[as.character(comnums)]
rownames(probasd1)<-chapters;colnames(probasd1)<-citcomnames[as.character(comnums)]

probasall[,colMeans(probasall)>0.01]
probasd1[,colMeans(probasd1)>0.01]

# plot with signif communities
#signifcoms = 

g=ggplot(data=melt(probasall),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")
# plot deviations ? (seem rather similar with all ?)

g=ggplot(data=melt(probasd1),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")



