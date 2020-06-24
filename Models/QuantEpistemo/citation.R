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

load('processed/core.RData')

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

citcomnames = list('23'='chinese','7'='social geography','10'='urban economics','9'='complexity',
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
for(k in names(sort(comsizes[comsizes>1000],decreasing = T))){
  show(paste0(k," ; ",citcomnames[k]," ; ",100*length(which(com$membership==k))/vcount(undirected_core)))
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

langtab = table(V(citation)$lang)
langtab = langtab/sum(langtab)
100*langtab[langtab>0.01]
#  Chinese    English     French     German Portuguese    Spanish 
# 4.178408  80.865864   2.032443   2.276329   2.022842   2.381766 

#######
# Composition of chapters in terms of communities / overlaps

load('processed/core.RData')

resdir = paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Results/QuantEpistemo/Analysis/')

chapters = c("ecogeo","southafrica","butterflies","topology","scalingurban","scalinglaws",
"econophysics","cage","simpopnet","china","complexities","bubble","definingcomplexity","urbansprawl")

chapternames = list("ecogeo"="14-ecogeo","southafrica"="11-southafrica","butterflies"="8-butterflies",
                    "topology"="5-topology","scalingurban"="4-scalingurban",
                    "scalinglaws"="3-scalinglaws",
                    "econophysics"="6-econophysics","cage"="7-cage",
                    "simpopnet"="13-simpopnet","china"="9-china",
                    "complexities"="2-complexities","bubble"="10-bubble"
                    ,"definingcomplexity"="1-definingcomplexity","urbansprawl"="12-urbansprawl")
sortedchapters = c("1-definingcomplexity","2-complexities","3-scalinglaws","4-scalingurban","5-topology",
                   "6-econophysics","7-cage","8-butterflies","9-china","10-bubble","11-southafrica",
                   "12-urbansprawl","13-simpopnet","14-ecogeo")

# 1) is there a significant unbalance in coverage ?

#cit = citation
cit=citationcorehigher

length(which(V(cit)$depth==2))
# citation : 391
# citationcorehigher : 386

# layer filling
100*length(which(V(cit)$depth=='2'&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==2))
100*length(which(V(cit)$depth=='1'&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth==1))
100*length(which(V(cit)$depth<2&degree(cit,mode = 'in')>0)) / length(which(V(cit)$depth<2))
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
#rownames(overlaps)<-chapters;colnames(overlaps)<-chapters
#rownames(reloverlaps)<-chapters;colnames(reloverlaps)<-chapters
rownames(overlaps)<-unlist(chapternames[chapters]);colnames(overlaps)<-unlist(chapternames[chapters])
rownames(reloverlaps)<-unlist(chapternames[chapters]);colnames(reloverlaps)<-unlist(chapternames[chapters])

# heatmaps
g=ggplot(data=melt(overlaps[sortedchapters,rev(sortedchapters)]),aes(x=Var1,y=Var2,fill=value))
g+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name="Absolute\nOverlap")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_overlaps_nums.png'),width=30,height=28,units='cm')


g=ggplot(data=melt(reloverlaps[sortedchapters,rev(sortedchapters)]),aes(x=Var1,y=Var2,fill=value))
g+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name="Relative\nOverlap")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_reloverlaps_nums.png'),width=30,height=28,units='cm')


# diag(overlaps)
#ecogeo        southafrica        butterflies           topology       scalingurban        scalinglaws 
#135588             139393             126024             113672             136791             134472 
#econophysics               cage          simpopnet              china       complexities             bubble 
#119367             115367             113510             118607             133601             113303 
#definingcomplexity        urbansprawl 
#134531             113269 

# summary(diag(overlaps)/vcount(cit))
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.7095  0.7147  0.7685  0.7819  0.8426  0.8731 


interov = overlaps;diag(interov)<- 0
summary(c(interov))

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
rownames(probasall)<-unlist(chapternames[chapters]);colnames(probasall)<-citcomnames[as.character(comnums)]
rownames(probasd1)<-unlist(chapternames[chapters]);colnames(probasd1)<-citcomnames[as.character(comnums)]
probasall<-probasall[rev(sortedchapters),]
probasd1<-probasd1[rev(sortedchapters),]

#probasall[,colMeans(probasall)>0.01]
#probasd1[,colMeans(probasd1)>0.01]

# plot with signif communities
#signifcoms = 

g=ggplot(data=melt(probasall[,colMeans(probasall)>0.01]),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")+xlab("")+ylab("")+scale_fill_continuous(name="Composition\n(all)")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_composition_all_nums.png'),width=30,height=28,units='cm')
# plot deviations ? (seem rather similar with all ?)

g=ggplot(data=melt(probasd1[,colMeans(probasd1)>0.01]),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")+scale_fill_continuous(name="Composition\n(depth 1)")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_composition_d1_nums.png'),width=30,height=28,units='cm')



probasallnorm = apply(probasall[,colMeans(probasall)>0.01],2,function(x){(x-mean(x))/sd(x)})
g=ggplot(data=melt(probasallnorm),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")+xlab("")+ylab("")+scale_fill_continuous(name="Normalized\ncomposition\n(all)")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_composition_normalized_all_nums.png'),width=30,height=28,units='cm')

probasd1norm = apply(probasd1[,colMeans(probasd1)>0.01],2,function(x){(x-mean(x))/sd(x)})
g=ggplot(data=melt(probasd1norm),aes(x=Var2,y=Var1,fill=value))
g+geom_raster()+xlab("")+ylab("")+xlab("")+ylab("")+scale_fill_continuous(name="Normalized\ncomposition\n(depth 1)")+
  theme(axis.text.x = element_text(angle = 90,hjust=1))+stdtheme
ggsave(file=paste0(resdir,'chapters_composition_normalized_d1_nums.png'),width=30,height=28,units='cm')


####

# Herfindhal index (not rao stirling !) - let keep simple
summary(apply(probasd1,1,function(r){1-sum(r^2)}))




#####
##

# gephi com names

gephinodes <- as.tbl(read.csv('processed/core_nodes_gephi.csv',sep=';',colClasses = c('integer','character','character','integer','character','integer')))
gephinodes <- left_join(gephinodes,data.frame(name=V(citationcorehigher)$name,d=degree(citationcorehigher,mode='in')))

gephinodes %>% group_by(modularity_class) %>% summarize(count=100*n()/vcount(cit),
                                                        title=title[which(d==max(d))[1]],
                                                        title2=title[which(d==quantile(d,0.95))[1]]
                                                        )

gephicomnames = list(
  '0'='transport networks',# / urban physics / scaling laws
  '1'='microdemographics',
  '2'='spatial analysis',
  '3'='urban economics',
  '4'='complexity',
  '5'='social geography',
  '6'='noise',
  '7'='noise',
  '8'='mobility',
  '9'='demographics', # / urban growth
  '10'='noise',
  '11'='procedural modeling',
  '12'='noise',
  '13'='planning/governance',
  '14'='pattern design',
  '15'='noise',
  '16'='chinese',
  '17'='regional science',
  '18'='smart city'
)

#[1] "12 ; regional science ; 18.0039837642814"
#[1] "4 ; planning/governance ; 12.4824614151132"
#[1] "10 ; urban economics ; 12.3302515534175"
#[1] "7 ; social geography ; 11.5360042092604"
#[1] "9 ; complexity ; 8.93966726798958"
#[1] "2 ; pattern design ; 7.70006514331529"
#[1] "17 ; microdemographics ; 6.1034275405893"
#[1] "14 ; mobility ; 4.58070254560032"
#[1] "24 ; transport networks ; 4.18107336139507"
#[1] "6 ; spatial analysis ; 3.96935758669072"
#[1] "27 ; demography ; 3.9655993185007"
#[1] "21 ; smart cities/big data ; 2.92706454199238"
#[1] "8 ; procedural modeling ; 0.634520946081379"

#citcomnames = list('23'='chinese','7'='social geography','10'='urban economics','9'='complexity',
#                   '12'='regional science','4'='planning/governance', # planning / climate change / power / global city network
#                   '2'= 'pattern design','27'='demography','14'='mobility',
#                   '24'='transport networks','17'='microdemographics',
#                   '8'='procedural modeling','3'='aquaculture',# noise
#                   '6'='spatial analysis','21'='smart cities/big data',
#                   '13'='french','15'='german','19'='microeconomics', # noise
#                   '20'='settlement data','26'='economic development',
#                   '22'='chinese housing market', # negligible
#                   '18'='ireland','5'='sociology',
#                   '16'='corporate political science', # noise
#                   '11'='power','25'='noise','1'='trade'
#)

# reexport to have right coms

write.table(data.frame(id=V(citationcorehigher)$name,title=V(citationcorehigher)$title,year=V(citationcorehigher)$year,
                     lang=V(citationcorehigher)$lang,community=c(membership(com))
                     ),file='processed/core_nodes.csv',sep=';',col.names = T,row.names = F)
write.table(data.frame(from=tail_of(citationcorehigher,E(citationcorehigher))$name,
                       to=head_of(citationcorehigher,E(citationcorehigher))$name
                       ),file='processed/core_edges.csv',sep=';',col.names = T,row.names = F)



