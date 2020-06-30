
setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/Networks'))

library(rjson)
library(igraph)

source('functions.R')

resdir = paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Results/Networks/');dir.create(resdir)


####
# create network -> cf python script


nw = fromJSON(file = '../../Data/arxiv/internal-references-v0.2.0-2019-03-01.json')
authors = unlist(fromJSON(file = '../../Data/arxiv/authors-parsed-v0.2.0-2019-03-01.json'))

#nodes = unique(names(nw))
edges = data.frame(
  from = unlist(sapply(names(nw),function(n){rep(n,length(nw[[n]]))})),
  to = unlist(nw)
)
nodes = unique(edges$from,edges$to)
#nodes = data.frame(name=nodes,authors[nodes])

#g = graph_from_data_frame(d = edges, directed = T, vertices = nodes)
g = graph_from_data_frame(d = edges, directed = T)
save(g,edges,nodes,authors, file='Data/arxiv/graph.RData')

# keep largest component
# work on core?

# -> cf python script for intercisciplinarity / cooauthorship computations

#####
# distribution of author interdisc? (papers in different communities)


library(Matrix)
library(ggplot2)

# coauthorship matrix
rows = read.csv('processed/collabprobas_rows.csv',header = F)[,1]
cols = read.csv('processed/collabprobas_cols.csv',header = F)[,1]
collabdata = read.csv('processed/collabprobas_data.csv',header = F)[,1]
collab = Matrix::sparseMatrix(i=rows,j=cols,x=collabdata,dims = c(max(rows)+1,max(cols)+1),index1=F)
rm(cols);gc()

# distrib of probas?
summary(collabdata)
summary(log(collabdata))
g=ggplot(data.frame(proba = collabdata),aes(x = proba))
ggsave(file=paste0(resdir,'collabprobas.png'),
       plot=g+geom_density()+scale_x_log10()+xlab("Coauthorship probabilities"),#+stat_function(fun = dlnorm, args = list(meanlog = mean(log(collabdata)), sdlog = sd(log(collabdata))),col='red'),
       width=18,height=15,units='cm')

###
# number of papers per author
articlenum = read.csv('processed/articlenum.csv',header=F)[,1]
# keep authors with articles in considered communities only
#articlenum = articlenum[articlenum>0]
# rank size plot
#plot(log(1:length(articlenum)),log(sort(articlenum,decreasing=T)))
# fit powerlaw?
fitDistrPowerLaw(articlenum[articlenum>0],xlab='Number of articles',ylab='CDF',file=paste0(resdir,'articlenum_fitDistrPowerLaw.png'))

# probas conditionally to number of publis
logarticlenumF = cut(log(1+articlenum),breaks = 10)
names(logarticlenumF)<-1:length(logarticlenumF)

g=ggplot(data.frame(proba = collabdata,articlenumF=logarticlenumF[1+rows]),aes(x = proba,color=articlenumF,group=articlenumF))
ggsave(file=paste0(resdir,'collabprobas_byarticclenum.png'),
       plot=g+geom_density()+scale_x_log10()+xlab("Coauthorship probabilities")+scale_color_discrete(name="log(articles)"),#+stat_function(fun = dlnorm, args = list(meanlog = mean(log(collabdata)), sdlog = sd(log(collabdata))),col='red'),
       width=18,height=15,units='cm')


# distrib of distances
histdist = read.csv(file='processed/proximityhist.csv',header=F)[,1]
# as freq
histdist = histdist/sum(histdist)
g=ggplot(data.frame(proximity=seq(from=0.001,to=1,by=0.001),frequency = histdist[2:length(histdist)]),aes(x=proximity,y=frequency))
g+geom_col(width=0.01)
ggsave(file=paste0(resdir,'proximity.png'),width=18,height=15,units='cm')


# probabilities: 34 large communities
comrows = read.csv('processed/comprobas_rows.csv',header = F)[,1]
comcols = read.csv('processed/comprobas_cols.csv',header = F)[,1]
comdata = read.csv('processed/comprobas_data.csv',header = F)[,1]
comprobas = Matrix::sparseMatrix(i=comrows,j=comcols,x=comdata,dims = c(max(comrows)+1,max(comcols)+1),index1=F)

# interdisc of authors
interdisc = 1 - rowSums(comprobas^2)
interdisc = interdisc[interdisc<1] # remove authors with no assignation
g=ggplot(data.frame(interdisc=interdisc),aes(x=interdisc))
g+geom_density()+xlab('Interdisciplinarity')+ylab('Density')
ggsave(file=paste0(resdir,'interdisciplinarity.png'),width=18,height=15,units='cm')




