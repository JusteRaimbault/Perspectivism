
setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/Networks'))

library(rjson)
library(igraph)

source('functions.R')

resdir = paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Results/Networks/');dir.create(resdir)


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
rm(rows,cols);gc()

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
articlenum = articlenum[articlenum>0]
# rank size plot
plot(log(1:length(articlenum)),log(sort(articlenum,decreasing=T)))
# fit powerlaw?
fitDistrPowerLaw(articlenum,xlab='Number of articles',ylab='CDF',file=paste0(resdir,'articlenum_fitDistrPowerLaw.png'))

# probas conditionally to number of publis



# distrib of distances




