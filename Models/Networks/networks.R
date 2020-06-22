
setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism'))

library(rjson)
library(igraph)

nw = fromJSON(file = 'Data/arxiv/internal-references-v0.2.0-2019-03-01.json')
authors = unlist(fromJSON(file = 'Data/arxiv/authors-parsed-v0.2.0-2019-03-01.json'))

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

# distribution of author interdisc? (papers in different communities)

