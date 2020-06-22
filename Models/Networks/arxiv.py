
import json,sys
import scipy.sparse
from igraph import *


task = sys.argv[1]

if task=='--network':
    print('Constructing network and communities')
    raw = json.load(open('../../Data/arxiv/internal-references-v0.2.0-2019-03-01.json'))
    ##print(raw.keys())
    ##print(raw['hep-lat/0004009'])

    ##edgelist = [(k,l) for k in raw.keys() for l in raw[k]]
    nodenames = list(set(list(raw.keys())+[l for k in raw.keys() for l in raw[k]]))
    nodes = dict([(nodenames[i],i) for i in range(0,len(nodenames))])
    edgelist = [(nodes[k],nodes[l]) for k in raw.keys() for l in raw[k]]
    ##print(edgelist)

    g = Graph(edgelist)
    ##g=Graph([('hep-lat/0004009', 'hep-lat/0002004'), ('hep-lat/0004009', 'hep-lat/9907024'), ('hep-lat/0004009', 'hep-lat/0004002'), ('hep-lat/0004009', 'hep-lat/9907028'), ('hep-lat/0004009', 'hep-lat/9912003')])
    ##print(len(g.vs))
    g.vs["name"] = nodenames

    largest = g.clusters().giant()
    ##print(largest)
    ##print(len(largest.vs))

# need to compute:
# - proximity matrix between author, based on cosine similarity between profiles in terms of communities
# - collaboration sparse mat

if task=='--collab':
    def getname(author):
        if len(author)==0: return(None)
        if len(author)==1: return(author[0])
        if len(author[1])>0:
            return(author[0]+" "+author[1][0])
        else:
            return(author[0])

    rawauthors = json.load(open('../../Data/arxiv/authors-parsed-v0.2.0-2019-03-01.json'))
    authornames = list(set([getname(a) for k in rawauthors.keys() for a in rawauthors[k]]))
    #print(authornames)
    authors = dict([(authornames[i],i) for i in range(0,len(authornames))])

    print('Authors: '+str(len(authors)))

    # construct collab sparse mat
    #collab = scipy.sparse.coo_matrix((len(authornames),len(authornames)))
    #articlenum =  scipy.sparse.coo_matrix((len(authornames),len(authornames)))
    collab = dict()
    articlenum = dict()

    k = 0

    for article in rawauthors.keys():
        k = k + 1
        if k%1000==0: print(k)
        for i in range(0,len(rawauthors[article])-1):
            for j in range(1,len(rawauthors[article])):
                i1 = authors[getname(rawauthors[article][i])]
                j1 = authors[getname(rawauthors[article][j])]
                if (i1,j1) in collab:
                    collab[(i1,j1)] = collab[(i1,j1)]+1
                else:
                    collab[(i1,j1)] = 1
                if i1 in articlenum:
                    articlenum[i1] = articlenum[i1] + 1
                else:
                    articlenum[i1] = 1
                if j1 in articlenum:
                    articlenum[j1] = articlenum[j1] + 1
                else:
                    articlenum[j1] = 1

    for i in articlenum.keys():
        articlenum[i] = 1/articlenum[i]

    print('Constructing sparse matrices')
    collabmat = scipy.sparse.csc_matrix((collab.values(), ([k[0] for k in collab.keys()], [k[1] for k in collab.keys()])))
    articlenummat = scipy.sparse.csc_matrix((articlenum.values(),(articlenum.keys(),articlenum.keys())))

    print('Multiplying')
    collabprobas = articlenummat.dot(collabmat)
    print('Saving result')
    scipy.sparse.save_npz('processed/collabprobas.npz', collabprobas)
