
import json,sys,csv,pickle,random
import scipy.sparse
from igraph import *


task = sys.argv[1]

def getname(author):
    if len(author)==0: return(None)
    if len(author)==1: return(author[0])
    if len(author[1])>0:
        return(author[0]+" "+author[1][0])
    else:
        return(author[0])


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

    # not needed as do not consider distance: full graph
    #largest = g.clusters().giant()
    ##print(largest)
    ##print(len(largest.vs))

    random.seed(0)
    communities = g.community_multilevel()
    membership = communities.membership
    membdico = dict(zip(nodenames,membership))
    comsizes = communities.sizes()
    print(comsizes)
    print(membership[1:10])
    pickle.dump(membdico,open('processed/commember.pkl','wb'))
    pickle.dump(comsizes,open('processed/comsizes.pkl','wb'))

# need to compute:
# - proximity matrix between author, based on cosine similarity between profiles in terms of communities
# - collaboration sparse mat

if task=='--collab':

    rawauthors = json.load(open('../../Data/arxiv/authors-parsed-v0.2.0-2019-03-01.json'))
    authornames = list(set([getname(a) for k in rawauthors.keys() for a in rawauthors[k]]))
    #print(authornames)
    authors = dict([(authornames[i],i) for i in range(0,len(authornames))])
    pickle.dump(authors,open('processed/authors.pkl','wb'))

    print('Authors: '+str(len(authors)))

    # construct collab sparse mat
    #collab = scipy.sparse.coo_matrix((len(authornames),len(authornames)))
    #articlenum =  scipy.sparse.coo_matrix((len(authornames),len(authornames)))
    collab = dict()
    articlenum = dict()

    k = 0
    rawauthorskeys = list(rawauthors.keys())

    for articlei in range(0,len(rawauthorskeys)):
        article = rawauthorskeys[articlei]
        k = k + 1
        if k%10000==0: print(k)
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
    collabmat = scipy.sparse.csc_matrix((list(collab.values()), ([k[0] for k in collab.keys()], [k[1] for k in collab.keys()])),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    articlenummat = scipy.sparse.csc_matrix((list(articlenum.values()),(list(articlenum.keys()),list(articlenum.keys()))),shape=(len(rawauthorskeys),len(rawauthorskeys)))

    print('Multiplying')
    collabprobas = articlenummat.dot(collabmat)
    ##print(collabprobas)
    ##print(collabprobas.sum(axis=0))
    print('Saving result')
    ##scipy.sparse.save_npz('processed/collabprobas.npz', collabprobas)
    collabprobascoo = scipy.sparse.coo_matrix(collabprobas)
    csv.writer(open('processed/collabprobas_data.csv','w')).writerows([[d] for d in collabprobascoo.data])
    csv.writer(open('processed/collabprobas_rows.csv','w')).writerows([[d] for d in collabprobascoo.row])
    csv.writer(open('processed/collabprobas_cols.csv','w')).writerows([[d] for d in collabprobascoo.col])

if task=='--proximity':
    rawauthors = json.load(open('../../Data/arxiv/authors-parsed-v0.2.0-2019-03-01.json'))
    authors = pickle.load(open('processed/authors.pkl','rb'))
    rawauthorskeys = list(rawauthors.keys())

    print('Proximity matrix between authors')
    commember = pickle.load(open('processed/commember.pkl','rb'))
    comsizes = pickle.load(open('processed/comsizes.pkl','rb'))
    print(len(commember))
    # 1.6Mio nodes -> com larger than 1000 (0.1%)
    coms = [i for i in range(0,len(comsizes)) if comsizes[i]>1000]
    cominds = dict(zip(coms,range(0,len(coms))))

    probasdico = dict()
    articlenum = dict()

    k = 0
    for articlei in range(0,len(rawauthorskeys)):
        article = rawauthorskeys[articlei]
        k = k + 1
        if k%10000==0: print(k)
        if article in commember:
            com = commember[article]
            if com in coms:
                comind = cominds[com]
                for author in rawauthors[article]:
                    authind = authors[getname(author)]
                    if (authind,comind) in probasdico:
                        probasdico[(authind,comind)] = probasdico[(authind,comind)] + 1
                    else:
                        probasdico[(authind,comind)] = 1
                    if authind in articlenum:
                        articlenum[authind] = articlenum[authind] + 1
                    else:
                        articlenum[authind] = 1

    for i in articlenum.keys():
        articlenum[i] = 1/articlenum[i]

    print('Constructing sparse matrices')
    print('probas mat')
    print('probas dico size = '+str(len(probasdico)))
    probasmat = scipy.sparse.csc_matrix((list(probasdico.values()), ([k[0] for k in probasdico.keys()], [k[1] for k in probasdico.keys()])),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    del probasdico
    print('article num mat')
    print('article num size = '+str(len(articlenum)))
    articlenummat = scipy.sparse.csc_matrix((list(articlenum.values()),(list(articlenum.keys()),list(articlenum.keys()))),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    del articlenum

    print('Multiplying')
    probas = articlenummat.dot(probasmat)
    del articlenummat
    del probasmat

    print('Proximity')
    proximity = probas.dot(probas.transpose())

    print('Saving result')
    probascoo = scipy.sparse.coo_matrix(probas)
    del probas
    csv.writer(open('processed/comprobas_data.csv','w')).writerows([[d] for d in probascoo.data])
    csv.writer(open('processed/comprobas_rows.csv','w')).writerows([[d] for d in probascoo.row])
    csv.writer(open('processed/comprobas_cols.csv','w')).writerows([[d] for d in probascoo.col])

    proximitycoo = scipy.sparse.coo_matrix(proximity)
    del proximity
    csv.writer(open('processed/proximity_data.csv','w')).writerows([[d] for d in proximitycoo.data])
    csv.writer(open('processed/proximity_rows.csv','w')).writerows([[d] for d in proximitycoo.row])
    csv.writer(open('processed/proximity_cols.csv','w')).writerows([[d] for d in proximitycoo.col])













#
