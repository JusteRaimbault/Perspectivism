
import json,sys,csv,pickle,random,numpy
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

    # export raw article num
    articlenumarray = numpy.repeat(0,len(rawauthorskeys))
    for a in articlenum.keys():
        articlenumarray[a] = articlenum[a]
    open('processed/articlenum.csv','w').writelines([str(n)+'\n' for n in articlenumarray])

    for i in articlenum.keys():
        articlenum[i] = 1/articlenum[i]

    print('Constructing sparse matrices')
    print('probas mat')
    print('probas dico size = '+str(len(probasdico)))
    probasmat = scipy.sparse.csc_matrix((list(probasdico.values()), ([k[0] for k in probasdico.keys()], [k[1] for k in probasdico.keys()])),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    print(probasmat.shape)
    del probasdico
    print('article num mat')
    print('article num size = '+str(len(articlenum)))
    articlenummat = scipy.sparse.csc_matrix((list(articlenum.values()),(list(articlenum.keys()),list(articlenum.keys()))),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    print(articlenummat.shape)
    del articlenum

    #print('Multiplying')
    #probas = articlenummat.dot(probasmat)
    #del articlenummat
    #del probasmat

    #print('Proximity')
    #print(probas.shape)
    #print(probas.nnz)
    #print(' -> transposing')
    #probtrans = probas.transpose()
    #print(' -> computing col by col')
    ##proximity = probas.dot(probtrans)
    #ncol = probtrans.shape[1]
    #
    ## need the collab proba sparse mat for correlations
    #collabmat = scipy.sparse.csc_matrix(([float(l) for l in open('processed/collabprobas_data.csv').readlines()],([int(l) for l in open('processed/collabprobas_rows.csv').readlines()],[int(l) for l in open('processed/collabprobas_cols.csv').readlines()])),shape=(len(rawauthorskeys),len(rawauthorskeys)))
    #
    #sxy = 0
    #sxx = 0
    #syy = 0
    #sx = 0
    #sy = 0
    #
    #proximityhist = list(numpy.repeat(0,1001))
    ##bycomproximityhists = {}
    ##for com in coms: bycomproximityhists[com] = numpy.repeat(0,1001)
    ##bycomproximityhists = list(numpy.repeat(list(numpy.repeat(0,1001)),len(coms),axis=0))
    #bycomproximityhists = numpy.zeros((len(coms),1001)).tolist()
    #print(bycomproximityhists)
    #
    #for j in range(0,ncol):
    #    if j%1000==0: print(str(j)+'; '+str(sxy))
    #    currentprobs = probtrans.getcol(j)
    #    currentcol = probas.dot(currentprobs).tocoo()
    #    authorcom = numpy.argmax(currentprobs.toarray())
    #    if currentcol.sum()>0:
    #        currentcollab = collabmat.getcol(j)
    #        #sxy = sxy + currentcol.dot(currentcollab.transpose())
    #        #sxx = sxx + currentcol.dot(currentcol.transpose())
    #        #syy = syy + currentcollab.dot(currentcollab.transpose())
    #        sxy = sxy + currentcol.multiply(currentcollab).sum()
    #        sxx = sxx + currentcol.power(2).sum()
    #        syy = syy + currentcollab.power(2).sum()
    #        sx = sx + currentcol.sum()
    #        sy = sy + currentcollab.sum()
    #
    #        currentcolcoo = currentcol.tocoo()
    #        for d in currentcolcoo.data:
    #            histind = int(math.floor(d*1000))
    #            proximityhist[histind] = proximityhist[histind] + 1
    #            bycomproximityhists[authorcom][histind] = bycomproximityhists[authorcom][histind] + 1
    #        proximityhist[0] = proximityhist[0] + currentcol.shape[0] - currentcolcoo.getnnz()
    #        bycomproximityhists[authorcom][0] = bycomproximityhists[authorcom][0] + currentcol.shape[0] - currentcolcoo.getnnz()
    #        #much too large to be fully written -> compute correlation and histograms on the fly
    #        #csv.writer(open('processed/proximity_data.csv','a+')).writerows([[d] for d in currentcol.data])
    #        #csv.writer(open('processed/proximity_rows.csv','a+')).writerows([[d] for d in currentcol.row])
    #        #csv.writer(open('processed/proximity_cols.csv','a+')).writerows([[d] for d in numpy.repeat(j,currentcol.nnz)])
    #    else:
    #        proximityhist[0] = proximityhist[0] + 1
    #        bycomproximityhists[authorcom][0] = bycomproximityhists[authorcom][0] + 1
    #
    #del probtrans
    ## export
    #csv.writer(open('processed/bycomproximityhists.csv','w')).writerows(bycomproximityhists)
    #csv.writer(open('processed/proximityhist.csv','w')).writerows([[d] for d in proximityhist])
    #N = len(rawauthorskeys)*len(rawauthorskeys)
    #corr = (1/(N-1)*sxy - 1/(N*(N-1))*sx*sy)/math.sqrt((1/N*sxx - math.pow(1/N*sx,2))*(1/N*syy - math.pow(1/N*sy,2)))
    #print(corr)
    #open('processed/corr.txt','w').write(str(corr))

    #print('Saving result')
    #probascoo = scipy.sparse.coo_matrix(probas)
    #del probas
    #csv.writer(open('processed/comprobas_data.csv','w')).writerows([[d] for d in probascoo.data])
    #csv.writer(open('processed/comprobas_rows.csv','w')).writerows([[d] for d in probascoo.row])
    #csv.writer(open('processed/comprobas_cols.csv','w')).writerows([[d] for d in probascoo.col])

    #proximitycoo = scipy.sparse.coo_matrix(proximity)
    #del proximity
    #csv.writer(open('processed/proximity_data.csv','w')).writerows([[d] for d in proximitycoo.data])
    #csv.writer(open('processed/proximity_rows.csv','w')).writerows([[d] for d in proximitycoo.row])
    #csv.writer(open('processed/proximity_cols.csv','w')).writerows([[d] for d in proximitycoo.col])













#
