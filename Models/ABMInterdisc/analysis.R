
setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism/Models/ABMInterdisc'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))



###
# Stochasticity

res1 <- as.tbl(read.csv('exploration/20201023_113158_STOCHASTICITY_CLUSTER.csv'))
res2 <- as.tbl(read.csv('exploration/20201023_191545_STOCHASTICITY_CLUSTER.csv'))
res2$id = res2$id + length(unique(res1$id))
res = rbind(res1,res2)

resdir = '../../Results/ABMInterdisc/20201023_STOCHASTICITY_CLUSTER/';dir.create(resdir)

set.seed(42)
g=ggplot(res[res$id %in% sample(unique(res$id),20),],aes(x=depth,fill=id,group=id))
g+geom_density(alpha=0.3)+stdtheme
ggsave(file=paste0(resdir,'depth_sample10-seed42.png'),width=20,height=18,units='cm')

g=ggplot(res[res$id %in% sample(unique(res$id),20),],aes(x=interdisciplinarity,fill=id,group=id))
g+geom_density(alpha=0.3)+stdtheme
ggsave(file=paste0(resdir,'interdisciplinarity_sample10-seed42.png'),width=20,height=18,units='cm')

# sharpes 
sres = res %>% group_by(id) %>% summarize(
  depthSd = sd(depth),depthSharpe = abs(mean(depth))/sd(depth),depth=mean(depth),
  interdisciplinaritySd =sd(interdisciplinarity) ,interdisciplinaritySharpe = abs(mean(interdisciplinarity))/sd(interdisciplinarity),interdisciplinarity=mean(interdisciplinarity),
  nwEffectiveCorrSharpe = abs(mean(nwEffectiveCorr))/sd(nwEffectiveCorr),
  maxDegreeSharpe = abs(mean(maxDegree))/sd(maxDegree)
)
summary(sres)

# distances between averages in comparison of std
# 2 * |mu_i - mu_j| / (sigma_i + sigma_j)
reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}
summary(reldistance("depth","depthSd"))
summary(reldistance("interdisciplinarity","interdisciplinaritySd"))







