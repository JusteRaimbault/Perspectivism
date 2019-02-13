
setwd(paste0(Sys.getenv('CS_HOME'),'/Perspectivism'))

library(dplyr)
library(ggplot2)
source(paste0(Sys.getenv('CN_HOME'),'/Models/Utils/R/plots.R'))

resdir = 'Results/20170912_grid/';dir.create(resdir)


res <- as.tbl(read.csv('Models/exploration/2017_09_12_20_07_59_GRID.csv'))

# raw plots

# random : rho has no role
g = ggplot(res[res$nwMode==0,],aes(x=betaDC,y=interdisciplinarity,colour=alphaPreference,group=alphaPreference))
g+geom_point(pch='.')+geom_smooth()+facet_wrap(~sigmaDiscipline,scales='free')+stdtheme
ggsave(paste0(resdir,'random_interdisc-beta.png'),width=30,height=15,units='cm')

g = ggplot(res[res$nwMode==0,],aes(x=sigmaDiscipline,y=interdisciplinarity,colour=betaDC,group=betaDC))
g+geom_point(pch='.')+geom_smooth(method = 'loess')+facet_wrap(~alphaPreference)+stdtheme
ggsave(paste0(resdir,'random_interdisc-sigma.png'),width=18,height=15,units='cm')

g = ggplot(res[res$nwMode==0,],aes(x=sigmaDiscipline,y=depth,colour=betaDC,group=betaDC))
g+geom_point(pch='.')+geom_smooth(method = 'loess')+facet_wrap(~alphaPreference)+stdtheme
ggsave(paste0(resdir,'random_depth-sigma.png'),width=18,height=15,units='cm')


# pareto front
g = ggplot(res[res$nwMode==0,],aes(x=interdisciplinarity,y=depth,colour=betaDC))
g+geom_point()+stdtheme

g = ggplot(res[res$nwMode==0,],aes(x=interdisciplinarity,y=depth,colour=alphaPreference))
g+geom_point()+stdtheme


g = ggplot(res[res$nwMode==1,],aes(x=interdisciplinarity,y=depth,colour=alphaPreference))
g+geom_point()+facet_wrap(~nwCorrelation)+stdtheme

# aggregated res

sres <- res[res$nwMode==0,] %>% group_by(betaDC,alphaPreference,sigmaDiscipline)%>%summarise(
  interdisciplinaritySd=sd(interdisciplinarity),interdisciplinarity=mean(interdisciplinarity),
  depthSd=sd(depth),depth=mean(depth)
)

g = ggplot(sres,aes(x=sigmaDiscipline,y=interdisciplinarity,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=interdisciplinarity-interdisciplinaritySd,ymax=interdisciplinarity+interdisciplinaritySd))+facet_wrap(~alphaPreference)+stdtheme
ggsave(paste0(resdir,'random_interdisc-sigma.png'),width=18,height=15,units='cm')




# correlated
sres <- res[res$nwMode==1&res$alphaPreference==0.5&res$nwCorrelation==0,] %>% group_by(betaDC,alphaPreference,sigmaDiscipline,nwCorrelation)%>%summarise(
  interdisciplinaritySd=sd(interdisciplinarity),interdisciplinarity=mean(interdisciplinarity),
  depthSd=sd(depth),depth=mean(depth)
)

g = ggplot(sres,aes(x=sigmaDiscipline,y=interdisciplinarity,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=interdisciplinarity-interdisciplinaritySd,ymax=interdisciplinarity+interdisciplinaritySd))+facet_grid(nwCorrelation~alphaPreference)+stdtheme

g = ggplot(sres,aes(x=sigmaDiscipline,y=interdisciplinarity,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=interdisciplinarity-interdisciplinaritySd,ymax=interdisciplinarity+interdisciplinaritySd))+stdtheme
ggsave(paste0(resdir,'interdisc-sigma_alpha0.5_rho0.png'),width=18,height=15,units='cm')

g = ggplot(sres,aes(x=sigmaDiscipline,y=depth,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=depth-depthSd,ymax=depth+depthSd))+stdtheme
ggsave(paste0(resdir,'depth-sigma_alpha0.5_rho0.png'),width=18,height=15,units='cm')

g = ggplot(sres,aes(x=depth,y=interdisciplinarity,colour=betaDC))
g+geom_point()+stdtheme
ggsave(paste0(resdir,'pareto_alpha0.5_rho0.png'),width=18,height=15,units='cm')



g = ggplot(sres,aes(x=sigmaDiscipline,y=depth,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=depth-depthSd,ymax=depth+depthSd))+facet_grid(nwCorrelation~alphaPreference)+stdtheme


# pareto fronts
g=ggplot(sres,aes(x=interdisciplinarity,y=depth,colour=betaDC))
g+geom_point()+facet_grid(nwCorrelation~alphaPreference)+stdtheme

g=ggplot(sres,aes(x=interdisciplinarity,y=depth,colour=sigmaDiscipline))
g+geom_point()+stdtheme

# correlated relative to null model

sres <- res[res$nwMode==1,] %>% group_by(betaDC,alphaPreference,sigmaDiscipline,nwCorrelation)%>%summarise(
  interdisciplinaritySd=sd(interdisciplinarity),interdisciplinarity=mean(interdisciplinarity),
  depthSd=sd(depth),depth=mean(depth)
)

g = ggplot(sres,aes(x=sigmaDiscipline,y=interdisciplinarity,colour=betaDC,group=betaDC))
g+geom_line()+geom_point()+geom_errorbar(aes(ymin=interdisciplinarity-interdisciplinaritySd,ymax=interdisciplinarity+interdisciplinaritySd))+facet_grid(nwCorrelation~alphaPreference)+stdtheme







