## Generate Figure 3 with hierarchical model results.
## Note that it will not be possible to run this script without first generating
## and saving the set of hierarchical models.

setwd("~/dengue-Zika-chik_Americas")
source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")
load("lat_values.Rdata")
source("Scripts/set_location_order_colors.R")


library(dplyr)
library(tidyr)
library(ggplot2)
library(rstan)
library(rstanarm)
library(grid)
library(gridExtra)

##########
## Hierarchical year effect models
##########
line.size=0.5
error.width=.4
text.size=8 #18
alpha.choice=.3

get.combined.cred.int = function(upper.prob=.975,lower,prob=.025,coef.choice){
  shared.index=which(dimnames(as.array(stan.model))$parameters==coef.choice)
  these.coefs=grep(paste0(coef.choice,' l'),names(stan.model$coefficients), value=T)
  
  stan.array=as.array(stan.model)
  
  temp.coefs.df=c()
  for(i in 1:length(these.coefs)){
    temp= stan.array[,,shared.index]+stan.array[,,which(dimnames(as.array(stan.model))$parameters==these.coefs[i])]
    temp.coefs.df=rbind(temp.coefs.df,c(mean(temp),quantile(temp,probs=c(.975,.025))))
  }
  rownames(temp.coefs.df)=these.coefs
  colnames(temp.coefs.df)=c("mean","upper","lower")
  temp.coefs.df=as.data.frame(temp.coefs.df)
  temp.coefs.df$location=unique(stan.model$data$location)
  
  temp.coefs.df$pindex = -attCountry$getPindexFromLocation(temp.coefs.df$location)
  
  temp.coefs.df=temp.coefs.df%>%
    mutate( region=factor(attCountry$getRegion(unique(stan.model$data$location)),levels=attCountry$uniqueRegions())
            ,index=1:dim(temp.coefs.df)[1])
  return(temp.coefs.df)
}

country.choice="Brazil"
attCountry <- get(paste0('att',country.choice))
absolute.inc=T
absolute.season=T

Epibulletin=T

temp2015="year2015"
temp2016="year2016"
temp2017="year2017"
  
temp.l.2015="year2015 l"
temp.l.2016="year2016 l"
temp.l.2017="year2017 l"

## Load shared-year effect hierarchical model based on absolute incidence.
# FILL

coefs.df1=get.combined.cred.int(coef.choice=temp2015)

alpha.vals1=rep(alpha.choice,length(coefs.df1$lower))
signif.index=c(which(coefs.df1$upper<0),which(coefs.df1$lower>0))
alpha.vals1[signif.index]=1

coefs.df.1=data.frame(mean=c(stan.model$stan_summary[temp2015,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2015,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2015,"2.5%"])
                      ,pindex=c(0))
alpha.vals.1=rep(alpha.choice,length(coefs.df.1$lower))
signif.index=c(which(coefs.df.1$upper<0),which(coefs.df.1$lower>0))
alpha.vals.1[signif.index]=1

coefs.df2=get.combined.cred.int(coef.choice=temp2016)

alpha.vals2=rep(alpha.choice,length(coefs.df2$lower))
signif.index=c(which(coefs.df2$upper<0),which(coefs.df2$lower>0))
alpha.vals2[signif.index]=1

coefs.df.2=data.frame(mean=c(stan.model$stan_summary[temp2016,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2016,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2016,"2.5%"])
                      ,pindex=c(0))
alpha.vals.2=rep(alpha.choice,length(coefs.df.2$lower))
signif.index=c(which(coefs.df.2$upper<0),which(coefs.df.2$lower>0))
alpha.vals.2[signif.index]=1

coefs.df3=get.combined.cred.int(coef.choice=temp2017)

alpha.vals3=rep(alpha.choice,length(coefs.df3$lower))
signif.index=c(which(coefs.df3$upper<0),which(coefs.df3$lower>0))
alpha.vals3[signif.index]=1

coefs.df.3=data.frame(mean=c(stan.model$stan_summary[temp2017,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2017,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2017,"2.5%"])
                      ,pindex=c(0))
alpha.vals.3=rep(alpha.choice,length(coefs.df.3$lower))
signif.index=c(which(coefs.df.3$upper<0),which(coefs.df.3$lower>0))
alpha.vals.3[signif.index]=1

## Add cumulative Zika model effects
# load("~/FILL/stan-models/shared_Zika/outStan_zikaIncCumu_shared_effect_group_slope_formula.Rdata")

coefs.df4=get.combined.cred.int(coef.choice="zikaIncCumu")

alpha.vals4=rep(alpha.choice,length(coefs.df4$lower))
signif.index=c(which(coefs.df4$upper<0),which(coefs.df4$lower>0))
alpha.vals4[signif.index]=1

coefs.df4[signif.index,]

coefs.df.4=data.frame(mean=c(stan.model$stan_summary["zikaIncCumu","mean"])
                      ,upper=c(stan.model$stan_summary["zikaIncCumu","97.5%"])
                      ,lower=c(stan.model$stan_summary["zikaIncCumu","2.5%"])
                      ,pindex=c(1))
alpha.vals.4=rep(alpha.choice,length(coefs.df.4$lower))
signif.index=c(which(coefs.df.4$upper<0),which(coefs.df.4$lower>0))
alpha.vals.4[signif.index]=1

## Add cumulative chik model effects
# load("~/FILL/stan-models/shared_Zika/outStan_chikIncCumu_shared_effect_group_slope_formula.Rdata")

coefs.df.5=data.frame(mean=c(stan.model$stan_summary["chikIncCumu","mean"])
                      ,upper=c(stan.model$stan_summary["chikIncCumu","97.5%"])
                      ,lower=c(stan.model$stan_summary["chikIncCumu","2.5%"])
                      ,pindex=c(1))
alpha.vals.5=rep(alpha.choice,length(coefs.df.5$lower))
signif.index=c(which(coefs.df.5$upper<0),which(coefs.df.5$lower>0))
alpha.vals.5[signif.index]=1

## Add biweek Zika model effects
# load("~/FILL/stan-models/shared_Zika/outStan_zikaInc_shared_effect_group_slope_formula.Rdata")

coefs.df.6=data.frame(mean=c(stan.model$stan_summary["zikaInc","mean"])
                      ,upper=c(stan.model$stan_summary["zikaInc","97.5%"])
                      ,lower=c(stan.model$stan_summary["zikaInc","2.5%"])
                      ,pindex=c(1))
alpha.vals.6=rep(alpha.choice,length(coefs.df.6$lower))
signif.index=c(which(coefs.df.6$upper<0),which(coefs.df.6$lower>0))
alpha.vals.6[signif.index]=1

## Add biweek chik model effects
# load("~/FILL/stan-models/shared_Zika/outStan_chikInc_shared_effect_group_slope_formula.Rdata")
coefs.df.7=data.frame(mean=c(stan.model$stan_summary["chikInc","mean"])
                      ,upper=c(stan.model$stan_summary["chikInc","97.5%"])
                      ,lower=c(stan.model$stan_summary["chikInc","2.5%"])
                      ,pindex=c(1))
alpha.vals.7=rep(alpha.choice,length(coefs.df.7$lower))
signif.index=c(which(coefs.df.7$upper<0),which(coefs.df.7$lower>0))
alpha.vals.7[signif.index]=1


## combine the three dataframes
Brazil.coefs.dfAll = rbind(
  coefs.df1 %>% mutate(grp=2015, alphaVals=alpha.vals1)
  ,coefs.df2 %>% mutate(grp=2016, alphaVals=alpha.vals2)
  ,coefs.df3 %>% mutate(grp=2017, alphaVals=alpha.vals3)
  #       ,coefs.df4 %>% mutate(grp="Zika", alphaVals=alpha.vals4)
) %>%
  mutate(location = attCountry$getActualName(location)) %>%
  rbind(rbind(
    coefs.df.1
    ,coefs.df.2
    ,coefs.df.3
    #         ,coefs.df.4
  ) %>% mutate(
    grp=c(2015:2017) #,"Zika"
    ,alphaVals=c(alpha.vals.1,alpha.vals.2,alpha.vals.3) #,alpha.vals.4
    ,location='shared effect'
    ,pindex=1
    ,region=NA
    ,index=NA
  )
  )
xrange = range(Brazil.coefs.dfAll[,c('upper','lower')])
BR.xrange=xrange
xrange=c(-3.933,3.933)#c(-3.63,3.63)

breaks1=c(0.001,0.01,0.1,1,10,100)


Brazil.year.plot=ggplot(data = Brazil.coefs.dfAll, mapping=aes(
  x = pindex
  ,y = mean
)) + 
  # region separation line
  geom_vline(data = Brazil.coefs.dfAll %>%
               filter(!is.na(region)) %>%
               group_by(region) %>%
               summarize(index = max(pindex)+.5)
             ,aes(xintercept = index)
             ,col = "#dddddd"    # light grey
             ,size = .5
  )+
  geom_hline(aes(yintercept=0,alpha=1),col="black") +
  # black bars    
  geom_errorbar(aes(
    ymin=lower
    ,ymax=upper
    ,alpha = alphaVals
  ), width=error.width, size=line.size) +
  geom_point(aes(
    alpha = alphaVals
  ))+
  geom_point(aes(
    x=pindex
    ,y=mean
    ,alpha=alphaVals
  ),color="black") +
  geom_errorbar(aes(
    x=pindex
    ,ymin=lower
    ,ymax=upper
    ,alpha=alphaVals
  ), color="black", width=error.width, size=line.size) +
  
  scale_alpha_continuous(range=c(.2,1))+
  theme(
    text = element_text(family="", size=text.size)
    ,axis.text.y = element_text(angle = 90, hjust = 0.7) #  ,axis.text.y = element_text(angle = 35, hjust = 1) # 
    ,axis.ticks.y = element_blank()
    ,legend.position="none"
    ,panel.spacing = unit(.25, "lines")
    ,plot.margin=unit(c(-0.07,0.01,0.01,-0.27),'cm')
  )+
  scale_x_continuous(
    ""
    , breaks=c(1,-4,-12,-18.5,-22.5,-26)#unique(coefs.dfAll$pindex)
    , labels=c("shared","North","Northeast","Southeast","Central-West","South")#rep("",length(unique(coefs.dfAll$pindex)))
    
#    , breaks=unique(coefs.dfAll$pindex)
#    , labels=rep("",length(unique(coefs.dfAll$pindex)))
   )+
  scale_y_continuous(name=""
                     ,breaks=log(breaks1)
                     ,labels=breaks1
                     ,limits=xrange
                     )+
  coord_flip()+
  facet_grid(.~grp)


####

# save as 12x5
#grid.arrange(year.plot, cumulative.plot, ncol=2,widths=c(2,1))
full.only=F

## all shared effects plotted together


## combine the three dataframes
Brazil.coefs.dfShared =rbind(rbind(
    coefs.df.1
    ,coefs.df.2
    ,coefs.df.3
    ,coefs.df.4
    ,coefs.df.5
    ,coefs.df.6
    ,coefs.df.7
  ) %>% mutate(
    grp2=c(2015:2017,"cumulative Zika","cumulative chikungunya","biweek Zika","biweek chikungunya") #
    ,alphaVals=c(alpha.vals.1,alpha.vals.2,alpha.vals.3,alpha.vals.4,alpha.vals.5,alpha.vals.6,alpha.vals.7) #
    ,location='shared effect'
    ,pindex=c(-1.5,-2,-2.5,-(4:7))
    ,region=NA
    ,index=NA
  )%>% mutate(
    grp="Brazil shared effects"
  )
  )


####

country.choice="Colombia"  
attCountry <- get(paste0('att',country.choice))

## Load shared-year effect hierarchical model based on absolute incidence.
# FILL

temp2015="year2015"
temp2016="year2016"
temp2017="year2017"

temp.l.2015="year2015 l"
temp.l.2016="year2016 l"
temp.l.2017="year2017 l"

coefs.df1=get.combined.cred.int(coef.choice=temp2015)

alpha.vals1=rep(alpha.choice,length(coefs.df1$lower))
signif.index=c(which(coefs.df1$upper<0),which(coefs.df1$lower>0))
alpha.vals1[signif.index]=1

coefs.df.1=data.frame(mean=c(stan.model$stan_summary[temp2015,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2015,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2015,"2.5%"])
                      ,pindex=c(0))
alpha.vals.1=rep(alpha.choice,length(coefs.df.1$lower))
signif.index=c(which(coefs.df.1$upper<0),which(coefs.df.1$lower>0))
alpha.vals.1[signif.index]=1

coefs.df2=get.combined.cred.int(coef.choice=temp2016)

alpha.vals2=rep(alpha.choice,length(coefs.df2$lower))
signif.index=c(which(coefs.df2$upper<0),which(coefs.df2$lower>0))
alpha.vals2[signif.index]=1

coefs.df.2=data.frame(mean=c(stan.model$stan_summary[temp2016,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2016,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2016,"2.5%"])
                      ,pindex=c(0))
alpha.vals.2=rep(alpha.choice,length(coefs.df.2$lower))
signif.index=c(which(coefs.df.2$upper<0),which(coefs.df.2$lower>0))
alpha.vals.2[signif.index]=1

coefs.df3=get.combined.cred.int(coef.choice=temp2017)

alpha.vals3=rep(alpha.choice,length(coefs.df3$lower))
signif.index=c(which(coefs.df3$upper<0),which(coefs.df3$lower>0))
alpha.vals3[signif.index]=1

coefs.df.3=data.frame(mean=c(stan.model$stan_summary[temp2017,"mean"])
                      ,upper=c(stan.model$stan_summary[temp2017,"97.5%"])
                      ,lower=c(stan.model$stan_summary[temp2017,"2.5%"])
                      ,pindex=c(0))
alpha.vals.3=rep(alpha.choice,length(coefs.df.3$lower))
signif.index=c(which(coefs.df.3$upper<0),which(coefs.df.3$lower>0))
alpha.vals.3[signif.index]=1

## Add cumulative Zika model effects
# load("~/FILL/stan-models/shared_Zika/outStan_Colombia_zikaIncCumu_shared_effect_group_slope_Colombia_formula.Rdata")

coefs.df4=get.combined.cred.int(coef.choice="zikaIncCumu")

alpha.vals4=rep(alpha.choice,length(coefs.df4$lower))
signif.index=c(which(coefs.df4$upper<0),which(coefs.df4$lower>0))
alpha.vals4[signif.index]=1

coefs.df.4=data.frame(mean=c(stan.model$stan_summary["zikaIncCumu","mean"])
                      ,upper=c(stan.model$stan_summary["zikaIncCumu","97.5%"])
                      ,lower=c(stan.model$stan_summary["zikaIncCumu","2.5%"])
                      ,pindex=c(1))
alpha.vals.4=rep(alpha.choice,length(coefs.df.4$lower))
signif.index=c(which(coefs.df.4$upper<0),which(coefs.df.4$lower>0))
alpha.vals.4[signif.index]=1

## Add cumulative chik model effects
# load("~/FILL/stan-models/shared_Zika/outStan_Colombia_chikIncCumu_shared_effect_group_slope_Colombia_formula.Rdata")
coefs.df.5=data.frame(mean=c(stan.model$stan_summary["chikIncCumu","mean"])
                      ,upper=c(stan.model$stan_summary["chikIncCumu","97.5%"])
                      ,lower=c(stan.model$stan_summary["chikIncCumu","2.5%"])
                      ,pindex=c(1))
alpha.vals.5=rep(alpha.choice,length(coefs.df.5$lower))
signif.index=c(which(coefs.df.5$upper<0),which(coefs.df.5$lower>0))
alpha.vals.5[signif.index]=1

## Add biweek Zika model effects
# load("~/FILL/stan-models/shared_Zika/outStan_Colombia_zikaInc_shared_effect_group_slope_Colombia_formula.Rdata")
coefs.df.6=data.frame(mean=c(stan.model$stan_summary["zikaInc","mean"])
                      ,upper=c(stan.model$stan_summary["zikaInc","97.5%"])
                      ,lower=c(stan.model$stan_summary["zikaInc","2.5%"])
                      ,pindex=c(1))
alpha.vals.6=rep(alpha.choice,length(coefs.df.6$lower))
signif.index=c(which(coefs.df.6$upper<0),which(coefs.df.6$lower>0))
alpha.vals.6[signif.index]=1

## Add biweek chik model effects
#  load("~/FILL/stan-models/shared_Zika/outStan_Colombia_chikInc_shared_effect_group_slope_Colombia_formula.Rdata")
coefs.df.7=data.frame(mean=c(stan.model$stan_summary["chikInc","mean"])
                      ,upper=c(stan.model$stan_summary["chikInc","97.5%"])
                      ,lower=c(stan.model$stan_summary["chikInc","2.5%"])
                      ,pindex=c(1))
alpha.vals.7=rep(alpha.choice,length(coefs.df.7$lower))
signif.index=c(which(coefs.df.7$upper<0),which(coefs.df.7$lower>0))
alpha.vals.7[signif.index]=1

## combine the three dataframes
coefs.dfAll = rbind(
  coefs.df1 %>% mutate(grp=2015, alphaVals=alpha.vals1)
  ,coefs.df2 %>% mutate(grp=2016, alphaVals=alpha.vals2)
  ,coefs.df3 %>% mutate(grp=2017, alphaVals=alpha.vals3)
  #       ,coefs.df4 %>% mutate(grp="Zika", alphaVals=alpha.vals4)
) %>%
  mutate(location = attCountry$getActualName(location)) %>%
  rbind(rbind(
    coefs.df.1
    ,coefs.df.2
    ,coefs.df.3
    #         ,coefs.df.4
  ) %>% mutate(
    grp=c(2015:2017) #,"Zika"
    ,alphaVals=c(alpha.vals.1,alpha.vals.2,alpha.vals.3) #,alpha.vals.4
    ,location='shared effect'
    ,pindex=1
    ,region=NA
    ,index=NA
  )
  )
CO.xrange = range(coefs.dfAll[,c('upper','lower')])

Colombia.year.plot=ggplot(data = coefs.dfAll, mapping=aes(
  x = pindex
  ,y = mean
)) + 
  # region separation line
  geom_vline(data = coefs.dfAll %>%
               filter(!is.na(region)) %>%
               group_by(region) %>%
               summarize(index = max(pindex)+.5)
             ,aes(xintercept = index)
             ,col = "#dddddd"    # light grey
             ,size = .5
  )+
  geom_hline(aes(yintercept=0,alpha=1),col="black") +
  # black bars    
  geom_errorbar(aes(
    ymin=lower
    ,ymax=upper
    ,alpha = alphaVals
  ), width=error.width, size=line.size) +
  geom_point(aes(
    alpha = alphaVals
  ))+
  geom_point(aes(
    x=pindex
    ,y=mean
    ,alpha=alphaVals
  ),color="black") +
  geom_errorbar(aes(
    x=pindex
    ,ymin=lower
    ,ymax=upper
    ,alpha=alphaVals
  ), color="black", width=error.width, size=line.size) +
  
  scale_alpha_continuous(range=c(.2,1))+
  theme(
    text = element_text(family="", size=text.size)
    ,axis.text.y = element_text(angle = 90, hjust = 0.7) 
#    ,axis.text.y = element_text(angle = 35, hjust = 1) #  
    ,axis.ticks.y = element_blank()
    ,legend.position="none"
    ,panel.spacing = unit(.25, "lines")
    ,plot.margin=unit(c(-0.07,0.01,0.01,-0.27),'cm')
#    ,plot.margin=unit(c(-0.27,0.01,0.01,-0.27),'cm')
  )+
  scale_x_continuous(
    ""
    , breaks=c(1,-4.5,-10.5,-17.5,-25.5,-30.5)#unique(coefs.dfAll$pindex)
    , labels=c("shared","Caribbean","Pacific","Andes","Orinoquia","Amazon")#rep("",length(unique(coefs.dfAll$pindex)))
    #    ,labels=unique(coefs.dfAll$location)
  )+
  scale_y_continuous(name=""
                     ,breaks=log(breaks1)
                     ,labels=breaks1
                     ,limits=xrange
  )+
  coord_flip()+
  facet_grid(.~grp)


##### all shared effects plotted together


## combine the three dataframes
Colombia.coefs.dfShared =rbind(rbind(
  coefs.df.1
  ,coefs.df.2
  ,coefs.df.3
  ,coefs.df.4
  ,coefs.df.5
  ,coefs.df.6
  ,coefs.df.7
) %>% mutate(
  grp2=c(2015:2017,"cumulative Zika","cumulative chikungunya","biweek Zika","biweek chikungunya") #
  ,alphaVals=c(alpha.vals.1,alpha.vals.2,alpha.vals.3,alpha.vals.4,alpha.vals.5,alpha.vals.6,alpha.vals.7) #
  ,location='shared effect'
  ,pindex=c(-1.5,-2,-2.5,-(4:7))
  ,region=NA
  ,index=NA
) %>% mutate(
  grp="Colombia shared effects"
)
)



xrange.Brazil=range(Brazil.coefs.dfShared[,c('upper','lower')])
xrange.Colombia = range(Colombia.coefs.dfShared[,c('upper','lower')])


xrange.shared=c(1*min(c(xrange.Brazil,xrange.Colombia))
                ,1*max(c(xrange.Brazil,xrange.Colombia)))


Brazil.shared=ggplot(data = Brazil.coefs.dfShared, mapping=aes(
  x = pindex
  ,y = mean
)) + 
  geom_hline(aes(yintercept=0,alpha=1),col="black") +
  # black bars    
  geom_errorbar(aes(
    ymin=lower
    ,ymax=upper
    ,alpha = alphaVals
  ), width=error.width, size=line.size) +
  geom_point(aes(
    alpha = alphaVals
  ))+
  geom_point(aes(
    x=pindex
    ,y=mean
    ,alpha=alphaVals
  ),color="black") +
  geom_errorbar(aes(
    x=pindex
    ,ymin=lower
    ,ymax=upper
    ,alpha=alphaVals
  ), color="black", width=error.width, size=line.size) +
  
  scale_alpha_continuous(range=c(.2,1))+
  theme(
    text = element_text(family="", size=text.size)
    ,plot.title=element_text(family="",size=text.size)
    ,axis.text.y = element_text(angle = 35, hjust = 1) #  element_text(angle = 45, hjust = 1)
    ,axis.ticks.y = element_blank()
    ,legend.position="none"
    ,panel.spacing = unit(.25, "lines")
    ,plot.margin=unit(c(-0.07,-0.07,0.01,-0.27),'cm')
  )+
  scale_x_continuous(
    ""
    , breaks=unique(Brazil.coefs.dfShared$pindex)
    , labels=c("2015","2016","2017","Zika","chikungunya","Zika","chikungunya")
#    , labels=Brazil.coefs.dfShared$grp2 #unique(coefs.dfAll$location)
  )+
  scale_y_continuous(name="",limits=xrange.shared)+
  coord_flip()+
  facet_grid(.~grp)

Colombia.shared=ggplot(data = Colombia.coefs.dfShared, mapping=aes(
  x = pindex
  ,y = mean
)) + 
  geom_hline(aes(yintercept=0,alpha=1),col="black") +
  # black bars    
  geom_errorbar(aes(
    ymin=lower
    ,ymax=upper
    ,alpha = alphaVals
  ), width=error.width, size=line.size) +
  geom_point(aes(
    alpha = alphaVals
  ))+
  geom_point(aes(
    x=pindex
    ,y=mean
    ,alpha=alphaVals
  ),color="black") +
  geom_errorbar(aes(
    x=pindex
    ,ymin=lower
    ,ymax=upper
    ,alpha=alphaVals
  ), color="black", width=error.width, size=line.size) +
  
  scale_alpha_continuous(range=c(.2,1))+
  theme(
    text = element_text(family="", size=text.size)
    ,plot.title=element_text(family="",size=text.size)
    ,axis.text.y = element_text(angle = 35, hjust = 1) #    ,axis.text.y = element_text(angle = 90, hjust = 0) 
    ,axis.ticks.y = element_blank()
    ,legend.position="none"
    ,panel.spacing = unit(.25, "lines")
    ,plot.margin=unit(c(-0.07,-0.07,0.01,-0.27),'cm')
#    ,plot.margin=unit(c(-0.27,-0.07,0.01,-0.27),'cm')
  )+
  scale_x_continuous(
    ""
    , breaks=unique(Colombia.coefs.dfShared$pindex)
    , labels=c("2015","2016","2017","Zika","chikungunya","Zika","chikungunya")
    #, labels=Colombia.coefs.dfShared$grp2 #unique(coefs.dfAll$location)
  )+
  scale_y_continuous(name="",limits=xrange.shared)+
  coord_flip()+
  facet_grid(.~grp)

#grid.arrange(Brazil.shared,Colombia.shared, ncol=2,widths=c(1,1))
#grid.arrange(Brazil.year.plot, Colombia.year.plot, ncol=1)

grid.arrange(Brazil.shared,Brazil.year.plot,Colombia.shared, Colombia.year.plot, ncol=2,widths=c(1,2))

file.name=paste0('06-analyze/Figure3')

ggsave(filename=paste0(file.name,'.pdf'))

