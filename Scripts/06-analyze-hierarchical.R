## This script loads the hierarchical stan models (previous biweek
## Zika or chikungunya and cumulative Zika or chikungunya) and extracts
## 95% Bayesian credible intervals for the shared effects of these models

# library(dplyr)
# library(tidyr)
# #library(tidybayes)
# library(ggplot2)
# library(rstan)
# library(rstanarm)
# library(grid)
# #library(gridExtra)
 
 country.choice="Brazil"
 country.choice="Colombia"
 attCountry <- get(paste0('att',country.choice))
 
# ##########
# ## shared effect Zika and chik models
# ## extended data plot
# ##########

line.size=0.9
error.width=.4
text.size=16


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

get.coef.pairs.data <- function(){
  zika.dat=subset(coefs.dfAll,grp=="zika")[,c("mean","location","pindex","region","grp1","grp2")]
  colnames(zika.dat)=c("zika.mean","location","pindex","region","grp1","grp2")
  chik.dat=subset(coefs.dfAll,grp=="chik")[,c("mean","location","pindex","region","grp1","grp2")]
  colnames(chik.dat)=c("chik.mean","location","pindex","region","grp1","grp2")
  new.dat=merge(zika.dat,chik.dat,by=c("location","region","grp1","pindex"))
  
  new.dat=subset(new.dat,location != "shared effect")
  return(new.dat)
}

get.shared.effects <- function(){
  shared.dat=subset(coefs.dfAll,location=="shared effect")
  shared.dat=shared.dat[,c("mean","upper","lower","location","pindex","grp1","grp2","alphaVals")]
  shared.dat[,"pindex"]=4:1
  shared.dat[,"alphaVals"]=rep(0.5,length(shared.dat[,"lower"]))
  signif.index=c(which(shared.dat[,"upper"]<0),which(shared.dat[,"lower"]>0))
  shared.dat[,"alphaVals"][signif.index]=1
  return(shared.dat)
}

## Load hierarchical stan models 
## Note: this portion of code will not run without saved models
## File names will change depending on where these models are saved
## (indicated by FILL below)
## random.choice indicates the first year of randomized data (e.g.
## random.choice = 2011 indicates that the data for 2015-2017 were 
## replaced with data from 2011 to 2013)
if(randomized==T){
  if(country.choice=="Brazil"){
    file.head=paste0("~/FILL/",random.choice,"/outStan_")
    file.tail=paste0("_shared_effect_group_slope_formula_random.Rdata")
  }else{
    file.head=paste0("~/FILL/",random.choice,"/outStan_")
    file.tail=paste0("_shared_effect_group_slope_Colombia_formula_random.Rdata")
  }
}else{
  random.choice=paste0(country.choice,"actual")
  if(country.choice=="Brazil"){
    file.head=paste0("~/FILL/outStan_")
    file.tail=paste0("_shared_effect_group_slope_formula.Rdata")
  }else{
    file.head=paste0("~/FILL/outStan_Colombia_")
    file.tail=paste0("_shared_effect_group_slope_Colombia_formula.Rdata")
  }
}



alpha.vals=list()
coefs.df=list()
coefs.df.shared=list()
alpha.vals.shared=list()

for(i in 1:length(these.effects)){
  file.load=paste0(file.head,these.effects[i],file.tail)
  load(file.load)
  coefs.df[[i]]=get.combined.cred.int(coef.choice=these.effects[i])
  signif.index=c(which(coefs.df[[i]]$upper<0),which(coefs.df[[i]]$lower>0))
  alpha.vals[[i]]=rep(0.5,length(coefs.df[[i]]$lower))
  alpha.vals[[i]][signif.index]=1
  
  #coefs.df.1
  coefs.df.shared[[i]]=data.frame(mean=c(stan.model$stan_summary[these.effects[i],"mean"])
                                  ,upper=c(stan.model$stan_summary[these.effects[i],"97.5%"])
                                  ,lower=c(stan.model$stan_summary[these.effects[i],"2.5%"])
                                  ,pindex=c(1))
  #alpha.vals.1 # only one element these next few lines could possibly be condensed
  alpha.vals.shared[[i]]=rep(0.5,length(coefs.df.shared[[i]]$lower))
  signif.index=c(which(coefs.df.shared[[1]]$upper<0),which(coefs.df.shared[[i]]$lower>0))
  alpha.vals.shared[[i]][signif.index]=1
  coefs.df[[i]]=coefs.df[[i]] %>% mutate(grp=substr(these.effects[i],start=1,stop=4)
                                         ,grp1=substr(these.effects,start=5,stop=nchar(these.effects))[i]
                                         ,grp2=these.effects[i]
                                         ,alphaVals=alpha.vals[[i]])
}
names(coefs.df)=these.effects
names(coefs.df.shared)=these.effects
names(alpha.vals.shared)=these.effects

choice1="zikaIncCumu"
choice2="chikIncCumu" 
choice3="zikaInc"
choice4="chikInc"

coefs.dfAll1 = rbind(coefs.df[[choice1]],coefs.df[[choice2]]) %>%
  mutate(grp2 = factor(c(coefs.df[[choice1]]$grp2,coefs.df[[choice2]]$grp2),levels = c(choice1,choice2),ordered = T)) %>%
  mutate(grp1 = factor(c(coefs.df[[choice1]]$grp1,coefs.df[[choice2]]$grp1),levels = c("IncCumu","Inc"),ordered = T)) %>%
  mutate(location = attCountry$getActualName(location)) %>%
  rbind(rbind(
    coefs.df.shared[[choice1]]
    ,coefs.df.shared[[choice2]]
  ) %>% mutate(
    grp=c(substr(choice1,start=1,stop = 4),substr(choice2,start=1,stop = 4))
    ,grp1= factor(c(substr(choice1,start=5,stop=nchar(choice1)),substr(choice2,start=5,stop=nchar(choice2))),levels=c("IncCumu","Inc"),ordered=T)
    ,grp2= factor(c(choice1,choice2),levels=c(choice1,choice2),ordered=T)
    ,alphaVals=c(alpha.vals.shared[[choice1]],alpha.vals.shared[[choice2]])
    ,location='shared effect'
    ,pindex=1
    ,region=NA
    ,index=NA
  )
  )

coefs.dfAll2 = rbind(coefs.df[[choice3]],coefs.df[[choice4]]) %>%
  mutate(grp2 = factor(c(coefs.df[[choice3]]$grp2,coefs.df[[choice4]]$grp2),levels = c(choice3,choice4),ordered = T)) %>%
  mutate(grp1 = factor(c(coefs.df[[choice3]]$grp1,coefs.df[[choice4]]$grp1),levels = c("IncCumu","Inc"),ordered = T)) %>%
  mutate(location = attCountry$getActualName(location)) %>%
  rbind(rbind(
    coefs.df.shared[[choice3]]
    ,coefs.df.shared[[choice4]]
  ) %>% mutate(
    grp=c(substr(choice1,start=3,stop = 4),substr(choice4,start=1,stop = 4))
    ,grp1= factor(c(substr(choice3,start=5,stop=nchar(choice3)),substr(choice4,start=5,stop=nchar(choice4))),levels=c("IncCumu","Inc"),ordered=T)
    ,grp2= factor(c(choice3,choice4),levels=c(choice3,choice4),ordered=T)
    ,alphaVals=c(alpha.vals.shared[[choice3]],alpha.vals.shared[[choice4]])
    ,location='shared effect'
    ,pindex=1
    ,region=NA
    ,index=NA
  )
  )

coefs.dfAll=rbind(coefs.dfAll1,coefs.dfAll2) %>%
mutate(grp2=factor(substr(grp2,start=1,stop=4),levels=c("zika","chik"),ordered=T)) 
save(coefs.dfAll,file=paste0(random.choice,"coefs.dfAll.Rdata"))

# plot.all=T
# if(plot.all==T){
#   coefs.dfAll=rbind(coefs.dfAll1,coefs.dfAll2) %>%
#     mutate(grp2=factor(substr(grp2,start=1,stop=4),levels=c("zika","chik"),ordered=T)) 
#   save(coefs.dfAll,file=paste0(random.choice,"coefs.dfAll.Rdata"))
# }else{
#   coefs.dfAll=coefs.dfAll1
# }
# 
# xrange = range(coefs.dfAll[,c('upper','lower')])
##xrange =c(-0.2,0.6)
##xrange =c(-0.55,0.75)
#CO.xrange=xrange # = c(-0.1378425 , 0.1705404)
# BR.xrange = c( -0.2521262,  0.8756839)
xrange= c( -0.2521262,  0.8756839)


ggplot(data = coefs.dfAll, mapping=aes(
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
  
  scale_alpha_continuous(range=c(.4,1))+
  theme(
    text = element_text(family="", size=text.size)
    ,axis.ticks.y = element_blank()
    ,legend.position="none"
  )+
  scale_x_continuous(
    ""
    , breaks=unique(coefs.dfAll$pindex)
    , labels=rep("",length(unique(coefs.dfAll$pindex)))#unique(coefs.dfAll$location)
  )+
  scale_y_continuous(name="Coefficient mean (95% BCI)",limits=xrange)+
  coord_flip()+
  facet_grid(grp2~grp1) #facet_grid(.~grp2)


## save as 12 x 8 for each country (Supplementary Fig. 7)

# zika.dat=subset(coefs.dfAll,grp=="zika")[,c("mean","location","pindex","region","grp1","grp2")]
# colnames(zika.dat)=c("zika.mean","location","pindex","region","grp1","grp2")
# chik.dat=subset(coefs.dfAll,grp=="chik")[,c("mean","location","pindex","region","grp1","grp2")]
# colnames(chik.dat)=c("chik.mean","location","pindex","region","grp1","grp2")
# new.dat=merge(zika.dat,chik.dat,by=c("location","region","grp1","pindex"))
# 
# new.dat=subset(new.dat,location != "shared effect")
# 
# coefficient.pairs[[j]]=new.dat
# names(coefficient.pairs)[[j]]=random.choice







