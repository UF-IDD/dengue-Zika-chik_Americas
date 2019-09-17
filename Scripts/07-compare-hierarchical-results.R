## This script generates Supplementary Fig. 9 by sourcing "Scripts/06-analyze-hierarchical.R"

library(dplyr)
library(tidyr)
#library(tidybayes)
library(ggplot2)
library(rstan)
library(rstanarm)
library(grid)
#library(gridExtra)

country.choice="Brazil"
attCountry <- get(paste0('att',country.choice))

coefficient.pairs=list()
shared.dat=list()

# choose effects to plot
these.effects=c('zikaIncCumu','zikaInc','chikIncCumu','chikInc')

randomized=F

random.choices=c("Brazil actual","Brazil2003","Brazil2004","Brazil2006","Brazil2009","Brazil2011")
source("Scripts/06-analyze-hierarchical.R")
coefficient.pairs[[1]]=get.coef.pairs.data()
shared.dat[[1]] = get.shared.effects()

randomized=T
for(j in 2:length(random.choices)){
  random.choice=random.choices[j]
  source("Scripts/06-analyze-hierarchical.R")
  coefficient.pairs[[j]]=get.coef.pairs.data()
  names(coefficient.pairs)[[j]]=random.choice
  shared.dat[[j]]=get.shared.effects()
}
names(shared.dat)=random.choices

country.choice="Colombia"
attCountry <- get(paste0('att',country.choice))

Colombia.coefficient.pairs=list()
Colombia.shared.dat=list()

randomized=F
random.choices=c("Colombia actual","Colombia2008","Colombia2009","Colombia2010","Colombia2011","Colombia2012") 
source("Scripts/06-analyze-hierarchical.R")
Colombia.coefficient.pairs[[1]]=get.coef.pairs.data()
Colombia.shared.dat[[1]] = get.shared.effects()

randomized=T
for(j in 2:length(random.choices)){
  random.choice=random.choices[j]
  source("Scripts/06-analyze-hierarchical.R")
  Colombia.coefficient.pairs[[j]]=get.coef.pairs.data()
  names(Colombia.coefficient.pairs)[[j]]=random.choice
  Colombia.shared.dat[[j]]=get.shared.effects()
}
names(Colombia.shared.dat)=random.choices

names(coefficient.pairs)[1]="Brazil actual"
names(Colombia.coefficient.pairs)[1]="Colombia actual"

inc.plot <- function(temp,temp.title,xrange=c(-0.1,0.2)){ #xrange=c(-0.3,0.6)c(-0.5,0.5)
  new.dat=temp
 ggplot(new.dat %>% filter(grp1=="Inc"), aes(
   x = zika.mean
   ,y = chik.mean
   ,col = region
 ))+
   geom_point(size=3)+
   coord_fixed()+
#   scale_x_continuous(limits=xrange)+
#   scale_y_continuous(limits=xrange)+
   ggtitle(temp.title)+
#   ggtitle(paste0(temp.title,"; R = ", round(cor(new.dat$zika.mean,new.dat$chik.mean),2)))+
   labs(x="Zika coefficient",y="chik coefficent")+
   theme(axis.text= element_text(family="",size=12),
         text = element_text(family="")
         ,legend.position = "none"
          ,aspect.ratio = 1)
}

inc.cumu.plot <- function(temp,temp.title){
  new.dat=temp
  ggplot(new.dat %>% filter(grp1=="IncCumu"), aes(
    x = zika.mean
    ,y = chik.mean
    ,col = region
  ))+
    geom_point(size=3)+
    coord_fixed()+
    ggtitle(paste0(temp.title," R = ", round(cor(new.dat$zika.mean,new.dat$chik.mean),2)))+
    labs(x="Zika coefficient",y="chik coefficent")+
    theme(axis.text= element_text(family="",size=12),
          text = element_text(family="")
          ,legend.position = "none"
          ,aspect.ratio = 1)
}


inc.p1.B=inc.plot(coefficient.pairs[[1]],names(coefficient.pairs)[1])
inc.p2.B=inc.plot(coefficient.pairs[[2]],names(coefficient.pairs)[2])
inc.p3.B=inc.plot(coefficient.pairs[[3]],names(coefficient.pairs)[3])
inc.p4.B=inc.plot(coefficient.pairs[[4]],names(coefficient.pairs)[4])
inc.p5.B=inc.plot(coefficient.pairs[[5]],names(coefficient.pairs)[5])
inc.p6.B=inc.plot(coefficient.pairs[[6]],names(coefficient.pairs)[6])
#grid.arrange(inc.p1.B,inc.p2.B,inc.p3.B,inc.p4.B,inc.p5.B,inc.p6.B, ncol=6)


inc.p1=inc.plot(Colombia.coefficient.pairs[[1]],names(Colombia.coefficient.pairs)[1])
inc.p2=inc.plot(Colombia.coefficient.pairs[[2]],names(Colombia.coefficient.pairs)[2])
inc.p3=inc.plot(Colombia.coefficient.pairs[[3]],names(Colombia.coefficient.pairs)[3])
inc.p4=inc.plot(Colombia.coefficient.pairs[[4]],names(Colombia.coefficient.pairs)[4])
inc.p5=inc.plot(Colombia.coefficient.pairs[[5]],names(Colombia.coefficient.pairs)[5])
inc.p6=inc.plot(Colombia.coefficient.pairs[[6]],names(Colombia.coefficient.pairs)[6])

#grid.arrange(inc.p1,inc.p2,inc.p3,inc.p4,inc.p5,inc.p6, ncol=6)

grid.arrange(inc.p1.B,inc.p2.B,inc.p3.B,inc.p4.B,inc.p5.B,inc.p6.B
             ,inc.p1,inc.p2,inc.p3,inc.p4,inc.p5,inc.p6, ncol=6)

inc.p1.B=inc.cumu.plot(coefficient.pairs[[1]],names(coefficient.pairs)[1])
inc.p2.B=inc.cumu.plot(coefficient.pairs[[2]],names(coefficient.pairs)[2])
inc.p3.B=inc.cumu.plot(coefficient.pairs[[3]],names(coefficient.pairs)[3])
inc.p4.B=inc.cumu.plot(coefficient.pairs[[4]],names(coefficient.pairs)[4])
inc.p5.B=inc.cumu.plot(coefficient.pairs[[5]],names(coefficient.pairs)[5])
inc.p6.B=inc.cumu.plot(coefficient.pairs[[6]],names(coefficient.pairs)[6])
grid.arrange(inc.p1.B,inc.p2.B,inc.p3.B,inc.p4.B,inc.p5.B,inc.p6.B, ncol=6)


inc.p1=inc.cumu.plot(Colombia.coefficient.pairs[[1]],names(Colombia.coefficient.pairs)[1])
inc.p2=inc.cumu.plot(Colombia.coefficient.pairs[[2]],names(Colombia.coefficient.pairs)[2])
inc.p3=inc.cumu.plot(Colombia.coefficient.pairs[[3]],names(Colombia.coefficient.pairs)[3])
inc.p4=inc.cumu.plot(Colombia.coefficient.pairs[[4]],names(Colombia.coefficient.pairs)[4])
inc.p5=inc.cumu.plot(Colombia.coefficient.pairs[[5]],names(Colombia.coefficient.pairs)[5])
inc.p6=inc.cumu.plot(Colombia.coefficient.pairs[[6]],names(Colombia.coefficient.pairs)[6])

grid.arrange(inc.p1,inc.p2,inc.p3,inc.p4,inc.p5,inc.p6, ncol=6)

grid.arrange(inc.p1.B,inc.p2.B,inc.p3.B,inc.p4.B,inc.p5.B,inc.p6.B
             ,inc.p1,inc.p2,inc.p3,inc.p4,inc.p5,inc.p6, ncol=6)

xrange.CO2=c(-.4,.3)
xrange.BR2=xrange.CO2


## plots for mean and 95% BCI of shared effect coefficients based on models with actual data (1st column)
## and random year data (other columns)
shared.effects.plot <- function(temp,temp.title,xrange){
  new.dat = temp
  ggplot(data = new.dat, mapping=aes(
    x = pindex #pindex
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
    
    scale_alpha_continuous(range=c(.4,1))+
    theme(
      text = element_text(family="", size=18)
      #      ,axis.text.x = element_text(angle = 45, hjust = 1)
      ,axis.ticks.y = element_blank()
      ,legend.position="none"
      ,panel.spacing = unit(.5, "lines")
      ,plot.margin=unit(c(0.05,-0.07,0.01,-0.07),'cm')
    )+
    ggtitle(temp.title)+
    scale_x_continuous(
      ""
      , breaks=unique(new.dat$pindex)
      #    , labels=coefs.dfShared$grp #unique(coefs.dfAll$location)
      , labels=paste0(new.dat$grp2,new.dat$grp1)#rep("",length(coefs.dfShared$grp)) #unique(coefs.dfAll$location)
    )+
    scale_y_continuous(name="",limits=xrange)+
    coord_flip()#+
  
}
#xrange.CO2=c(-0.16,.26)
#xrange.BR2=c(-0.4,.14)
shared.effects.plot2 <- function(temp,temp.title,xrange){
  new.dat = temp
  ggplot(data = new.dat, mapping=aes(
    x = pindex #pindex
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
    
    scale_alpha_continuous(range=c(.4,1))+
    theme(
      text = element_text(family="", size=18)
      #      ,axis.text.x = element_text(angle = 45, hjust = 1)
      ,axis.ticks.y = element_blank()
      ,legend.position="none"
      ,panel.spacing = unit(.5, "lines")
      ,plot.margin=unit(c(0.05,-0.07,0.01,-0.07),'cm')
    )+
    ggtitle(temp.title)+
    scale_x_continuous(
      ""
      , breaks=unique(new.dat$pindex)
      , labels = rep("",length(unique(new.dat$pindex)))
      #    , labels=coefs.dfShared$grp #unique(coefs.dfAll$location)
      #    , labels=paste0(new.dat$grp2,new.dat$grp1)#rep("",length(coefs.dfShared$grp)) #unique(coefs.dfAll$location)
    )+
    scale_y_continuous(name="",limits=xrange)+
    coord_flip()#+
  
}



names(shared.dat)[1]="Brazil actual"
names(Colombia.shared.dat)[1]="Colombia actual"
shared1=shared.effects.plot(shared.dat[[1]],names(shared.dat)[1],xrange.BR2)
shared2=shared.effects.plot2(shared.dat[[2]],names(shared.dat)[2],xrange.BR2)
shared3=shared.effects.plot2(shared.dat[[3]],names(shared.dat)[3],xrange.BR2)
shared4=shared.effects.plot2(shared.dat[[4]],names(shared.dat)[4],xrange.BR2)
shared5=shared.effects.plot2(shared.dat[[5]],names(shared.dat)[5],xrange.BR2)
shared6=shared.effects.plot2(shared.dat[[6]],names(shared.dat)[6],xrange.BR2)


shared7=shared.effects.plot(Colombia.shared.dat[[1]],names(Colombia.shared.dat)[1],xrange.CO2)
shared8=shared.effects.plot2(Colombia.shared.dat[[2]],names(Colombia.shared.dat)[2],xrange.CO2)
shared9=shared.effects.plot2(Colombia.shared.dat[[3]],names(Colombia.shared.dat)[3],xrange.CO2)
shared10=shared.effects.plot2(Colombia.shared.dat[[4]],names(Colombia.shared.dat)[4],xrange.CO2)
shared11=shared.effects.plot2(Colombia.shared.dat[[5]],names(Colombia.shared.dat)[5],xrange.CO2)
shared12=shared.effects.plot2(Colombia.shared.dat[[6]],names(Colombia.shared.dat)[6],xrange.CO2)

grid.arrange(shared1,shared2,shared3, ncol=3, widths=c(1,1,1))

grid.arrange(shared1,shared2,shared3,shared4,shared5,shared6,shared7,shared8,shared9,shared10,shared11,shared12, ncol=6, widths=c(1.35,1,1,1,1,1))

shared4=shared.effects.plot(shared.dat[[4]],names(shared.dat)[4],xrange.BR2)
shared10=shared.effects.plot(Colombia.shared.dat[[4]],names(Colombia.shared.dat)[4],xrange.CO2)

grid.arrange(shared1,shared2,shared3,shared4,shared5,shared6,shared7,shared8,shared9,shared10,shared11,shared12, ncol=3, widths=c(1.35,1,1))
