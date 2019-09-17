## Evaluate and analyze the subnational-level stan models for biweekly dengue incidence

setwd("~/dengue-Zika-chik_Americas")
source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")
load("lat_values.Rdata")
source("Scripts/set_location_order_colors.R")


library(dplyr)
library(tidyr)
library(tidybayes)
library(ggplot2)
library(rstan)
library(rstanarm)
library(grid)
library(gridExtra)

# Note: this script requires the subnational-level stan models generated in "06-stan-get-NB-models-HPC"
# We supply sample prediction intervals from our stan model fits in the Materials folder, but
# the full models are needed for the convergence checks and coefficient plots.


##########################################################
## year-out-validation model analysis plots and quanitites
##########################################################

country.choice="Brazil"
#country.choice="Colombia"
attCountry <- get(paste0('att',country.choice))


## Load the subnational-level stan models
## FILL 


## load the convergence check functions from the Shinystan package version 2.5.0
source("Scripts/functions.R")

## Load pred.intervals.list from "generate-m-files.R" 
if(country.choice=="Brazil"){
  load("Materials/predIntervals_Brazil.Rdata")
  place.list=lat.ordered.Brazil  
}else{
  load("Materials/predIntervals_Colombia.Rdata")
  place.list=lat.ordered.CO
}
#place.list[order(attCountry$getPindexFromLocation(place.list))]

#  genearate and plot in- and out-of-sample Rsq values
par(lwd=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5)

if(country.choice=="Brazil"){
  panel.nrow=7
  panel.ncol=5#4
}else{
  panel.nrow=7
  panel.ncol=5
}

par(mfrow=c(panel.nrow, panel.ncol)
    ,oma=c(7,6,1,1)+0.1
    ,mar=c(0.5,1,2.5,0.5)+0.1)

# Choose whether to calculate Bayesion R^2 values
BayesianR2=T#F

## calculate and plot Rsq values for both in- and out-of-sample predictions
## evaluate shinystan convergence metrics
Rsq.values=lapply(1:length(place.list), function(i){
  temp.loc=place.list[order(attCountry$getPindexFromLocation(place.list))[i]]
  
  temp.dat=subset(incidence.dat,location==temp.loc)
  
  Rsq.in.sample=c()
  Rsq.out.sample=c()
  n.eff.params=c()
  mcse.over.sd.params=c()
  rhat.warning.params=c()
  for(temp.year in unique(temp.dat$year)){
    
    year.out=temp.year
    
    temp.stan=stan.models[[temp.loc]][[as.character(year.out)]]$stan.model
    temp=subset(pred.intervals.list[[temp.loc]],year==temp.year)

    if(BayesianR2==F){

      SS.total = sum((temp.stan$y-mean(temp.stan$y))^2)
      SS.residual = sum((temp.stan$y - temp.stan$fitted.values)^2)
      sum((temp.stan$residuals)^2)
      SS.regression = sum((temp.stan$fitted.values-mean(temp.stan$y))^2)
      Rsq.in.sample=c(Rsq.in.sample,1-SS.residual/SS.total)

      SS.total = sum((temp$observed-mean(temp$observed))^2)
      SS.residual = sum((temp$observed - temp$median.pred)^2)
      SS.regression = sum((temp$median.pred-mean(temp$observed))^2)
      Rsq.out.sample=c(Rsq.out.sample, 1-SS.residual/SS.total)
    }else{
        Rsq.in.sample=c(Rsq.in.sample,median(bayes_R2(temp.stan)))
        Rsq.out.sample=c(Rsq.out.sample,unique(temp$R2.out))
     }
    
    
    if(.n_eff_warnings(temp.stan$stan_summary)=="None"){
      n.eff.params=c(n.eff.params,0)
    }else{
      n.eff.params=c(n.eff.params,length(.n_eff_warnings(temp.stan$stan_summary)))
    }

    if(.mcse_over_sd_warnings(temp.stan$stan_summary)=="None"){
      mcse.over.sd.params=c(mcse.over.sd.params,0)
    }else{
      mcse.over.sd.params=c(mcse.over.sd.params,length(.mcse_over_sd_warnings(temp.stan$stan_summary)))
    }
 
    if(.rhat_warnings(temp.stan$stan_summary)=="None"){
      rhat.warning.params=c(rhat.warning.params,0)
    }else{
      rhat.warning.params=c(rhat.warning.params,length(.rhat_warnings(temp.stan$stan_summary)))
    }
    
  
  }
  Rsq=rbind(Rsq.in.sample
            ,Rsq.out.sample
            ,n.eff.params
            ,mcse.over.sd.params
            ,rhat.warning.params)
  colnames(Rsq)=unique(temp.dat$year)
  
  legX <- (length(place.list) -panel.ncol)< i
  legY <- ((i %% panel.ncol)) == 1
  plot(unique(temp.dat$year),Rsq.in.sample,type='l',ylim=c(0,1),col='gray',main=attCountry$getActualName(temp.loc),lwd=2,xaxt='n',axes=F)
  points(unique(temp.dat$year),Rsq.out.sample,col='black',pch=16)
  Axis(side=1, labels = legX)
  Axis(side=2, labels = legY)
  
  return(Rsq)
})

plot.new()
legend("left",legend=c("In sample","Out of sample"),col = c("gray","black"),lwd = 2,bty="n",cex=1.5)

mtext("year", SOUTH<-1, line=3.5, cex=1.5, col="black", outer=T)
if(BayesianR2==F){
  mtext("R-squared", SOUTH<-2, line=3, cex=1.5, col="black", outer=T)
}else{
  mtext("Bayesian R-squared", SOUTH<-2, line=3, cex=1.5, col="black", outer=T)
}

dev.off()

#9x8 for each country

## compare coefficents

many.years=T 

coefs.list <- lapply(1:length(place.list), function(i){
  temp.loc=place.list[order(attCountry$getPindexFromLocation(place.list))[i]]
  
  temp.dat=subset(incidence.dat,location==temp.loc)
  
  coefs=c()
  if(many.years==T){
    for(temp.year in unique(temp.dat$year)){
      year.out=temp.year
      temp.stan=stan.models[[temp.loc]][[as.character(year.out)]]$stan.model
      coefs=rbind(coefs,temp.stan$coefficients)
    }  
    row.names(coefs)=unique(temp.dat$year)
  }else{
    temp.stan=stan.models[[temp.loc]]
    coefs=rbind(coefs,temp.stan$coefficients)
  }
  if(country.choice=="Brazil"){
    coefs=data.frame(location=attBrazil$getActualName(temp.loc),region=attBrazil$getRegion(temp.loc),coefs)
  }else{coefs=data.frame(location=attColombia$getActualName(temp.loc),region=attColombia$getRegion(temp.loc),coefs)}
  
  return(coefs) 
})
names(coefs.list)=place.list[order(attCountry$getPindexFromLocation(place.list))]

# get mins and maxes for coefficient boxplots
coef.mins=lapply(1:length(coefs.list),function(i){
  min(coefs.list[[i]][,-c(1,2,3)])
})
coef.min=1*min(0,min(unlist(coef.mins)))

coef.maxs=lapply(1:length(coefs.list),function(i){
  max(coefs.list[[i]][,-c(1,2,3)])
})
coef.max=1*max(1,max(unlist(coef.maxs)))

# make boxplox plots of coefficients for each location
# there is a biweek value for each year fit (i.e. total number of years available)
par(lwd=1.5,cex.axis=1.5,cex.lab=1.2,cex.main=1)

par(mfrow=c(panel.nrow, panel.ncol)
    ,oma=c(7,6,1,1)+0.1
    ,mar=c(0.5,1,2.5,0.5)+0.1)

# par(mfrow=c(panel.nrow, panel.ncol)
#     ,oma=c(7,6,1,1)+0.1
#     ,mar=c(1.5,1,3.5,0.5)+0.1)

lapply(1:length(coefs.list), function(i){
  temp.coefs=coefs.list[[i]]
  
  temp.loc=unique(temp.coefs$location)

  legX <- (length(coefs.list) -panel.ncol)< i
  legY <- ((i %% panel.ncol)) == 1
  
  boxplot(temp.coefs[,-c(1,2,3)],bty='n',main=temp.loc,col='lightgray',frame.plot=F,axes=F)
  #          ,ylim=c(coef.min,coef.max)) 
    Axis(side=2, labels = legY)
    if(legX==T){
      axis(side = 1,at=c(1,26),labels=c("Jan","Dec"))
    }else{
      axis(side=1,at=c(1,26),labels=c("",""))      
    } 
})


mtext("biweek", SOUTH<-1, line=3, cex=1.5, col="black", outer=T)
mtext("coefficient", SOUTH<-2, line=4, cex=1.5, col="black", outer=T)
dev.off()

par(lwd=1.5,cex.axis=1.5,cex.lab=1.2,cex.main=1)

par(mfrow=c(panel.nrow, panel.ncol)
    ,oma=c(7,6,1,1)+0.1
    ,mar=c(0.5,3,2.5,0.5)+0.1)


lapply(1:length(coefs.list), function(i){
  temp.coefs=coefs.list[[i]]
  
  temp.loc=unique(temp.coefs$location)
  
  legX <- (length(coefs.list) -panel.ncol)< i
  legY <- ((i %% panel.ncol)) == 1
  
  boxplot(temp.coefs[,-c(1,2,3)],bty='n',main=temp.loc,col='lightgray',frame.plot=F,xaxt="n")
  if(legX==T){
    axis(side = 1,at=c(1,26),labels=c("Jan","Dec"))
  }else{
    axis(side=1,at=c(1,26),labels=c("",""))      
  } 
})

mtext("biweek", SOUTH<-1, line=4, cex=1.5, col="black", outer=T)
mtext("coefficient", SOUTH<-2, line=3, cex=1.5, col="black", outer=T)



all.coefs=c()
for(i in 1:length(coefs.list)){
  all.coefs=rbind(all.coefs,coefs.list[[i]])
}

IBW.index=grep('IBW',names(all.coefs), value=F)

y.min = min(0,min(all.coefs[,IBW.index]))
y.max = max(all.coefs[,IBW.index])

## coefficients boxplot summary (region-level)
par(mfrow=c(length(coefs.list),1),ps=16)

par(mfrow=c(length(unique(all.coefs$region)),1),ps=16
    ,oma=c(7,6,1,1)+0.1
    ,mar=c(0.5,1,2.5,0.5)+0.1)
for(i in 1:length(unique(all.coefs$region))){
  temp.region=unique(all.coefs$region)[i]
  boxplot(all.coefs[which(all.coefs$region==temp.region),IBW.index],main=temp.region,col='lightgray',frame.plot=F,
          xaxt="n",ylim=c(y.min,y.max))
 # if(i==length(unique(all.coefs$region))){
    axis(side = 1,at=c(1,26),labels=c("Jan","Dec"))
#  }else(axis(side=1,at=c(1,26),labels=c("","")))
} 
mtext("biweek", SOUTH<-1, line=3, cex=1.1, col="black", outer=T)
mtext("coefficient", SOUTH<-2, line=4, cex=1.1, col="black", outer=T)

region.coefs <- by(
  all.coefs
  ,factor(all.coefs$region
          , levels= unique(all.coefs$region)
  )
  ,function(temp.dat){
    temp.region=unique(temp.dat$region)
    
    return(temp.dat)
  }
)
names(region.coefs)=unique(all.coefs$region)
summary(region.coefs[[1]])

## helper function for analyzing coefficients from subnational-level stan models

get.stan.coefs <- function(temp.coef){
  temp.coefs=c()
  for(temp.loc in unique(incidence.dat$location)){ 
    temp.dat=subset(incidence.dat,location==temp.loc)
    
    temp.stan=stan.models[[temp.loc]]
    
    coef.intervals=data.frame(location=temp.loc
                              ,region=factor(attCountry$getRegion(temp.loc),levels=attCountry$uniqueRegions())
                              ,interval.95.lower=as.numeric(temp.stan$stan_summary[temp.coef,"2.5%"])
                              ,mean.coef=as.numeric(temp.stan$stan_summary[temp.coef,"mean"])
                              ,interval.95.upper=as.numeric( temp.stan$stan_summary[temp.coef,"97.5%"])
    )
    temp.coefs=rbind(temp.coefs,coef.intervals)
  }
  colnames(temp.coefs)=c("location","region","lower","mean","upper")
  temp.coefs=as.data.frame(temp.coefs)
  return(temp.coefs)
}

