## Generate m-files for Figure 2 and for running permutation test on cluster
## NOTE: this code requires the subnational stan models in order to run

library(rstan)
library(rstanarm)

## choose directory for saving output files
# outdir = ""

country.choice="Brazil"
## load subnational stan models and corresponding incidence.dat data frame
# load("...")
# load("...")


test.bayes.R2=c()

pred.intervals.list <- by(
  incidence.dat
  ,factor(incidence.dat$location
          , levels= place.list
  )
  ,function(temp.dat){
    temp.loc=unique(temp.dat$location)
    pred.interval.9=c()
    pred.interval.95=c()
    median.pred=c()
    observed=c()
    p.vals=c()
    date=c()
    temp.dates=c()
    temp.dates=temp.dat$biweekEndDate[-1]
    
    for(temp.year in unique(temp.dat$year)){
      
      year.out=temp.year
      
      temp.stan=stan.models[[temp.loc]][[as.character(year.out)]]$stan.model
      ###########
      temp.dat$meanIncidence=as.integer(floor(temp.dat$meanIncidence))
      
      getIBW <- function(input){
        if(nrow(input)==0){ return(NULL) }
        
        IBW <- t(apply(input, 1, function(x){
          IBW = rep(0,times=26)
          IBW[as.integer(x['biweek'])] <- 1
          IBW
        }))
        colnames(IBW) = paste0('IBW',1:26)
        
        IBW <- IBW[-1,] * log(input$meanIncidence[-nrow(input)]+1)
        
        as.data.frame(cbind(
          ct1 = input$meanIncidence[-1] 
          ,IBW
          ,population = input$population[-1]
          ,year = input$year[-1]
        ))
      }
      
      temp.dat.out <- getIBW(
        temp.out <- rbind(
          temp.dat %>% filter(year==(year.out-1), biweek==26)
          ,subset(temp.dat, year == year.out)
        )
      )  
      
      temp.offset=log(temp.dat.out$population)
      
      # get median of predicted values from posterior distribution
      test.postpred=posterior_predict(temp.stan,newdata=temp.dat.out,offset=temp.offset,draws=500) #100
      median.pred.new=as.numeric(apply(test.postpred, 2, median))
      median.pred=c(median.pred,median.pred.new)
      
      year.obs=as.numeric(temp.dat.out$ct1)
      p.val=c()
      for(i in 1:length(year.obs)){
        p.val=c(p.val,ecdf(test.postpred[,i])(year.obs[i]))
      }
      p.vals=c(p.vals,p.val)
      
      # get observed values
      observed=c(observed,as.numeric(temp.dat.out$ct1))
      
      # get predicted intervals
      pred.interval.9=rbind(pred.interval.9
                            , predictive_interval(temp.stan,newdata=temp.dat.out,offset=temp.offset,prob=.9))
      pred.interval.95=rbind(pred.interval.95
                             , predictive_interval(temp.stan,newdata=temp.dat.out,offset=temp.offset,prob=.95))
      
      test.bayes.R2=c(test.bayes.R2,rep(median(bayes_R2(temp.stan,newdata=temp.dat.out,offset=temp.offset)),length(median.pred.new)))  
    }
    
    pred.intervals=data.frame(bw$getYear(temp.dat$biweekEndDate[-1]),temp.loc,temp.dat$biweekEndDate[-1],observed,pred.interval.95[,1],pred.interval.9[,1],median.pred,pred.interval.9[,2],pred.interval.95[,2],p.vals
                              ,test.bayes.R2)   
    names(pred.intervals)=c("year","location","biweekEndDate","observed","2.5%","5%","median.pred","95%","97.5%","quantile","R2.out")
    return(pred.intervals)
  }
)

save(pred.intervals.list,file=paste0("Materials/predIntervals","_",country.choice,".Rdata"))

for(i in 1:length(pred.intervals.list)){
  print(dim(pred.intervals.list[[i]]))
}

m <- do.call(rbind, mapply(
  function(x,i){
    
    temp <- x %>% as.data.frame %>% 
      mutate(
        pindex = -1*i
      )
    
    yvals=temp$quantile
    yvals[yvals>.5]=1-yvals[yvals>.5]
    
    over.below = c(rep(1,length=length(temp$observed)))
    over.below[which(temp$quantile>0.5)]=2
    
    alpha.vals=rep(.1,length(over.below))
    alpha.vals[which(temp$observed < temp$`5%`)]=0.4
    alpha.vals[which(temp$observed > temp$`95%`)]=0.4
    alpha.vals[which(temp$observed < temp$`2.5%`)]=1
    alpha.vals[which(temp$observed > temp$`97.5%`)]=1
    
    significant=rep(0,length(temp$observed))
    significant[which(alpha.vals>=0.4)]=1
    
    p.vals.adjust=p.adjust(yvals,method="bonferroni")
    p.vals.adjust[which(p.vals.adjust>.05)]=.9
    
    test.colors=rep(0,length(over.below))
    test.colors.adjust=rep(0,length(over.below))
    for(i in 1:length(over.below)){
      if(over.below[i]==1){
        test.colors[i]=rgb(0,0,1,alpha=alpha.vals[i])
        test.colors.adjust[i]=rgb(0,0,1,alpha=1-p.vals.adjust[i])
        significant[i]=-significant[i]
      }else{
        test.colors[i]=rgb(1,0,0,alpha=alpha.vals[i])
        test.colors.adjust[i]=rgb(1,0,0,alpha=1-p.vals.adjust[i])
      }
    }
    
    temp %>%
      mutate(
        test.colors = test.colors,
        test.colors.adjust=test.colors.adjust,
        significant=significant,
        p.vals=yvals,
        p.vals.adjust=p.vals.adjust
      )
    
  }
  , x= pred.intervals.list
  , i= seq_along(pred.intervals.list)
  , SIMPLIFY = FALSE
))

# load original mfile
use.m=m 

permute.m=subset(use.m,year>1999)
permute.m$pindex = -attCountry$getPindexFromLocation(permute.m$location)

permute.m =permute.m %>% as.data.frame %>%
  mutate(
    biweek=bw$get(as.Date(biweekEndDate,origin="1970-01-01"))
  )

# save the permute.m File
filename=paste0("outdir",country.choice,".Rdata")
save(country.choice,permute.m,file=filename)

######## 
country.choice="Colombia"
## load subnational stan models and corresponding incidence.dat data frame
# load("...")
# load("...")

test.bayes.R2=c()

pred.intervals.list <- by(
  incidence.dat
  ,factor(incidence.dat$location
          , levels= place.list
  )
  ,function(temp.dat){
    temp.loc=unique(temp.dat$location)
    pred.interval.9=c()
    pred.interval.95=c()
    median.pred=c()
    observed=c()
    p.vals=c()
    date=c()
    temp.dates=c()
    temp.dates=temp.dat$biweekEndDate[-1]
    
    for(temp.year in unique(temp.dat$year)){
      
      year.out=temp.year
      
      temp.stan=stan.models[[temp.loc]][[as.character(year.out)]]$stan.model
      ###########
      temp.dat$meanIncidence=as.integer(floor(temp.dat$meanIncidence))
      
      getIBW <- function(input){
        if(nrow(input)==0){ return(NULL) }
        
        IBW <- t(apply(input, 1, function(x){
          IBW = rep(0,times=26)
          IBW[as.integer(x['biweek'])] <- 1
          IBW
        }))
        colnames(IBW) = paste0('IBW',1:26)
        
        IBW <- IBW[-1,] * log(input$meanIncidence[-nrow(input)]+1)
        
        as.data.frame(cbind(
          ct1 = input$meanIncidence[-1] 
          ,IBW
          ,population = input$population[-1]
          ,year = input$year[-1]
        ))
      }
      
      temp.dat.out <- getIBW(
        temp.out <- rbind(
          temp.dat %>% filter(year==(year.out-1), biweek==26)
          ,subset(temp.dat, year == year.out)
        )
      )  
      
      temp.offset=log(temp.dat.out$population)
      
      # get median of predicted values from posterior distribution
      test.postpred=posterior_predict(temp.stan,newdata=temp.dat.out,offset=temp.offset,draws=500) #100
      median.pred.new=as.numeric(apply(test.postpred, 2, median))
      median.pred=c(median.pred,median.pred.new)
      
      year.obs=as.numeric(temp.dat.out$ct1)
      p.val=c()
      for(i in 1:length(year.obs)){
        p.val=c(p.val,ecdf(test.postpred[,i])(year.obs[i]))
      }
      p.vals=c(p.vals,p.val)
      
      # get observed values
      observed=c(observed,as.numeric(temp.dat.out$ct1))
      
      # get predicted intervals
      pred.interval.9=rbind(pred.interval.9
                            , predictive_interval(temp.stan,newdata=temp.dat.out,offset=temp.offset,prob=.9))
      pred.interval.95=rbind(pred.interval.95
                             , predictive_interval(temp.stan,newdata=temp.dat.out,offset=temp.offset,prob=.95))
      
      test.bayes.R2=c(test.bayes.R2,rep(median(bayes_R2(temp.stan,newdata=temp.dat.out,offset=temp.offset)),length(median.pred.new)))  
    }
    
    pred.intervals=data.frame(bw$getYear(temp.dat$biweekEndDate[-1]),temp.loc,temp.dat$biweekEndDate[-1],observed,pred.interval.95[,1],pred.interval.9[,1],median.pred,pred.interval.9[,2],pred.interval.95[,2],p.vals
                              ,test.bayes.R2) #temp.dat$biweekEndDate      
    names(pred.intervals)=c("year","location","biweekEndDate","observed","2.5%","5%","median.pred","95%","97.5%","quantile","R2.out")
    return(pred.intervals)
  }
)

save(pred.intervals.list,file=paste0("Materials/predIntervals","_",country.choice,".Rdata"))


m <- do.call(rbind, mapply(
  function(x,i){
    
    temp <- x %>% as.data.frame %>% 
      mutate(
        pindex = -1*i
      )
    
    yvals=temp$quantile
    yvals[yvals>.5]=1-yvals[yvals>.5]
    
    over.below = c(rep(1,length=length(temp$observed)))
    over.below[which(temp$quantile>0.5)]=2
    
    alpha.vals=rep(.1,length(over.below))
    alpha.vals[which(temp$observed < temp$`5%`)]=0.7
    alpha.vals[which(temp$observed > temp$`95%`)]=0.7
    alpha.vals[which(temp$observed < temp$`2.5%`)]=1
    alpha.vals[which(temp$observed > temp$`97.5%`)]=1
    
    significant=rep(0,length(temp$observed))
    significant[which(alpha.vals>=0.7)]=1
    
    p.vals.adjust=p.adjust(yvals,method="bonferroni")
    p.vals.adjust[which(p.vals.adjust>.01)]=.9 # need to adjust when there are more values than just 0 and 1
    
    test.colors=rep(0,length(over.below))
    test.colors.adjust=rep(0,length(over.below))
    for(i in 1:length(over.below)){
      if(over.below[i]==1){
        test.colors[i]=rgb(0,0,1,alpha=alpha.vals[i])
        test.colors.adjust[i]=rgb(0,0,1,alpha=1-p.vals.adjust[i])
        significant[i]=-significant[i]
      }else{
        test.colors[i]=rgb(1,0,0,alpha=alpha.vals[i])
        test.colors.adjust[i]=rgb(1,0,0,alpha=1-p.vals.adjust[i])
      }
    }
    
    temp %>%
      mutate(
        test.colors = test.colors,
        test.colors.adjust=test.colors.adjust,
        significant=significant,
        p.vals=yvals,
        p.vals.adjust=p.vals.adjust
      )
    
  }
  , x= pred.intervals.list
  , i= seq_along(pred.intervals.list)
  , SIMPLIFY = FALSE
))

# load original mfile
use.m=m 

permute.m=subset(use.m,year>1999)
permute.m$pindex = -attCountry$getPindexFromLocation(permute.m$location)

permute.m =permute.m %>% as.data.frame %>%
  mutate(
    biweek=bw$get(as.Date(biweekEndDate,origin="1970-01-01"))
  )

# save the permute.m File
filename=paste0("outdir",country.choice,".Rdata")
save(country.choice,permute.m,file=filename)
