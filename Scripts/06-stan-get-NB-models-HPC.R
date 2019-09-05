## using STAN to fit subnational negative binomial models

#load the libraries
library(arm) #for the invlogit function
library(emdbook) #for the rbetabinom function
library(rstan)
library(rstanarm) #for the launch_shinystan function

args <- commandArgs(trailingOnly=T)
incidence.data <- args[1]
print(args)

num.cores=4
load(incidence.data) #incidence.dat

# generate the negative binomial model for each location
get.stan.models=function(predict.year,temp.dat,log.version=T){
  year.out=predict.year
  
  temp.dat$meanIncidence=floor(temp.dat$meanIncidence)
  temp.dat$ZmeanIncidence=floor(temp.dat$ZmeanIncidence)
  
  temp.in1 = subset(temp.dat, year < year.out)
  temp.out = subset(temp.dat, year == year.out)
  temp.in2 = subset(temp.dat, year > year.out)
  
  if(year.out > min(temp.dat$year)){
    IBW.in1 <- array(0,c(dim(temp.in1)[1]-1,26))
    colnames(IBW.in1)=paste('IBW',1:26,sep='')
    for(i in 1:26){
      IBW.in1[which(temp.in1$biweek[-1]==i),i]=1
    }
    
    if(log.version==T){
      IBW.in1.inc = IBW.in1 * log(temp.in1$meanIncidence[-length(temp.in1$meanIncidence)]+1)
      Z.in1.inc =  log(temp.in1$ZmeanIncidence[-length(temp.in1$ZmeanIncidence)]+1)
    }else{
      IBW.in1.inc = IBW.in1 * temp.in1$meanIncidence[-length(temp.in1$meanIncidence)]
    }
    temp.dat.in1= as.data.frame(cbind(ct1=temp.in1$meanIncidence[-1],IBW.in1.inc,Zt=Z.in1.inc,population=temp.in1$population[-1],year=temp.in1$year[-1]))
  }else{
    IBW.in2 <- array(0,c(dim(temp.in2)[1]-1,26))
    colnames(IBW.in2)=paste('IBW',1:26,sep='')
    for(i in 1:26){
      IBW.in2[which(temp.in2$biweek[-1]==i),i]=1
    }
    
    if(log.version==T){
      IBW.in2.inc = IBW.in2 * log(temp.in2$meanIncidence[-length(temp.in2$meanIncidence)]+1)
      Z.in2.inc =  log(temp.in2$ZmeanIncidence[-length(temp.in2$ZmeanIncidence)]+1)
    }else{
      IBW.in2.inc = IBW.in2 * temp.in2$meanIncidence[-length(temp.in2$meanIncidence)]
    }
    temp.dat.in= as.data.frame(cbind(ct1=temp.in2$meanIncidence[-1],IBW.in2.inc,Zt=Z.in2.inc,population=temp.in2$population[-1],year=temp.in2$year[-1]))
  }
  
  if(year.out < max(temp.dat$year)){
    IBW.in2 <- array(0,c(dim(temp.in2)[1]-1,26))
    colnames(IBW.in2)=paste('IBW',1:26,sep='')
    for(i in 1:26){
      IBW.in2[which(temp.in2$biweek[-1]==i),i]=1
    }
    if(log.version==T){
      IBW.in2.inc = IBW.in2 * log(temp.in2$meanIncidence[-length(temp.in2$meanIncidence)]+1)
      Z.in2.inc = log(temp.in2$ZmeanIncidence[-length(temp.in2$ZmeanIncidence)]+1)
    }else{
      IBW.in2.inc = IBW.in2 * temp.in2$meanIncidence[-length(temp.in2$meanIncidence)]
    }
    temp.dat.in2= as.data.frame(cbind(ct1=temp.in2$meanIncidence[-1],IBW.in2.inc,Zt=Z.in2.inc,population=temp.in2$population[-1],year=temp.in2$year[-1]))
  }else{
    IBW.in1 <- array(0,c(dim(temp.in1)[1]-1,26))
    colnames(IBW.in1)=paste('IBW',1:26,sep='')
    for(i in 1:26){
      IBW.in1[which(temp.in1$biweek[-1]==i),i]=1
    }
    if(log.version==T){
      IBW.in1.inc = IBW.in1 * log(temp.in1$meanIncidence[-length(temp.in1$meanIncidence)]+1)
      Z.in1.inc = log(temp.in1$ZmeanIncidence[-length(temp.in1$ZmeanIncidence)]+1)
    }else{
      IBW.in1.inc = IBW.in1 * temp.in1$meanIncidence[-length(temp.in1$meanIncidence)]
    }
    temp.dat.in = as.data.frame(cbind(ct1=temp.in1$meanIncidence[-1],IBW.in1.inc,Zt=Z.in1.inc,population=temp.in1$population[-1],year=temp.in1$year[-1]))
  }
  
  if(year.out > min(temp.dat$year) & year.out < max(temp.dat$year)){
    temp.dat.in=rbind(temp.dat.in1,temp.dat.in2)
  }
  
  if(year.out>min(temp.dat$year)){
    temp.out=rbind(temp.in1[dim(temp.in1)[1],],temp.out)
  }
  
  IBW.out <- array(0, c(dim(temp.out)[1]-1,26)) #or NAs
  colnames(IBW.out)=paste('IBW',1:26,sep='')
  for(i in 1:26){
    IBW.out[which(temp.out$biweek[-1]==i),i]=1
  }
  
  if(log.version==T){
    IBW.out.inc = IBW.out * log(temp.out$meanIncidence[-length(temp.out$meanIncidence)]+1)
    Z.out.inc =  log(temp.out$ZmeanIncidence[-length(temp.out$ZmeanIncidence)]+1)
  }else{
    IBW.out.inc = IBW.out * temp.out$meanIncidence[-length(temp.out$meanIncidence)]
  }
  
  temp.dat.out= as.data.frame(cbind(ct1=temp.out$meanIncidence[-1],IBW.out.inc,Zt=Z.out.inc,population=temp.out$population[-1],year=temp.out$year[-1]))
  
  # negative binomial model formula
  model1.formula <- as.formula(
    paste(
      "ct1 ~ "
      ,paste(
        grep('IBW',colnames(temp.dat.in), value=T)
        ,collapse = "+"
      )
      ,"+offset(log(population))" #+Zt 
    )
  )
  

  ## STAN model
  stan.model=stan_glm.nb(formula = model1.formula,data = temp.dat.in,chains=4,link="log",cores=num.cores) #,offset=log(temp.dat.in$population)
  
  return(output=list(stan.model=stan.model))
}


stan.models <- by(
  incidence.dat
  ,factor(incidence.dat$location
          , levels= place.list
          )
  ,function(temp.dat){
    results <- lapply(
      unique(temp.dat$year)
      ,get.stan.models
      ,temp.dat=temp.dat
    )
    names(results) <- unique(temp.dat$year)
    results
  }
)

outdir = "~/outStan_models_"
outname = gsub('\\.Rdata$','',basename(incidence.data))

save(stan.models,file=paste0(outdir,outname,".Rdata"))

