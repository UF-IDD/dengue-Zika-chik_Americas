## permutation test for Figure 2

library("stats")
library("tidyr")
library("dplyr")

args <- commandArgs(trailingOnly=T)

country <- args[1]
#permute.mFile <- args[2]
print(args)

load(country) # country.choice
#load(permute.mFile) # permute.m

# choose where to save files
outdir = ""

replace.years=F

true.signif.blues=c()
true.signif.reds=c()
for(temp.year in unique(permute.m$year)){
  temp=subset(permute.m,year==temp.year)
  true.signif.blues=c(true.signif.blues,length(which(temp$significant==-1)))
  true.signif.reds=c(true.signif.reds,length(which(temp$significant==1)))
}


num.perms=10000#1000

## permute years 
signif.blues=c()
signif.reds=c()

for(perm in 1:num.perms){
  new.year=c()
  for(temp.loc in unique(permute.m$location)){
    temp.m=subset(permute.m,location==temp.loc)  
    
    years=unique(temp.m$year)
    new.years=sample(years,length(years),replace = replace.years)
    
    for(i in 1:length(years)){
      temp.year=years[i]
      new.year=c(new.year,rep(new.years[i],length(grep(temp.year,temp.m$year))))
    }
    
  }
  
  permute.m = permute.m %>% as.data.frame %>%
    mutate(
        permuted.year=new.year
    )
  
  num.signif.blues=c()
  num.signif.reds=c()
  
  for(temp.year in unique(permute.m$year)){
    temp=subset(permute.m,permuted.year==temp.year)
    num.signif.blues=c(num.signif.blues,length(which(temp$significant==-1)))
    num.signif.reds=c(num.signif.reds,length(which(temp$significant==1)))
  }
  signif.blues=rbind(signif.blues,num.signif.blues)
  signif.reds=rbind(signif.reds,num.signif.reds)
}
colnames(signif.blues)=unique(permute.m$year)
colnames(signif.reds)=unique(permute.m$year)

blue.q.vals=c()
red.q.vals=c()
for(i in 1:length(true.signif.blues)){
  blue.q.vals=c(blue.q.vals,ecdf(signif.blues[,i])(true.signif.blues[i]))
  red.q.vals=c(red.q.vals,ecdf(signif.reds[,i])(true.signif.reds[i]))
}

if(country.choice=="Brazil"){
  Brazil.signif.blues=true.signif.blues
  Brazil.signif.reds=true.signif.reds
  Brazil.blue.q.vals=blue.q.vals
  Brazil.red.q.vals=red.q.vals
}else{
  Colombia.signif.blues=true.signif.blues
  Colombia.signif.reds=true.signif.reds
  Colombia.blue.q.vals=blue.q.vals
  Colombia.red.q.vals=red.q.vals
}

##################
## permute biweeks
##################
signif.blues2=c()
signif.reds2=c()

years=unique(permute.m$year)

for(perm in 1:num.perms){
  permute.biweek.dat=c()
  for(j in 1:26){
    temp.dat=subset(permute.m,biweek==j)  
    if((j == 1) & (country.choice=="Colombia")){
      new.years=as.numeric(sample(years[-1],length(years)-1,replace = replace.years))
    }else{
      new.years=as.numeric(sample(years,length(years),replace = replace.years))      
    }
    
    year.dat=c()
    for(i in 1:length(years)){
      temp.dat2=subset(temp.dat,year==years[i])
      
      new.dat=subset(temp.dat, year==new.years[i])
      
      new.dat =new.dat %>% as.data.frame %>%
        mutate(
          original.year=years[i]
        )
      
      year.dat=rbind(year.dat,new.dat)
    }
    permute.biweek.dat=rbind(permute.biweek.dat,year.dat)
    
  }
  if(country.choice=="Colombia"){
    permute.biweek.dat=permute.biweek.dat[-which((permute.biweek.dat$original.year==2007)&(permute.biweek.dat$biweek==1)),]      
  }
  
  
  num.signif.blues=c()
  num.signif.reds=c()
  
  ## significant blues and reds
  #  for(temp.year in unique(permute.biweek.dat$original.year)){
  for(temp.year in years){
    temp = subset(permute.biweek.dat,original.year==temp.year)
    
    num.signif.blues=c(num.signif.blues,length(which(temp$significant==-1)))
    num.signif.reds=c(num.signif.reds,length(which(temp$significant==1)))
  }
  signif.blues2=rbind(signif.blues2,num.signif.blues)
  signif.reds2=rbind(signif.reds2,num.signif.reds)
}

colnames(signif.blues2)=unique(permute.biweek.dat$original.year)
colnames(signif.reds2)=unique(permute.biweek.dat$original.year)


blue.q2.vals=c()
red.q2.vals=c()
for(i in 1:length(true.signif.blues)){
  blue.q2.vals=c(blue.q2.vals,ecdf(signif.blues2[,i])(true.signif.blues[i]))
  red.q2.vals=c(red.q2.vals,ecdf(signif.reds2[,i])(true.signif.reds[i]))
}

if(country.choice=="Brazil"){
  Brazil.blue.q2.vals=blue.q2.vals
  Brazil.red.q2.vals=red.q2.vals
}else{
  Colombia.blue.q2.vals=blue.q2.vals
  Colombia.red.q2.vals=red.q2.vals
}

file.name=paste0(outdir,country.choice,".Rdata")
if(country.choice=="Brazil"){
  save(Brazil.signif.blues,Brazil.signif.reds
    ,Brazil.blue.q.vals,Brazil.red.q.vals
    ,Brazil.blue.q2.vals,Brazil.red.q2.vals,file=file.name)
}else{
  save(Colombia.signif.blues,Colombia.signif.reds
       ,Colombia.blue.q.vals,Colombia.red.q.vals
       ,Colombia.blue.q2.vals,Colombia.red.q2.vals,file=file.name)
}
