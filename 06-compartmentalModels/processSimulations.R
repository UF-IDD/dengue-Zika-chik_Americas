## Load compartmental model simulations 
## Note: parts of this code will not run since they require the full 
## set of simulations (which were too large to provide).

require(dplyr)
require(tidyr)
require(ggplot2)
library(gridExtra)

#setwd("~/dengue-Zika-chik_Americas")

source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")

source("06-compartmentalModels/indexing_functions.R")
source("06-compartmentalModels/indeces.R")

# ########################################################
# ## original simulations (including all R_0 sceanrios)
# 
# original.all = T
# R0.scenarios= c("R0D_2p0_R0Z_2p0","R0D_2p0_R0Z_4p0","R0D_4p0_R0Z_2p0","R0D_4p0_R0Z_4p0","R0D_3p0_R0Z_3p0")
# 
# wkdir = FILL 
# setwd(wkdir)
# 
# for(R0.scenario in R0.scenarios){
# #  wkdir = FILL_containing_R0.scenario
#  # setwd(wkdir)
#   dirs_to_load=c(dirs_to_load,list.dirs(path = ".", full.names = TRUE, recursive = TRUE))
# }
# dirs_to_load=grep("T_zika_intro_0",dirs_to_load,value=T)
# dirs_to_load=grep("ZIKV_intro",dirs_to_load,value=T)
# dirs_to_load=grep("R0D",dirs_to_load,value=T)
# 
## Set the ZIKV introduction time
# intro.ZIKV.t = 100 
# ########################################################

# ########################################################
# ## original simulations (presented in Figure 4)
# original.sims = T
# R0.scenario= "R0D_4p0_R0Z_2p0"
# wkdir = FILL_containing_R0.scenario
# setwd(wkdir)
# dirs_to_load=list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
# dirs_to_load=grep("T_zika_intro_0",dirs_to_load,value=T)
# 
# # Set the ZIKV introduction time
# intro.ZIKV.t = 100
# ########################################################
 
# ########################################################
# ## simuations with sequential introduction of DENV serotypes
# original.sims = F
# R0.scenario= "R0D_4p0_R0Z_2p0"
# wkdir= = FILL_containing_R0.scenario
# setwd(wkdir)
# dirs_to_load=list.dirs(path = ".", full.names = TRUE, recursive = TRUE)
# dirs_to_load=grep("T_zika_intro_0",dirs_to_load,value=T)
# 
# intro.ZIKV.t = 40
# ########################################################


# Input directories
# Note: this depends on how the simulations are saved
input <- lapply(
  dirs_to_load #
  , function(datadir){
    do.call(list, lapply( #rbind
      list.files(datadir, pattern='^output_tau_coarse', full.names=T) #pattern='.dat$'
      , read.delim
      , sep = ""
      , header = F
      , stringsAsFactors=F
    ))
  }
)
sim.scenarios=gsub("/zika_AR_0p0/T_zika_intro_0","",dirs_to_load)
sim.scenarios=gsub("/ZIKV_intro","",sim.scenarios)
sim.scenarios=sub("\\./","",sim.scenarios)
names(input)=sim.scenarios
input.summary=summary(input)

sim.nums=1:length(input[[1]])
sim.names=paste0("sim",sim.nums)

for(i in 1:length(input)){
  names(input[[i]])=sim.names
}


# get mean prevalence for determining trough cutoff
cutoff.factor=0.5
temp.data=input[[9]] # no enhancement and no cross-protection scenario #20% reduction - rho=0.8 input[[9]] #80% reduction - rho=0.2 input[[1]] 

num.times=c()
for(j in 1:length(temp.data)){
  num.times=c(num.times,length(which(temp.data[[j]][,1]>intro.ZIKV.t)))
}

denguePrev=c()

for(j in 1:length(temp.data)){

  temp.sim=temp.data[[j]][which(temp.data[[j]][,1]>intro.ZIKV.t),]
  last.row.ind=dim(temp.sim)[1]
  
  if(last.row.ind < max(num.times)){
    num.rows=max(num.times)-last.row.ind
    tail.mat=matrix(temp.sim[last.row.ind,],nrow=num.rows,ncol=length(temp.sim[last.row.ind,]),byrow=TRUE)
    temp.sim=rbind(temp.sim,tail.mat)   
    temp.sim=data.matrix(temp.sim)
  }
  
  #temp.sim=temp.sim[which(temp.sim[,1]>intro.ZIKV.t),]
  temp.dat=temp.sim[,2:dim(temp.sim)[2]]
  temp.dat=unname(as.matrix(temp.dat,dimnames=NULL))
  
  dengue.inf.h=as.vector(human_dengue)  
  
  human.pop=rowSums(temp.dat[,as.vector(human_zika)])
  
  denguePrev=rbind(denguePrev,rowSums(temp.dat[,dengue.inf.h])/human.pop)
}
trough.cutoff=cutoff.factor*mean(denguePrev)


sim.summary.list <- lapply(
  input
  ,function(temp.data){
    sim.summary=c()
    for(j in 1:length(temp.data)){
      temp.sim=temp.data[[j]]
      temp.sim=temp.sim[which(temp.sim[,1]>intro.ZIKV.t),]        
      temp.t=temp.sim[,1]
      temp.dat=temp.sim[,2:dim(temp.sim)[2]]
      
      dengue.inf.h=as.vector(human_dengue) ##### human_dengue defined in "indeces.R"
      
      human.pop=rowSums(temp.dat[,as.vector(human_zika)])
      denguePrev=rowSums(temp.dat[,dengue.inf.h])/human.pop
      
      temp.peak=max(denguePrev)
      temp.time=temp.t[which(denguePrev==temp.peak)]
      
      trough.t1=temp.t[which(denguePrev<trough.cutoff)[1]]
      
      #### Filter for adjacent points (below the cutoff)
      #### that are more than 1 position apart. Choose the first one.
      trough.steps=which(diff(which(denguePrev<trough.cutoff))>1)[1]
      trough.t2=temp.t[which(denguePrev<trough.cutoff)[trough.steps]]
      
      sim.summary=rbind(sim.summary
                        ,c(temp.peak
                           ,temp.time
                           ,trough.t1=trough.t1
                           ,trough.t2=trough.t2
                           ,trough.dur=trough.t2-trough.t1))
    }
    return(sim.summary)
  }
  
)

# Choose whether to assign a value of for simulations that did not contain
# troughs 
troughNAs_to_0=T

sim.summary.vals <- lapply(
  sim.summary.list
  ,function(sim.data){
    sim.data=as.data.frame(sim.data)
    
    if(troughNAs_to_0==T){
      sim.data[is.na(sim.data)]=0
    }
    
    sum.means = c(colMeans(sim.data,na.rm=T))
    
    sim.quantiles=c()
    sim.medians=c()
    for(i in 1:dim(sim.data)[2]){
      sim.quantiles=rbind(sim.quantiles
                          ,quantile(sim.data[,i],probs=c(0.025,0.975),na.rm=T))
      sim.medians=rbind(sim.medians
                        ,median(sim.data[,i]))
    }
    summary.metrics=cbind(mean=sum.means,median=sim.medians,sim.quantiles)
    colnames(summary.metrics)=c("mean","median","2.5%","97.5%")
    return(summary.metrics)
  }
)

sim.summary.vals

sim.summary=c()
for(i in 1:length(sim.summary.vals)){
  sim.summary=rbind(sim.summary, as.data.frame(sim.summary.vals[[i]]) 
                    %>% mutate(scenario=names(sim.summary.vals)[i])
                    %>% mutate(metric = row.names(sim.summary.vals[[i]])))
}

sim.summary$metric[which(sim.summary$metric=="V1")]="temp.peak"
sim.summary$metric[which(sim.summary$metric=="V2")]="temp.time"
subset(sim.summary,metric=="temp.peak")
subset(sim.summary,metric=="temp.time")

strsplit(sim.summary$scenario,"/")[[1]][1]

cross.protect=c()
waning.rate=c()
enhancement=c()
for(i in 1:dim(sim.summary)[1]){
  cross.protect=c(cross.protect,strsplit(sim.summary$scenario,"/")[[i]][1])
  waning.rate=c(waning.rate,strsplit(sim.summary$scenario,"/")[[i]][2])
  enhancement=c(enhancement,strsplit(sim.summary$scenario,"/")[[i]][3])
}

sim.summary=cbind(sim.summary,cross.protect,waning.rate,enhancement)
sim.summary=sim.summary[,-which(names(sim.summary)=="scenario")]



# find increase in peak size for scenarios without enhancement where the
# baseline = rho_DZ_ZD_0p8 gamma_z_100p0     phi_1p0
peak.no.enh=subset(subset(sim.summary,metric=="temp.peak"),enhancement=="phi_1p0")
baseline=peak.no.enh$mean[3]
baseline.row=peak.no.enh[3,]

temp.order=order(subset(subset(sim.summary,metric=="temp.peak"),enhancement=="phi_1p0")$mean)
#peak.rows=peak.no.enh[temp.order,]
peak.rows = subset(sim.summary,metric=="temp.peak")

peak.size.data <- peak.rows %>%
  mutate(mean = mean
         , median = median
         , `2.5%` = `2.5%`
         , `97.5%` = `97.5%`)

temp.names1=c()
temp.names2=c()
for(i in 1:dim(peak.size.data)[1]){
  if(peak.size.data$waning.rate[i] == "gamma_z_100p0"){
    temp.names1=c(temp.names1,"no cross-protection ")
  }else{
    temp.names1=c(temp.names1,"cross-protection ")
  }
  if(peak.size.data$cross.protect[i] == "rho_DZ_ZD_0p2"){
    temp.names2=c(temp.names2,"(high)")
  }else{
    temp.names2=c(temp.names2,"(low)")
  }
}

peak.size.data <- peak.size.data %>% 
  mutate(name = paste0(temp.names1,temp.names2))

peak.increase.data <- peak.no.enh[temp.order,] %>%
  mutate(mean = mean/baseline.row$mean
         , median = median/baseline.row$median
         , `2.5%` = `2.5%`/baseline.row$`2.5%`
         , `97.5%` = `97.5%`/baseline.row$`97.5%`)

peak.increase.data <- peak.increase.data %>% 
  mutate(name = c("no cross-protection (low)","no cross-protection (high)","cross-protection (low)","cross-protection (high)"))


# largest peak size out of all scenarios
peak.subset=subset(sim.summary,metric=="temp.peak")
peak.subset[which(peak.subset$mean==max(peak.subset$mean)),]


# should not consider the redundant scenarios (not shown in Fig. 4; i.e.  rho_DZ_ZD_0p2 gamma_z_100p0)
subset(sim.summary,metric=="trough.dur")
max(subset(sim.summary,metric=="trough.dur")$mean)
min(subset(sim.summary,metric=="trough.dur")$mean)

# only consider scenarios with expected duration of cross protection = 1 year 
cross.protect.sub <- filter(subset(sim.summary,metric=="trough.dur"),waning.rate=="gamma_z_1p0")
max(cross.protect.sub$mean)
min(cross.protect.sub$mean)

trough.dur = subset(sim.summary,metric=="trough.dur")

trough.group=c()
for(temp.enh in c("phi_1p0","phi_DD_1p6","phi_DD_DZ_1p6","phi_DD_DZ_ZD_1p6")){
  trough.dur.temp = trough.dur %>% filter(enhancement==temp.enh)
  trough.row1 <- trough.dur.temp %>% filter(waning.rate=="gamma_z_100p0" & cross.protect=="rho_DZ_ZD_0p8") %>% mutate(name="no cross-protection (low)")
  trough.row2 <-trough.dur.temp %>% filter(waning.rate=="gamma_z_100p0" & cross.protect=="rho_DZ_ZD_0p2") %>% mutate(name="no cross-protection (high)")
  trough.row3 <-trough.dur.temp %>% filter(waning.rate=="gamma_z_1p0" & cross.protect=="rho_DZ_ZD_0p8") %>% mutate(name="cross-protection (low)")
  trough.row4 <-trough.dur.temp %>% filter(waning.rate=="gamma_z_1p0" & cross.protect=="rho_DZ_ZD_0p2") %>% mutate(name="cross-protection (high)")

  trough.temp <- trough.row1 %>% bind_rows(trough.row2) %>% bind_rows(trough.row3) %>% bind_rows(trough.row4) 
  trough.group <- trough.group %>% bind_rows(trough.temp)
}

record.times=seq(intro.ZIKV.t,intro.ZIKV.t+10,by=0.01)
#record.times=seq(0,60,by=0.01)

prevalence.list <- lapply(
  input
  ,function(temp.data){
    denguePrev.mat=c()
    for(j in 1:length(temp.data)){
      temp.sim=temp.data[[j]]
      temp.t=temp.sim[,1]
      temp.dat=temp.sim[,2:dim(temp.sim)[2]]
      
      dengue.inf.h=as.vector(human_dengue) 
      
      human.pop=rowSums(temp.dat[,as.vector(human_zika)])
      denguePrev=rowSums(temp.dat[,dengue.inf.h])/human.pop
      
      keep.rows=min(which(temp.t>=record.times[1]))
      for(i in 2:length(record.times)){
        keep.rows=c(keep.rows,min(which(temp.t>=record.times[i])))
      }
      new.denguePrev=denguePrev[keep.rows]
      
      denguePrev.mat=cbind(denguePrev.mat,new.denguePrev)
    }
    return(denguePrev.mat)
  }
  
)

prev.summary.vals <- lapply(
  prevalence.list
  ,function(sim.data){
    sum.means = c(rowMeans(sim.data,na.rm=T))
    
    sim.quantiles=c()
    sim.medians=c()
    for(i in 1:dim(sim.data)[1]){
      sim.quantiles=rbind(sim.quantiles
                          ,quantile(sim.data[i,],probs=c(0.025,0.975),na.rm=T))
      sim.medians=rbind(sim.medians
                        ,median(sim.data[i,]))
    }
    
    summary.metrics=cbind(mean=sum.means,median=sim.medians,sim.quantiles)
    colnames(summary.metrics)=c("mean","median","2.5%","97.5%")
    return(summary.metrics)
  }
)


setwd("~/dengue-Zika-chik_Americas")
if(original.sims == T){
  trough.data.ZIKV100 <- trough.group %>% mutate(ZIKV_intro="ZIKV_intro_100")
  peak.size.data.ZIKV100 <- peak.size.data %>% mutate(ZIKV_intro="ZIKV_intro_100")
  peak.increase.data.ZIKV100 <- peak.increase.data %>% mutate(ZIKV_intro="ZIKV_intro_100")
  trough.cutoff.ZIKV100 <- trough.cutoff
  baseline.peak.ZIKV100 <- baseline.row
  prev.summary.vals.ZIKV100 <- prev.summary.vals
  save(trough.data.ZIKV100,peak.size.data.ZIKV100,peak.increase.data.ZIKV100,trough.cutoff.ZIKV100,baseline.peak.ZIKV100,prev.summary.vals.ZIKV100
       ,file = "06-compartmentalModels/sims_output/data_ZIKV100.Rdata")
}else{
  trough.data.ZIKV40 <- trough.group %>% mutate(ZIKV_intro="ZIKV_intro_40")
  peak.size.data.ZIKV40 <- peak.size.data %>% mutate(ZIKV_intro="ZIKV_intro_40")
  peak.increase.data.ZIKV40 <- peak.increase.data %>% mutate(ZIKV_intro="ZIKV_intro_40")
  trough.cutoff.ZIKV40 <- trough.cutoff
  baseline.peak.ZIKV40 <- baseline.row
  prev.summary.vals.ZIKV40 <- prev.summary.vals
  save(trough.data.ZIKV40,peak.size.data.ZIKV40,peak.increase.data.ZIKV40,trough.cutoff.ZIKV40,baseline.peak.ZIKV40,prev.summary.vals.ZIKV40
       ,file = "06-compartmentalModels/sims_output/data_ZIKV40.Rdata")
}

## Analysis of DENV dynamics prior ot the introduction of ZIKV
# Note that it does not matter which set of DENV-ZIKV interaction simulations is loaded 
# (since there is no interaction prior to ZIKV introduction)
temp.data=input[[9]] # no enhancement and no cross-protection scenario
record.times=seq(0,100,by=0.01)

# view the indeces for cumulative DENV incidence for each serotype
# (defined in "indeces.R")
inc_dengue

# extract serotype specific DENV incidence time-series for each simulation in temp.data
dengueInc.mat=c()
for(j in 1:length(temp.data)){
  temp.sim=temp.data[[j]]
  temp.t=temp.sim[,1]
  temp.dat=temp.sim[,2:dim(temp.sim)[2]]
  
 
  new.dengueInc=c()
  for(i in 1:4){
    # here diff is used to convert cumulative incidence to incidence and
    # the rowSums are aggregating over ZIKV status 
    seroInc=data.frame(sim=paste0("sim",j)
                       , time=temp.t[-1]
                       , serotype=paste0("DENV",i)
                       , incidence = diff(rowSums(temp.dat[,inc_dengue[i,]])))
    new.dengueInc=rbind(new.dengueInc,seroInc)
  }
  dengueInc.mat=rbind(dengueInc.mat,new.dengueInc)
}

simulations=unique(dengueInc.mat$sim)

# add total DENV incidence for each time and simulation
total.dat <- summarise(dat %>% group_by(sim,time),incidence=sum(incidence)) %>% mutate(serotype="total")
dengueInc.mat <- union_all(dengueInc.mat,total.dat)


## Supplementary Figure 12
## note that the following will neet to be run twice - once saved as dat for the stable state simulations
## and once saved as new.dat with dengueInc.mat %>% filter(time>=20 & time<40) for the recently
## introduced DENV simulations
dat <- dengueInc.mat %>% filter(time>=80 & time<100) %>%
  filter(sim %in% simulations[1:12]) %>%
  filter(serotype != "total")
original.sims.plot=ggplot(dat,aes(x=time,y=incidence,color=serotype))+
  geom_line(size=1.2,alpha=0.6)+
  facet_wrap(vars(sim))+
  theme_linedraw()+
  theme(strip.text.x = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(colour="#dddddd",size=0.5)
        ,text = element_text(size = 14))+
  labs(x="\nTime (years)",y="DENV incidence\n",colour="Serotype",tag="a")

new.sims.plot=ggplot(new.dat,aes(x=time,y=incidence,color=serotype))+
  geom_line(size=1.2,alpha=0.6)+
  # scale_colour_manual(values=cbbPalette)+
  facet_wrap(vars(sim))+
  theme_linedraw()+
  theme(strip.text.x = element_blank()
        ,panel.grid.minor = element_blank()
        ,panel.grid.major = element_line(colour="#dddddd",size=0.5)
        ,text = element_text(size = 14))+
  labs(x="\nTime since DENV introduction (years)",y="DENV incidence\n",colour="Serotype",tag="b")



grid.arrange(original.sims.plot,new.sims.plot,nrow=2)

# or could label x=axis time until ZIKV introduction years, going from 20 to 0
# save as 9x9 

