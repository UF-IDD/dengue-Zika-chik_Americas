setwd("~/Documents/dengueZika")
source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")
source("Scripts/04-set_location_order.R")

library(dplyr)

# generate and save incidence.dat files for the subnational-level stan models
country.choice="Brazil"

## load incidence data to analyze
if(country.choice=="Brazil"){
    incidenceCsv = "05-simulationSummary/Dengue-Brazil-probable/Dengue-Brazil-probable-tsCountMid/incidence.csv"   
    ZincidenceCsv = "05-simulationSummary/Zika-Brazil/Zika-Brazil-tsCountMid/incidence.csv"
    CincidenceCsv = "05-simulationSummary/Chik-Brazil/Chik-Brazil-tsCountMid/incidence.csv"
  incidence.dat = read.csv(incidenceCsv, stringsAsFactors=F)
  Zincidence.dat = read.csv(ZincidenceCsv, stringsAsFactors = F)
  Cincidence.dat = read.csv(CincidenceCsv, stringsAsFactors = F)
  place.list=lat.ordered.Brazil
}

incidence.dat=  merge(incidence.dat,Zincidence.dat,by=c("location","biweekEndDate","year","biweek","population"),all=T)
incidence.dat = incidence.dat[,-which(names(incidence.dat) %in% c("varIncidence.x","varIncidence.y"))]
names(incidence.dat)[names(incidence.dat)%in% c("meanIncidence.x","meanIncidence.y")]=c("meanIncidence","ZmeanIncidence")
incidence.dat$ZmeanIncidence[is.na(incidence.dat$ZmeanIncidence)]=0

## make incidence.dat not include data from 2018
incidence.dat=subset(incidence.dat,year<2018)

#save(place.list,incidence.dat,file=paste0("~/",outdir,"/",filename,".Rdata"))

## generate Colombia incidence.data data frame
country.choice="Colombia"

if(country.choice=="Colombia"){
    incidenceCsv = "05-simulationSummary/Dengue-Colombia-Total/Dengue-Colombia-Total-tsCountMid/incidence.csv"
    ZincidenceCsv = "05-simulationSummary/Zika-Colombia/Zika-Colombia-tsCountMid/incidence.csv"
    CincidenceCsv = "05-simulationSummary/Chik-Colombia/Chik-Colombia-tsCountMid/incidence.csv"
  
  incidence.dat = read.csv(incidenceCsv, stringsAsFactors=F)
  Zincidence.dat = read.csv(ZincidenceCsv, stringsAsFactors = F)
  Cincidence.dat = read.csv(CincidenceCsv, stringsAsFactors = F)
  
  place.list = lat.ordered.CO
  place.list=place.list[-which(place.list=="Bogota")]
  place.list=place.list[-which(place.list=="Vaupes")]
  #place.list=place.list[-which(place.list=="Procedencia_Desconocida")] #Amazonas and Vichada may also be too low.  
}


incidence.dat=  merge(incidence.dat,Zincidence.dat,by=c("location","biweekEndDate","year","biweek","population"),all=T)
incidence.dat = incidence.dat[,-which(names(incidence.dat) %in% c("varIncidence.x","varIncidence.y"))]
names(incidence.dat)[names(incidence.dat)%in% c("meanIncidence.x","meanIncidence.y")]=c("meanIncidence","ZmeanIncidence")
incidence.dat$ZmeanIncidence[is.na(incidence.dat$ZmeanIncidence)]=0

## make incidence.dat not include data from 2018
incidence.dat=subset(incidence.dat,year<2018)
incidence.dat=subset(incidence.dat,year>2006)

# save(place.list,incidence.dat,file=paste0("~/",outdir,"/",filename,".Rdata"))

