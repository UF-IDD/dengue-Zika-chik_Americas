# generate ibw with random years of dengue for recent years

setwd("~/dengue-Zika-chik_Americas")

library(dplyr)
library(ggplot2)
library(grid)
library(gridExtra)

country.choice='Colombia' #'Brazil' 

if(country.choice=='Brazil'){
  # Brazil settings
  dengueCsv = "05-simulationSummary/Dengue-Brazil-probable/Dengue-Brazil-probable-tsCountMid/incidence.csv"
  zikaCsv = "05-simulationSummary/Zika-Brazil/Zika-Brazil-tsCountMid/incidence.csv"
  mcCsv = "05-simulationSummary/Microcephaly-Brazil/Microcephaly-Brazil-tsCountMid/incidence.csv"
  chikCsv = "05-simulationSummary/Chik-Brazil/Chik-Brazil-tsCountMid/incidence.csv"
  country = "Brazil"
  outdir = "06-analyze"
} else if (country.choice=='Colombia'){
  # Colombia settings
  dengueCsv = "05-simulationSummary/Dengue-Colombia-Total/Dengue-Colombia-Total-tsCountMid/incidence.csv"
  zikaCsv = "05-simulationSummary/Zika-Colombia/Zika-Colombia-tsCountMid/incidence.csv"
  mcCsv = ""
  chikCsv = "05-simulationSummary/Chik-Colombia/Chik-Colombia-tsCountMid/incidence.csv"
  country = "Colombia"
  outdir = "06-analyze"
} else {
  stop("Set 'country.choice' to either 'Colombia' or 'Brazil'")   
}



source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")
attCountry <- get(paste0('att',country))


importOptionalData <- function(csvfile){
  if(is.na(csvfile)|is.null(csvfile)|csvfile==""){
    return(NULL)
  } else {
    return(read.csv(csvfile, stringsAsFactors = F))
  }
}

inputDengue <- read.csv(dengueCsv, stringsAsFactors = F)
inputZika <- read.csv(zikaCsv, stringsAsFactors = F)
inputMc <- importOptionalData(mcCsv)
inputChik <- importOptionalData(chikCsv)

place.list <- attCountry$uniqueLocationsNonNA('regionLatOrder')


#   Define functions
#   ......................



inputDengue=subset(inputDengue,location != 'Bogota')

absolute.inc=F#T

if(country.choice=="Brazil"){
  these.years=c(2000:2012,2015)
}else{
  these.years=c(2008:2012,2015)
}

for(new.year1 in these.years){
  new.years=c(new.year1:(new.year1+2))
  recent.years=2015:2017
  
  for(temp.loc in unique(inputDengue$location)){
    for(i in 1:3){
      temp.ind=which((inputDengue$year==recent.years[i] & inputDengue$location==temp.loc))
      temp.ind.replace=which((inputDengue$year==new.years[i] & inputDengue$location==temp.loc))
      inputDengue[temp.ind,]$meanIncidence = inputDengue[temp.ind.replace,]$meanIncidence
    }
  }
  
  getCountryIBW <- function(){
    # make separately per each place
    ibw <- by(
      inputDengue
      ,factor(inputDengue$location, levels=place.list)
      ,function(input){
        print(input$location[1])
        input$meanIncidence <- floor(input$meanIncidence)
        
        if(nrow(input)==0){ return(NULL) }
        
        IBW <- t(apply(input, 1, function(x){
          IBW = rep(0,times=26)
          IBW[as.integer(x['biweek'])] <- 1
          IBW
        }))
        colnames(IBW) = paste0('IBW',1:26,'.',input$location[1])
        
        if(absolute.inc==T){
          IBW <- IBW[-1,] * 1
        }else{
          #### use log(meanIncidence) instead of meanIncidence for autoregressive style models
          IBW <- IBW[-1,] * log(input$meanIncidence[-nrow(input)]+1)      
        }
        
        IBW <- data.frame(
          location = input$location[1]
          ,region = attCountry$getRegion(input$location[1])
          ,ct1 = input$meanIncidence[-1]  
          ,ct = log(input$meanIncidence[-nrow(input)]+1)
          ,population = input$population[-1]
          ,year = input$year[-1]
          ,biweek = input$biweek[-1]
        ) %>%
          cbind( as.data.frame(IBW) ) %>%
          left_join(
            inputZika %>%
              filter(location==input$location[1]) %>%
              select(biweek,year,meanIncidence) %>%
              rename(zikaInc = meanIncidence)
          ) %>%
          mutate(zikaInc = sapply(zikaInc, function(x){
            ifelse(is.na(x),0,floor(x))
          })) %>%
          arrange(year,biweek) %>%
          mutate(
            zikaIncCumu = log(cumsum(zikaInc)+1)
            ,zikaInc = log(zikaInc+1)
          )  %>%
          mutate(
            year2015 = as.numeric(year==2015)
            ,year2016 = as.numeric(year==2016)
            ,year2017 = as.numeric(year==2017)
          )
        
        if(!is.null(inputChik)){
          IBW <- IBW %>%
            left_join(
              inputChik %>%
                filter(location==input$location[1]) %>%
                select(biweek,year,meanIncidence) %>%
                rename(chikInc = meanIncidence)
            ) %>%
            mutate(chikInc = sapply(chikInc, function(x){
              ifelse(is.na(x),0,floor(x))
            })) %>%
            arrange(year,biweek) %>%
            mutate(
              chikIncCumu = log(cumsum(chikInc)+1)
              ,chikInc = log(chikInc+1)
            )
          
        }
        
        if(is.null(inputMc)) { return(IBW) }
        inputMc <- inputMc %>%
          filter(location==input$location[1]) %>%
          select(biweekEndDate,biweek,year,meanIncidence) %>%
          rename(mcInc = meanIncidence)
        inputMcLag <- inputMc %>%
          mutate(
            biweekEndDate = as.Date(biweekEndDate) - 14*14    # 14 biweeks lag
            ,biweek = bw$get(biweekEndDate)
            ,year = bw$getYear(biweekEndDate)
          ) %>%
          rename(mcIncLag = mcInc)
        
        x <- IBW %>%
          left_join(
            inputMc %>% select(-biweekEndDate)
          ) %>%
          mutate(mcInc = sapply(mcInc, function(x){
            ifelse(is.na(x),0,floor(x))
          })) %>%
          arrange(year,biweek) %>%
          mutate(mcIncCumu = cumsum(mcInc)) %>%
          
          left_join(
            inputMcLag %>% select(-biweekEndDate)
          ) %>%
          mutate(mcIncLag = sapply(mcIncLag, function(x){
            ifelse(is.na(x),0,floor(x))
          })) %>%
          arrange(year,biweek) %>%
          mutate(mcIncLagCumu = cumsum(mcIncLag)) %>%
          # use log incidence + 1
          mutate(
            mcInc = log(mcInc+1)
            ,mcIncCumu = log(mcIncCumu+1)
            ,mcIncLag = log(mcIncLag+1)
            ,mcIncLagCumu = log(mcIncLagCumu+1)
          )
      }
    )
    
    allColumns <- sapply(ibw, colnames) %>%
      unlist %>%
      unique
    
    out <- do.call(rbind, lapply(ibw, function(ibwLoc){
      if(is.null(ibwLoc)){ return(NULL) }
      ibwLoc[,setdiff(allColumns,colnames(ibwLoc))] <- 0
      ibwLoc
    })) 
  }
  
  ibw <- getCountryIBW()
  
  ibw=subset(ibw,year<2018)
  
  
  if(absolute.inc==T){
    save(ibw,file=paste0(country.choice,'_ibw_absolute_random_recent_',new.years[1],'.Rdata'))
  }else{
    save(ibw,file=paste0(country.choice,'_ibw_random_recent_',new.years[1],'.Rdata'))
  }
}

temp.vars=c("zikaInc","zikaIncCumu","chikInc","chikIncCumu")

for(temp.var in temp.vars){

  # generate hierarchical model formulas
  model1.formula <- as.formula(
    paste(
      "ct1 ~ "
      ,paste(
        grep('IBW',colnames(temp.dat.in), value=T)
        ,collapse = "+"
      )
      ,"+",temp.var,"+(0+",temp.var,"|location) + offset(log(population))" 
    )
  )
  if(country.choice=="Colombia"){
    save(model1.formula,file=paste0(temp.var,"_shared_effect_group_slope_Colombia_formula.Rdata"))
  }else{
    save(model1.formula,file=paste0(temp.var,"_shared_effect_group_slope_formula.Rdata"))
  }
  
  # generate shared effect only model formulas
  model1.formula <- as.formula(
    paste(
      "ct1 ~ "
      ,paste(
        grep('IBW',colnames(temp.dat.in), value=T)
        ,collapse = "+"
      )
      ,"+",temp.var,"+ offset(log(population))" 
    )
  )
  if(country.choice=="Colombia"){
    save(model1.formula,file=paste0(temp.var,"_shared_effect_Colombia_formula.Rdata"))
  }else{
    save(model1.formula,file=paste0(temp.var,"_shared_effect_formula.Rdata"))
  }
  
}

  
