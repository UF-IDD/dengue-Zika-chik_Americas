#!/usr/bin/env Rscript --vanilla

rm(list=ls())
library(dplyr)
library(tidyr)

# import dependencies
source("Scripts/04-generateBiweeks.R")


# parse arguments
args = commandArgs(trailingOnly=TRUE)
indir = args[1]
outdir = args[2]
popsizeCsv = args[3]


#for testing
#indir = "04-TsSimulations/Dengue-Colombia-MinistryTotal/Dengue-Colombia-MinistryTotal-tsCountMid"
#outdir = "05-simulationSummary/Dengue-Colombia-MinistryTotal/Dengue-Colombia-MinistryTotal-tsCountMid"
#popsizeCsv = "01-TabulatedData/Population-size/Colombia_population_1985-2020.csv"


	# import inputs

# import population size
popsize <- read.csv(popsizeCsv, stringsAsFactors=F)
colnames(popsize)[1] <- "location"
popsize <- popsize %>%
  gather(year,population, -location) %>%
  mutate(
    year = as.integer(gsub("[a-zA-Z]","",year))
  )

# read in the simulations
myfiles = lapply(
	temp <- list.files(indir,pattern="*.csv", full.names=T)
	, function(x){
	  x <- read.csv(x, header=T, stringsAsFactors=F)
	  list(
		dates = as.Date(x$biweekDateEnd)
		,mat = as.matrix(x[,-ncol(x)])
	  )
	}
)
names(myfiles) <- gsub('-.+$|\\..+$','',basename(temp))



# generate slope summary df
dir.create(outdir, recursive=T, showWarnings = F)
write.csv(
  do.call(rbind, lapply(names(myfiles), function(loc){
    x = myfiles[[loc]]
    if(is.null(x)){ return(NULL) }
    
    slopes = x$mat[-1,] / x$mat[-nrow(x$mat),]
    slopes[is.nan(slopes)] <- NA
    slopes[slopes==Inf] <- NA
    
    data.frame(
      biweekEndDate = x$dates[-1]
      , meanSlope = rowMeans(slopes, na.rm=T)
      , varSlope = apply(slopes,1,var, na.rm=T)
    ) %>%
    mutate(
      biweek = bw$get(biweekEndDate)
      , year = bw$getYear(biweekEndDate)
      , location = loc
    )
  }))
  , file = paste0(outdir,"/slope.csv")
  , row.names = F
)
  

# generate incidence summary df
write.csv(
  do.call(rbind, lapply(names(myfiles), function(loc){
    x = myfiles[[loc]]
    if(is.null(x)){ return(NULL) }

    x <- data.frame(
      biweekEndDate = x$dates
      , meanIncidence = rowMeans(x$mat)
      , varIncidence = apply(x$mat,1,var)
    ) %>%
      mutate(
        biweek = bw$get(biweekEndDate)
        , year = bw$getYear(biweekEndDate)
        , location = loc
      ) %>%
      merge(
        popsize
        , all.x = T
        , all.y = F
      ) %>%
      arrange(location,biweekEndDate)
    if( any(is.na(x$meanIncidence)) | any(is.na(x$varIncidence)) ){
      stop('NA present in the incidence matrix. CHECK!')
    }
    x
  }))
  , file = paste0(outdir,"/incidence.csv")
  , row.names = F
)




