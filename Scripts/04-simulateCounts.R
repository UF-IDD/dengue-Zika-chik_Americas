#!/usr/bin/env Rscript --vanilla
# rm(list=ls())

library(dplyr)
library(tidyr)
library(parallel)


# parse arguments
args = commandArgs(trailingOnly=TRUE)
inputdir = args[1]
outdir = args[2]
numsim = as.integer(args[3])
if(length(args)>3){
    correctionType = args[4:length(args)]
} else {
    correctionType = c(
    	"tsCountUpper"
    	,"tsCountMid"
    	,"tsCountLower"
    )
}


# test
#inputdir = "03-TimeSeries/Dengue-Brazil-probable"
#outdir = "04-TsSimulations/Dengue-Brazil-probable"
#numsim = 1000
#inputdir="03-TimeSeries/Zika-Brazil"
#outdir="04-TsSimulations/Zika-Brazil"
#numsim=1000
#inputdir="03-TimeSeries/Dengue-Brazil-severealarm"
#outdir="04-TsSimulations/Dengue-Brazil-severealarm"
#numsim=1000
#
#inputdir="03-TimeSeries/Microcephaly-Brazil"
#outdir="04-TsSimulations/Microcephaly-Brazil"
#numsim=1000
#inputdir="03-TimeSeries/Chik-Colombia"
#outdir="04-TsSimulations/Chik-Colombia"
#numsim=1000




# set environment
source("Scripts/04-generateBiweeks.R")


# import data
infiles = grep( 
	pattern = paste(
		# remove places that aren't places
		"ignor"
		,"exterior"
		# remove extra Colombia provinces
		,"Barranquilla"
		,"Buenaventura"
		,"Cartagena"
		,"Desconocido"
		,"Exterior"
		,"Procedencia_Desconocida"
		,"Santa_Marta"
		, sep = "|"
	)
	, x = list.files(inputdir, pattern="*.csv", full.names=T)
	, ignore.case = T
	, value = T
	, invert = T
)


# read in data
myfiles = lapply(infiles, read.csv)
names(myfiles) <- gsub('.+/|\\.csv$','',infiles)

# function to run the simulation
# based on the timeseries chosen
simTs <- function(place,countcol){

  f = myfiles[[place]]
  
  # check if data is sufficient to run spline
  if(nrow(f)<4){ return(TRUE) }
  
  
  # setup environment
  set.seed(9292)
  date.origin = "1970-01-01"    # temporary store the 1st dateEnd
  outdir = paste0(outdir,'/',basename(outdir),'-',countcol)
  dir.create(outdir, recursive=T, showWarnings=F)

  # fit the spline
  y.test <- data.frame(
      dates=as.numeric( as.Date(f$dateEnd), origin = date.origin)
      ,counts=f[[countcol]]
  )
  s.test <- smooth.spline(y.test,all.knots=T)

  # get biweek dates to predict for
  tsRange <- range(as.Date(f$dateEnd))
  new.dates <- as.numeric(bw$seq[
    min(which(bw$seq >= tsRange[1])-1) :
    max(which(bw$seq <= tsRange[2]))
    ]
    , origin = date.origin
  )

  # predict the counts from the spline
  predicted.counts=predict(s.test, new.dates)$y

  # calculate count differences
  count.diff <- diff(predicted.counts)
  count.diff[count.diff < 0] <- 0

  # do 1000 simulations
  
  	# creat clusters to do multicore processing
	ncores <- detectCores()-1
  	cl <- makeCluster(ncores)
  	clusterEvalQ(cl, library(dplyr))
  	
	# split up the work for each core
	seeds <- runif(numsim)* numsim
	chunk <- seq_along(seeds) %% (ncores)
	chunk <- lapply(unique(chunk), function(x){ seeds[chunk==x] })
	  
  result <- do.call(cbind, parLapply(
  	cl = cl
  	, X = chunk 
  	, function(seeds, count.diff, y.test){
		do.call(cbind, lapply(seeds, function(i){
		   set.seed = i
		   a <-data.frame(
			bin = sample(
				seq_along(count.diff)
				, size=max(y.test$counts)
				, replace=T
				, prob=count.diff
				)
			) %>%
			group_by(bin) %>%
			summarize(count = n() )
		   a <- data.frame(bin = seq_along(count.diff)) %>%
			merge(a, all.x=T) %>%
			arrange(bin) %>%
			select(count)
		   a[is.na(a)] <- 0
		   a
		})) # end lapply and internal cbind
	}
	, count.diff = count.diff
	, y.test = y.test
  )) # end parLapply and external do.call


	# stop the multicoring cluster
	stopCluster(cl)

  colnames(result) <- paste0('sim',1:ncol(result))
  result$biweekDateEnd <- as.Date(new.dates, origin = date.origin)[-1]


  write.csv(
    result
    , file=paste0(outdir,'/',place,'.csv')
    , row.names = F
  )
}


# do the simulation for each of the places and export to CSV
lapply(
    correctionType
    , function(tscol){
        lapply(names(myfiles), simTs, countcol=tscol)
    }
)


