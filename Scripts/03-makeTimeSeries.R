#!/usr/bin/env Rscript --vanilla


require(ggplot2)
require(dplyr)
require(tidyr)
source('Scripts/functions.R')

# parse arguments
args = commandArgs(trailingOnly=TRUE)
outCsvDir = args[1]		# output directory
countMode = args[2]		# one of the count modes below:
	# total		: combines all severities including suspicious/probable
	# suspect	: only use suspicious/probable counts
	# confirm	: exclude suspicious/probable counts
	# severe	: only include severe manifestations (severe,alarm,death)
inpath = args[-(1:2)]		# takes the rest as directory paths to input CSV files


#for testing
#inpath = "02-UnifiedCsvData/Dengue-Brazil-EpiBulletin"
#countMode = "probable"
#outCsvDir = paste0(inpath,"Total")

#makeTS Dengue-Brazil-EpiBulletin "severe_severealarm" "SevereAll"





# import data from 'inpath'
input <- lapply( inpath, function(indir){

	do.call(rbind, lapply(list.files(indir, pattern=".csv$", full.names=T)
	, function(f){
		read.csv(f, stringsAsFactors=F)
	}))
})



# compute the ultimate cumulative count column (based on the chosen mode)
breakValuesAndSum <- function(x, delim=";", cols=countMode ){
	if( is.na(x)|x=="" ){ return(NA) }
	cols = strsplit(cols,"_")[[1]]
	values = as.numeric(
		sapply(strsplit(x, delim)[[1]], function(a){
			key = gsub('=.+$','',a)
			value = gsub('^.+=','',a)
			if( key %in% cols){
				return(value)
			} else {
				return(NA)
			}
		}) #[[1]]
	)
	if(all(is.na(values))){ return(NA) }
	sum(values,na.rm=T)
}
input <- do.call(rbind, lapply(input, function(x){
	if('sevCountCumu' %in% colnames(x)){
		x$tsCount = sapply(x$sevCountCumu, breakValuesAndSum)
	} else {
		x$tsCount <- x[,countMode]
	}

	if( all(is.na(x$tsCount)) ){
	    message('Using "countCumu" as count column')
		x$tsCount <- x$countCumu
	}
	x
}))

# reduce the number of columns to reduce computational burden
pathogen = input$pathogen
country = input$country
#locationType = input$locationType
#source = input$source

input = input %>%
	select(location, dateStart, dateEnd, tsCount) %>%
	mutate(
		# standardize the location names
		location = gsub('[^a-zA-Z]','_',location)
		, dateStart = as.Date(dateStart)
		, dateEnd = as.Date(dateEnd)
	)
gc()


# make cumulatives monotonic
# : each of the cumulatives are dealt with separately
input <- do.call(rbind, by(input, input[,c('location','dateStart')], function(x){

	forceMin <- function(a) {
		while(TRUE){
			increment = c(a[-1] - a[-length(a)],0)
			i <- which(increment < 0 )
			if(length(i)==0){
				break
			}
			a[i] <- a[i+1]
		}
		a
	}

	forceMax <- function(a){
		increment = c(a[-1] - a[-length(a)],0)
		i <- which(increment < 0 )
		for (j in i){
			a[1:j] = a[1:j] + increment[j]
		}
		a[a<0] = 0
		a
	}

	x %>% arrange(dateEnd) %>%
		mutate(
			tsCountUpper = forceMin(tsCount)
			,tsCountLower = forceMax(tsCount)
			,tsCountMid = (tsCountUpper+tsCountLower)/2
		)
}))


# Iteratively fix the 'tsCount' such that all dateStart==min(dateStart)
fixed <- do.call(rbind, by(input, input$location, function(x) {

	x <- x %>%
		filter( !is.na(tsCount) ) %>%
		mutate( rowi = seq_along(location) )
	tsStart = min(x$dateStart)
	countCols = ls(x, pattern="^tsCount")

	while( any(x$dateStart != tsStart) ){
		# ones we consider fixing: does not start from 'tsStart' but is very close
		needFix <- x %>%
			filter( dateStart != tsStart ) %>%
			filter( dateStart == min(dateStart) )

		# a bunch of possible cumulative counts that starts from the 'tsStart'
		b <- x %>%
			filter( dateStart == tsStart) %>%
    		# becasue all needFix$dateStart are equal,
    		# find the closest base
			filter( dateEnd <= needFix$dateStart[1] ) %>%
			filter( dateEnd == max(dateEnd) )

		# do the fixing
		x[needFix$rowi,countCols] <- do.call(rbind, lapply(1:nrow(needFix), function(i){
			mapply( function(q,r){
				q + r
				}
				, q = needFix[i,countCols]
				, r = b[,countCols]
			)
		}))

		x[needFix$rowi,"dateStart"] <- tsStart
	}
	x %>% select(-rowi)
}))

rm(input); gc()


# Quick check for outliers: indicating data entry errors
plotTimeSeries <- function(df, plotdir){
	dir.create(plotdir, recursive=T, showWarnings=F)
	lapply( unique(df$location), function(loc){

		ggplot(
			data = df %>%
				filter(location==loc) %>%
				gather(adjustment,counts,-location,-dateStart,-dateEnd)
			, aes(
				x=dateEnd
				, y=counts
				#,col= format(dateEnd,"%Y")
				, col=adjustment
				, group=adjustment
			)) +

			geom_line(size=1, alpha=.6)

			#geom_text(aes(label=sapply(dateEnd, function(a) {dateToEpiweek(a)$weekno})), size=2)

		ggsave(paste0(plotdir,'/',gsub('/','_',loc),'.jpg'))
	})
}
plotTimeSeries(fixed,paste0(outCsvDir,'_plots'))





# Export the the time-series: one CSV per location
dir.create(outCsvDir, showWarnings=F, recursive=T)
by(fixed,fixed$location, function(x){
	write.csv(x, paste0(outCsvDir,'/',x$location[1],'-',countMode,'.csv'), row.names=F)
})
