#!/usr/bin/env Rscript --vanilla
rm(list=ls())

library(dplyr)
library(EpiWeek)
library(stringi)

# for testing
indir = "01-TabulatedData/Microcephaly-Brazil-Microcephaly"
outdir = "02-UnifiedCsvData/Microcephaly-Brazil-Microcephaly"

args = commandArgs(trailing=TRUE)
indir = args[1]
outdir = args[2]

country = "Brazil"
source("Scripts/04-getLocationAttributes.R")
attCountry <- get(paste0('att',country))

# import data
infiles = list.files(indir, full.names=T)

# clean data
data <- do.call(rbind, mapply(
	function(infile, nskip, placecol, totalcol, discardcol, confirmcol, pendingcol, year){
		df <- read.csv(
			infile
			, skip = nskip
			, header = FALSE
			, stringsAsFactors = FALSE
		)
		epidates <- epiweekToDate(
			year
			,as.integer(gsub('[^0-9]','',strsplit(basename(infile),"_")[[1]][2])) # epiweek
		)

		# Clean location
		# change Latin characters into the equivalent ASCII characters
		loc <- df[[placecol]]
		loc <- gsub('[\\\"]','',loc)
		Encoding(loc) <- "latin1"
		loc <- stri_trans_general(loc, "latin-ascii")

        extractValue <- function(col){
            if(is.na(col)){ return(NA) }
            as.numeric(gsub('\\.','',df[,col]))        
        }


		df <- data.frame(
		    pathogen = "Microcephaly"
		    ,country = country
		    ,location = gsub('[^a-zA-Z_]','',gsub(' ','_',loc))
			,dateStart = as.Date(paste0(year,"-01-01"))
			,dateEnd = as.Date(epidates$d1)
            ,totals = extractValue(totalcol)
            ,discards = extractValue(discardcol)
            ,pendings = extractValue(pendingcol)
            ,confirms = extractValue(confirmcol)
		) %>%
		mutate(countCumu = totals - discards)
		
		df$countCumu <- with(df, ifelse(is.na(countCumu),totals,countCumu))
		
		df$sevCountCumu = with(df, paste0(
	        "total=",totals
	        ,";discard=",discards
	        ,";pending=",pendings
	        ,";confirm=",confirms
	    ))
	    df	            
	}
	, infile = infiles
	, nskip = 3
	, placecol = 1
	, totalcol = c(6,2,5,5,5,5,2,2,2,rep(2,length(infiles)-10),2)
    , discardcol = c(NA,6,4,4,4,4,5,5,5,rep(6,length(infiles)-10),7)
    , confirmcol = c(NA,4,3,3,3,3,4,4,4,rep(5,length(infiles)-10),5)
    , pendingcol = c(NA,3,2,2,2,2,3,3,3,rep(4,length(infiles)-10),4)
	, year = 2016
	, SIMPLIFY = FALSE
))

rownames(data) <- NULL
# fix location name capitalization problem
locs = unique(data$location[data$location!=toupper(data$location)])
data$location <- sapply(data$location, function(x){
    locs[toupper(x)==toupper(locs)]
})



# export data
dir.create(outdir, recursive=T, showWarnings=F)
write.csv(
	data %>%
    mutate(
        source = "Microcephaly"
        ,locationType = "state"
        ,notes = ""
    ) %>%
    select(pathogen, country, location, locationType, dateStart, dateEnd, countCumu, sevCountCumu, source, notes)
	, file=paste0(outdir,'/Microcephaly-Brazil-Microcephaly.csv')
	, row.names = FALSE
)


q()	# quit R




    #   Microcephaly data exploration
    #   ..................................
    
library(ggplot2)
library(tidyr)

ggplot(
    data %>%
        select(location,dateEnd,totals,discards,pendings,confirms) %>%
        mutate(vals = totals - discards) %>%
        gather(type,count,-location,-dateEnd) %>%
        filter(type != "totals") %>%
        mutate(location = factor(location, levels=attCountry$uniqueLocations()))
    , aes(
        x = dateEnd
        , y = count
        , col = type
        , group = type
	))+
	geom_line()+
	facet_wrap(~location, scale="free_y")+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	xlab('Last date of epiweek')

ggsave(
    '03-DataExplore-Brazil/Microcephaly-ReportedNumbers.png'
    ,width=12
    ,height=9
)


    #   explore ranking locations by each of the reported numbers

# totals
rank.na.rm <- function(x){
    out <- rank(x)
    out[is.na(x)] <- NA
    out
}

ggplot(
    data %>%
        group_by(dateEnd) %>%
        mutate(
            rankTotal = rank.na.rm(totals)
            ,rankPending = rank.na.rm(pendings)
            ,rankConfirm = rank.na.rm(confirms)
            ,rankVal = rank.na.rm(totals - discards)
        ) %>%
        ungroup %>%
        select(location, dateEnd, rankTotal, rankPending, rankConfirm, rankVal) %>%
        gather(metric,rank,-location,-dateEnd) %>%
        mutate(location = factor(location, levels=attCountry$uniqueLocations()))
    ,aes(x = dateEnd, y = rank, col=metric, group=metric))+
    geom_line()+
    facet_wrap(~location)+
	theme(axis.text.x = element_text(angle = 90, hjust = 1))+
	xlab('Last date of epiweek')

ggsave(
    '03-DataExplore-Brazil/Microcephaly-StateRankedByReportedNumbers.png'
    ,width=12
    ,height=9
)


