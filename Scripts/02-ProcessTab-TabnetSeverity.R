#!/usr/bin/env Rscript

# load libraries
library(plyr)
library(stringi)
library(dplyr)
library(tidyr)


args = commandArgs(trailingOnly=TRUE)
indir = args[1]
outdir = args[2]
ignoreSev = args[-(1:2)]

#Just here for testing purposes (commented out)
#indir = '01-TabulatedData/Dengue-Brazil-TabnetSeverity'
#outdir = '02-UnifiedCsvData/Dengue-Brazil-TabnetSeverity'
#ignoreSev = c('discard')

#indir = '00-RawData/Dengue-Brazil-TabnetSeverity'
#outdir = '01-TabulatedData/Dengue-Brazil-TabnetSeverity'
#ignoreSev = ''


# just in case the output directory is still not there
dir.create(outdir, showWarnings=FALSE, recursive=TRUE)

# import all the severity data per each year
infiles = list.files(indir, full.names=T)
meta <- strsplit(basename(dirname(infiles)),"-")[[1]]


lapply(
	# per each year
	unique(gsub('^.+_|\\..+$','',basename(infiles)))
	, function(y){
		# import each of the input files and join them
		df <- lapply(
			grep(
				paste0('/',ignoreSev,'_')
				, grep(paste0(y,'\\..+$'),infiles, value=T)
				, value=T, invert=T)
			,function(infile){

				# parse severity level
				sev = gsub('_.+$','',basename(infile))


				# strip them by their columns
				input = strsplit(readLines(infile, encoding="UTF-8"), ';')

				# change Latin characters into the equivalent ASCII characters
				input = lapply( input, function(x){
				  x = gsub('[\\\"]','',x)
				  Encoding(x) <- "UTF-8"
				  stri_trans_general(x, "latin-ascii")
				})

				# create the initial data.frame
				input = do.call(rbind, input)				
				input = input[ grepl('[a-zA-Z0-9]',input[,1]), ]	# filter out rows without data
				df = sapply( as.data.frame(input[-1,-1], stringsAsFactors=F), as.integer)
				df[is.na(df)] <- 0
				df = cbind(data.frame(
					gsub("^[0-9]{1,2} ", "", input[-1,1])
				  ),df)

				if(ncol(input)<3){
					# skip this file because there is no data to use
					return(NULL)
				}

				# change Portugese months to English months
				month.translate <- c(
					"Jan" = "Jan"
					,"Fev" = "Feb"
					,"Mar" = "Mar"
					,"Abr" = "Apr"
					,"Mai" = "May"
					,"Jun" = "Jun"
					,"Jul" = "Jul"
					,"Ago" = "Aug"
					,"Set" = "Sep"
					,"Out" = "Oct"
					,"Nov" = "Nov"
					,"Dez" = "Dec"
				)
				
				# remove non-month columns (except 1st and last column)
				includeCols <- c(
					TRUE
					, input[1,c(-1,-ncol(input))] %in% names(month.translate) 
					, TRUE
				)
				df <- df[ , includeCols]
				
				colnames(df) = c(
					'state'
					,month.translate[ input[1,includeCols][-c(1,ncol(df))] ]
					,'total'
				)
	
				# check if sum of months matches the total
				# : throws warning if not match
				if (any(rowSums(df[,2:(ncol(df)-1)]) != df[,ncol(df)])) {
				  warning(paste(
					"Row totals doesn't match provided total:"
					, infile
				  ))
				}

                # insert zeros for months omitted from the dataset (due to zero cases)
                df[,setdiff(month.translate, colnames(df))] <- 0
                df <- df[,c('state',month.translate,'total')]

				month.num <- function(x){
				  which(x == month.abb)
				}

				# compute the fields
				df = df[-nrow(df),setdiff(colnames(df),'total')]
				df = df %>%
				  gather(month_abb,count,-state) %>%
				  mutate(
					year = as.integer(y)
					, dateStart = as.Date(paste(year,month_abb,1,sep="-"), "%Y-%b-%d")	# first day of month
					, dateEnd = as.Date(sapply(dateStart, function(x) {
						as.character(seq(x,by="month",length=2)[2]-1)
					  }))
					, dateStart = as.Date(paste(year,1,1,sep="-"), "%Y-%m-%d")	# first day of year: overwrites the previous dateStart value
				  ) %>%
				  group_by(state) %>%
				  arrange(state,dateStart) %>%
				  mutate(
					countCumu =  cumsum(count)
				  )
				
				print(infile)
				colnames(df)[colnames(df)=="count"] <- paste0(sev,'_count')
				colnames(df)[colnames(df)=="countCumu"] <- paste0(sev,'_Cumu')
				df %>% ungroup
			})
			
		df <- join_all( df[sapply(df,length)>0] )
	
		# make into unified format
		collapseCols <- function(df,colsuffix){
			cols = grep(colnames(df) ,pattern=colsuffix, value=T)
			apply( df[,cols] ,1 ,function(vals){
				paste(paste(
					gsub('_.+$','',cols)
					, vals
					, sep="="
				), collapse=";")
			})
		}
		sumCols <- function(df,colsuffix){
			cols = grep(colnames(df) ,pattern=colsuffix, value=T)
			apply( df[,cols] ,1 ,function(vals){
				sum(vals,na.rm=T)
			})

		}

		write.csv(
			df %>% mutate(
				  count = sumCols(df,"_count")
				  , countCumu = sumCols(df,"_Cumu")
				  , sevCount = collapseCols(df,"_count")
				  , sevCountCumu = collapseCols(df,"_Cumu")
				  , pathogen = meta[1]
				  , country = meta[2]
				  , source = meta[3]
				  , notes = ""
				  , location = state
				  , locationType = "state"
				) %>%
				ungroup() %>%
				select(pathogen, country, location, locationType, dateStart, dateEnd, count, countCumu, sevCount, sevCountCumu, source, notes)
		  , paste0(outdir,'/',paste(c(meta,y),collapse='-'),'.csv')
		  , row.names = FALSE
		)
	}
)





