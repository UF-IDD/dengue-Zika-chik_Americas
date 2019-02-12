#!/usr/bin/env Rscript

# load libraries
require(dplyr)
source('Scripts/functions.R')

# take only and only one input file
args = commandArgs(trailingOnly=TRUE)
infile = args[1]
outfile = args[2]
forceStart = ifelse(length(args)>2, args[3], NA)	# set all 'dateStart' to this value


#Just here for testing purposes (commented out)
#infile = '01-TabulatedData/Dengue-Columbia-EpiBulletin/Epidemiological_Bulletin_2017-Week-30.csv'
#outfile = '02-UnifiedCsvData/Dengue-Columbia-EpiBulletin/Epidemiological_Bulletin_2017-Week-30.csv'
#infile = '01-TabulatedData/Dengue-Brazil-EpiBulletin/Epidemiological_Bulletin_2016-Week-49.csv'
#outfile = '02-UnifiedCsvData/Dengue-Brazil-EpiBulletin/Epidemiological_Bulletin_2016-Week-49.csv'
#infile = '01-TabulatedData/Chik-Colombia-EpiBulletin/Epidemiological_Bulletin_2016-Week-37.csv'
#outfile = '02-UnifiedCsvData/Chik-Colombia-EpiBulletin/Epidemiological_Bulletin_2016-Week-37.csv'



# read in the files (omitting the comments)
input <- read.csv(infile, stringsAsFactors=FALSE, comment.char="*") %>%
    filter(location != "")

# determine number of severity levels
sevCols <- ls(input, pattern="value")
sevLevels <- gsub("value|_","",sevCols)
sevLevels <- ifelse(sevLevels=="","moderate",sevLevels)
if(length(sevLevels)==1){ sevLevels = "total" }

input <- input %>%
  mutate(
  	# assuming all "value" are distinct sets of people
    countCumu = apply(input[sevCols],1,function(x){ sum(as.integer(x),na.rm=T) })
    ,sevCountCumu = mapply(function(val,lev){
          paste0(lev,'=',val)
        }
        , val = input[sevCols]
        , lev = sevLevels
      ) %>%
      apply(1,paste,collapse=";")
    , location = gsub(".+-","",location)
    , locationType = location_type
  )


# convert to "unified format" and export to CSV
meta <- strsplit(basename(dirname(infile)),"-")[[1]]
epidates <- epiweekToDate(
  as.integer(gsub("^.+_|-.+$", "", basename(infile))) # year
  , as.integer(gsub(".+-|\\..+", "", basename(infile))) # epiweek
)
yearStart <- epiweekToDate(
  as.integer(gsub("^.+_|-.+$", "", basename(infile))) # year
  ,1
)

  # just in case the output directory is still not there
  dir.create(dirname(outfile), showWarnings=FALSE, recursive=TRUE)

write.csv(
  input %>%
    mutate(
      sevCount = as.character(NA)
      , count = as.integer(NA)
      , pathogen = meta[1]
      , country = meta[2]
      , source = meta[3]
      , notes = ""
      , dateStart = as.Date(ifelse(is.na(forceStart), as.character(yearStart$d0), forceStart))
      , dateEnd = as.Date(epidates$d1)
    ) %>%
    select(pathogen, country, location, locationType, dateStart, dateEnd, count, countCumu, sevCount, sevCountCumu, source, notes)
  , outfile
  , row.names = FALSE
)


