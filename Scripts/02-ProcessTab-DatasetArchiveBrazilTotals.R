#!/usr/bin/env Rscript

# load libraries
require(dplyr)
require(tidyr)

# take only and only one input file
args = commandArgs(trailingOnly=TRUE)
infile = args[1]
outfile = args[2]
mapping = args[3]

#for testing purpose only
#infile="01-TabulatedData/Dengue-Brazil-DatasetArchiveTotal/EpiBulletin1986-2006_totals.csv"
#outfile = "02-UnifiedCsvData/Dengue-Brazil-DatasetArchiveTotal/EpiBulletin1986-2006_totals.csv"
#mapping = "01-TabulatedData/Mappings-Brazil-Location.csv"

# import input
input = read.csv(infile, stringsAsFactors=F) %>%
    gather(location,count,-Dengue.cases)
mapping = read.csv(mapping, stringsAsFactors=F)


# format the input into Unified format
# "pathogen","country","location","locationType","dateStart","dateEnd","count","countCumu","sevCount","sevCountCumu","source","notes"

mapping$full <- sapply(strsplit(mapping$full,"-"), function(x){ x[2] })

output <- data.frame(
          dateStart = sapply(input$Dengue.cases, function(x){
                paste(c(1,substring(x,1,3),substring(x,4,7)), collapse="-")
            })
      ) %>%
    mutate(
        dateStart = as.Date(dateStart, format="%d-%b-%Y")
        ,dateEnd = sapply(dateStart, function(x){
            as.character(seq(x,by="month",length.out=2)[2]-1)
        })
        ,dateEnd = as.Date(dateEnd)
        ,count = input$count
        ,sevCount = NA
        ,countCumu = count
        ,sevCountCumu = NA
        ,location = sapply(input$location, function(x){
            mapping$full[mapping$abb==substring(x,1,2)]
            })
    )

metadata = strsplit(basename(dirname(infile)), "-")[[1]]

output <- output %>%
    mutate(
        pathogen = metadata[1]
        ,country = metadata[2]
        ,source = metadata[3]
        ,notes = NA
        ,locationType = "state"
    ) %>%
    select(
        pathogen
        ,country
        ,location
        ,locationType
        ,dateStart
        ,dateEnd
        ,count
        ,countCumu
        ,sevCount
        ,sevCountCumu
        ,source
        ,notes
    )


output <- output %>%
    # remove entries after Oct2006 (unstable data at the time)
    filter( dateStart < as.Date("2006-10-31") ) %>%
    # remove entries after Dec2000 (because we already have that data in subsequent datasets)
    filter( dateStart < as.Date("2001-1-1") ) %>%
    # remove leading zeros for each province
    group_by(location) %>%
    arrange(dateStart) %>%
    filter( cumsum(count)>0 )

dir.create(dirname(outfile), recursive=T, showWarnings=F)
write.csv(output, file=outfile, row.names=F)
