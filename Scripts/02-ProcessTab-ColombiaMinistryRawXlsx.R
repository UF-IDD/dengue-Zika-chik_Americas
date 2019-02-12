#!/usr/bin/env Rscript --vanilla

rm(list=ls())


args = commandArgs(trailingOnly=T)
infile = args[1]
outfile = args[2]
pathogen = args[3]
year = as.integer(args[4])

# infile = "00-RawData/Colombia-Ministry2007_2017/rutinaria_2007.xlsx"; outfile = "02-UnifiedCsvData/Dengue-Colombia-Ministry2007_2017/2007.csv"; pathogen='dengue'; year = 2007

library(readxl)
library(dplyr)
library(tidyr)

mapping = "01-TabulatedData/Mappings-Colombia-Location.csv"
mapping = read.csv(mapping, stringsAsFactors=F)
source('Scripts/04-getLocationAttributes.R')
source('Scripts/functions.R')




input = read_excel(infile
    ,sheet = length( excel_sheets( infile ) )
)
colnames(input) = tolower(colnames(input))


input = input %>%
    select_(.dots = c(
        'nombre'
        ,'departamento'
        ,'semana'
        ,colnames(input)[ncol(input)]
    )) %>%
    filter(grepl(pathogen,tolower(nombre))) %>%
    select(-nombre)
colnames(input) = c(
    'location'
    ,'epiweek'
    ,'count'
)

output = input %>%
    group_by(location,epiweek) %>%
    summarize(count = sum(count, na.rm=T)) %>%
    ungroup %>%
    mutate(location = tolower(location)) %>%
    left_join(
        mapping %>%
        mutate(location = tolower(Location)) %>%
        select(-Location)
        ,by = 'location'
    ) %>%
    select(-location) %>%
    rename(location = UnifiedLocation)

output$location = attColombia$getAttribute(
    gsub('^Colombia-','',output$location)
    ,"locGroup"
)

output = output %>%
    group_by(location,epiweek) %>%
    summarize(count = sum(count,na.rm=T)) %>%
    ungroup %>%
    filter(location!="") %>%

    # make non-filled entries zero
    spread(epiweek,count,fill=0) %>%
    gather(epiweek,count,-location) %>%
    
    mutate(
        pathogen = paste0(
            toupper(substring(pathogen,1,1))
            ,tolower(substring(pathogen,2))
        )
        ,country = 'Colombia'
        ,epiweek = as.integer(epiweek)
        ,dateStart = do.call(c, lapply(epiweek, function(x){ as.Date(epiweekToDate(year,x)$d0) }))
        ,dateEnd = do.call(c, lapply(epiweek, function(x){ as.Date(epiweekToDate(year,x)$d1) }))
        ,countCumu = count
        ,source = 'C-NIH'
    ) %>%
    select(pathogen, country, location, dateStart, dateEnd, count, countCumu, source)


dir.create(dirname(outfile), recursive=T)
write.csv(
    output
    ,file = outfile
    ,row.names = F
)