## Calculate annual incidence values and other data metrics

rm(list=ls())
library(lubridate)
library(dplyr)
library(tidyr)

source('Scripts/04-getLocationAttributes.R')
outdir = '06-getDescriptives'
dir.create(outdir, recursive=T)

fun <- new.env()

fun$getPath = function(pathogen,country,dataset){
    dname = paste(c(pathogen,country,dataset), collapse='-')
    paste0('05-simulationSummary/',dname,'/',dname,'-tsCountMid/incidence.csv')
}

fun$getData = function(pathogen,country,dataset){
    attCountry <- get(paste0('att',country))
    read.csv(
        fun$getPath(pathogen,country,dataset)
        ,stringsAsFactors = F
    ) %>%
    mutate(
        biweekEndDate = as.Date(biweekEndDate)
        ,region = attCountry$getRegion(location)
        ,location = attCountry$getActualName(location)
        ,region = factor(region, levels=attCountry$data$region %>% unique)
        ,location = factor(location, levels=attCountry$data$actual %>% unique)
    )
}

fun$getFirstCount = function(df){
    df %>%
        filter( meanIncidence > 0 ) %>%
        group_by(region,location) %>%
        summarize( biweekMidDate1 = min(biweekEndDate)-7 ) %>%
        mutate(
            year = epiyear(biweekMidDate1)
            ,epiweek = epiweek(biweekMidDate1)
        ) %>%
        arrange(biweekMidDate1) %>%
        select(location, year, epiweek) %>%
        as.data.frame
}

fun$getAnnualIncHt = function(df, grouper=c('region','location','year')){
    df %>%
        group_by_(.dots=grouper) %>%
        summarize(
            inc = sum(meanIncidence)
            ,population = sum(unique(population))
        ) %>%
        mutate( annualIncRateHt = inc*100000/population )
}

fun$getAvgAnnualIncHt = function(df, grouper=c('region','location')){
    fun$getAnnualIncHt(df) %>%
        group_by_(.dots=grouper) %>%
        summarize(
            annualIncRateHt_025 = round(quantile(annualIncRateHt,.025,na.rm=T),2)
            ,annualIncRateHt_975 = round(quantile(annualIncRateHt,.975,na.rm=T),2)
            ,annualIncRateHt_median = round(median(annualIncRateHt,na.rm=T),2)
        ) %>%
        #arrange( annualIncRateHt_median ) %>%
        as.data.frame
}



# 1st counts (epiweek, year)

    # Brazil
        # zika
        fun$getData('Zika','Brazil',NULL) %>%
            fun$getFirstCount()

        # microcephaly
        fun$getData('Microcephaly','Brazil',NULL) %>%
            fun$getFirstCount()
    
        # chik
        fun$getData('Chik','Brazil',NULL) %>%
            fun$getFirstCount()

        
    # Colombia
        # zika
        fun$getData('Zika','Colombia',NULL) %>%
            fun$getFirstCount()
    
        # chik
        fun$getData('Chik','Colombia',NULL) %>%
            fun$getFirstCount()


# Dengue
# Average annual incidence rate per 100,000, per each location
lapply(c('Brazil','Colombia'), function(country){
    ver = c('Brazil'='probable', 'Colombia'='Total')
    # Brazil
    write.csv(
        fun$getData('Dengue',country,ver[[country]]) %>%
            fun$getAvgAnnualIncHt() %>%
            select(region,location,annualIncRateHt_median) %>%
            left_join(
                fun$getData('Dengue',country,ver[[country]]) %>%
                fun$getAnnualIncHt() %>%
                mutate(value=paste0(round(annualIncRateHt,2),'\n(',population,')')) %>%
                select(region,location,year,value) %>%
                spread(year,value)
            ) %>%
        rbind(
        fun$getData('Dengue',country,ver[[country]]) %>%
            fun$getAvgAnnualIncHt(grouper=NULL) %>%
            mutate(region=country,location='') %>%
            select(region,location,annualIncRateHt_median) %>%
            left_join(
                fun$getData('Dengue',country,ver[[country]]) %>%
                fun$getAnnualIncHt(grouper='year') %>%
                mutate(region=country,location='') %>%
                mutate(value=paste0(round(annualIncRateHt,2),'\n(',population,')')) %>%
                select(region,location,year,value) %>%
                spread(year,value)
            )
        )
        ,file = paste0(outdir,'/Dengue-',country,'-incSupplement-full.csv')
        ,fileEncoding = 'utf8'
    )
})


# Dengue
# incidence per 100,000 greater than median
    # Brazil
    fun$getData('Dengue','Brazil','probable') %>%
        filter(year %in% 2014:2017) %>%
        fun$getAnnualIncHt() %>%
        arrange(annualIncRateHt) %>%
        left_join(
            fun$getData('Dengue','Brazil','probable') %>%
                fun$getAvgAnnualIncHt()
            ,by = c('location','region')
            ,suffix = c('','.avg')
        ) %>%
        filter( annualIncRateHt < annualIncRateHt_median ) %>%
        arrange( year, region, annualIncRateHt ) %>%
        as.data.frame

    write.csv(
        fun$getData('Dengue','Brazil','probable') %>%
            fun$getAvgAnnualIncHt()
        ,file = paste0(outdir,"/Dengue-Brazil-probable_avgAnnualIncHt.csv")
    )
    
    # Colombia
    fun$getData('Dengue','Colombia','Total') %>%
        filter(year %in% 2014:2017) %>%
        fun$getAnnualIncHt() %>%
        arrange(annualIncRateHt) %>%
        left_join(
            fun$getData('Dengue','Colombia','Total') %>%
                fun$getAvgAnnualIncHt()
            ,by = c('location','region')
            ,suffix = c('','.avg')
        ) %>%
        filter( annualIncRateHt > annualIncRateHt.avg ) %>%
        arrange( year, region, annualIncRateHt ) %>%
        as.data.frame

    write.csv(
        fun$getData('Dengue','Colombia','Total') %>%
            fun$getAvgAnnualIncHt()
        ,file = paste0(outdir,"/Dengue-Colombia-Total_avgAnnualIncHt.csv")
    )
    
    
# Zika, 2016
# Average annual incidence rate per 100,000, per each location
    # Brazil
    fun$getData('Zika','Brazil',NULL) %>%
        filter(year %in% 2016:2017) %>%
        fun$getAnnualIncHt() %>%
        arrange( year, annualIncRateHt ) %>%
        as.data.frame
    
    # Colombia
    fun$getData('Zika','Colombia',NULL) %>%
        filter(year %in% 2016:2017) %>%
        fun$getAnnualIncHt() %>%
        arrange( year, annualIncRateHt ) %>%
        as.data.frame

# Zika
# Annual incidence 2016,2017
    # Brazil
    fun$getData('Zika','Brazil',NULL) %>%
        filter(year %in% 2016:2017) %>%
        group_by(year) %>%
        summarize( incidence = sum(meanIncidence))

    # Colombia
    fun$getData('Zika','Colombia',NULL) %>%
        filter(year %in% 2016:2017) %>%
        group_by(year) %>%
        summarize( incidence = sum(meanIncidence))
    