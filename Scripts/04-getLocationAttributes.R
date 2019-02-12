#!/usr/bin/env Rscript --vanilla

library(dplyr)
library(methods)

locMapping <- setRefClass(
    "LocationObject"
    ,fields = list(
        datafile = "character"
        ,data = "data.frame"
        ,color = "list"
    )
    ,methods = list(
        importData = function(){
            data <<- read.csv(datafile, stringsAsFactors=F) %>%
                arrange(regionNum,desc(latitude))
        }
        ,setColor = function(col, pathogen){
            col = list(col)
            names(col) = pathogen
            color <<- c(color, col)
        }
        ,getAttribute = function(locs,at, matcher="location"){
            locs = data.frame(location = locs, stringsAsFactors=F)
            colnames(locs) <- matcher

            # check attribute availability
            if(!(at %in% colnames(data))){
                message(paste('Attribute not available:',at))
                return(NULL)
            }
            
            locs %>%
                left_join(
                    data[,c(at,matcher)] %>% unique
                ) %>%
                select_(at) %>%
                unlist
        }
        ,getRegion = function(locs,...){
            getAttribute(locs,"region",...)
        }
        ,getLatitude = function(locs,...){
            getAttribute(locs,"latitude",...)
        }
        ,getActualName = function(locs,...){
            getAttribute(locs,"actual",...)
        }
        
        ,getRegionFromActual = function(locs){
            getRegion(locs, matcher="actual")
        }
        
        ,getPindexFromLocation = function(locs,...){
            x = unique(locs)
            data.frame(
                location = x
                , pindex = getAttribute(x, "regionLatOrder",...)
            ) %>%
            arrange(pindex) %>%
            mutate(pindex = seq_along(x)) %>%
            right_join(data.frame(location=locs)) %>%
            select(pindex) %>%
            unlist
        }
        
        ,uniqueRegions = function(){ setdiff(unique(data$region),c("")) }
        ,uniqueActuals = function(){ unique(data$actual) }
        ,uniqueActualsNonNA = function(checkCols){
            data[complete.cases(data[,checkCols]),'actual'] %>%
            unique
        }
        
        ,uniqueLocations = function(){ unique(data$location) }
        ,uniqueLocationsNonNA = function(checkCols){
            data[complete.cases(data[,checkCols]),'location'] %>%
            unique
        }

        ,initialize = function(...) {
            callSuper(...)
            .self$importData()
            .self
        }
    )
)


locMapping$new(datafile="02-UnifiedCsvData/Mappings-Colombia-Location.csv") -> attColombia
locMapping$new(datafile="02-UnifiedCsvData/Mappings-Brazil-Location.csv") -> attBrazil


attBrazil$setColor('#660577','dengue')
attBrazil$setColor('#0B7C7C','zika')
attBrazil$setColor('#edffff','zikaLow')
attBrazil$setColor('#d61500','chik')



attColombia$setColor('#660577','dengue')
attColombia$setColor('#0B7C7C','zika')
attColombia$setColor('#edffff','zikaLow')
attColombia$setColor('#d61500','chik')

# example usage:
# attBrazil$getActualName(c('Rondonia','Sao_Paulo','Rondonia'))
# attBrazil$color$dengue

