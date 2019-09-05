#!/usr/bin/env Rscript --vanilla
rm(list=ls())
library(dplyr)
library(ggplot2)
library(ggpubr)

source('Scripts/04-getLocationAttributes.R')

    #   Define functions
    #   ................

# function to import data
importData <- function(country, fileDengue, fileZika, fileChik){

    # function to read in and process the data for particular pathogen, country
    scalingincidence <- function(file,filepathogen,filecountry){
      read.csv(file, stringsAsFactors = F) %>%
        mutate(
            pathogen = filepathogen
            ,location = attCountry$getActualName(location)
            ,country = filecountry
            ,locationpathogen = sapply(location,function(x){ paste(x,filepathogen,sep="_")   })
            ,biweekEndDate = as.Date(biweekEndDate)
        ) %>%
        group_by(location)%>%
        mutate(
            incidence100t = log10( (meanIncidence*100000/population)+1 )
            ,incidence100t = sapply(incidence100t, function(x){ ifelse(x==0,NA,x) })
        )
    } 

    attCountry <- get(paste0('att',country))
    # Order by region-latitude
    state.latsort = attCountry$data %>% 
        filter(!is.na(regionLatOrder)) %>% 
        arrange(desc(regionLatOrder))
    state.latsort = state.latsort$actual

    # import data for all three pathogens for that country
    list(
        dengue.incidence = scalingincidence(
            file = fileDengue
            ,filepathogen = "dengue"
            ,filecountry = country
        )
        ,zika.incidence = scalingincidence(
            file = fileZika
            ,filepathogen = "zika"
            ,filecountry = country
        )
        ,chik.incidence = scalingincidence(
            file = fileChik
            ,filepathogen = "chik"
            ,filecountry = country
        )
        ,state.latsort = state.latsort
        ,attCountry = attCountry
    )
}



    #   Import data
    #   ...........

config <- new.env()

config$Brazil <- importData(
    country = 'Brazil'
    ,fileDengue = "05-simulationSummary/Dengue-Brazil-probable/Dengue-Brazil-probable-tsCountMid/incidence.csv"
    ,fileZika = "05-simulationSummary/Zika-Brazil/Zika-Brazil-tsCountMid/incidence.csv"
    ,fileChik = "05-simulationSummary/Chik-Brazil/Chik-Brazil-tsCountMid/incidence.csv"
)

config$Colombia <- importData(
    country = 'Colombia'
    ,fileDengue = "05-simulationSummary/Dengue-Colombia-Total/Dengue-Colombia-Total-tsCountMid/incidence.csv"
    ,fileZika = "05-simulationSummary/Zika-Colombia/Zika-Colombia-tsCountMid/incidence.csv"
    ,fileChik = "05-simulationSummary/Chik-Colombia/Chik-Colombia-tsCountMid/incidence.csv"
)



    #   Figure 1.
    #   ........................................

#   Function to generate plots and store in list
#   (heatmap, time-series)
generatePlotList <- function(incidenceCol
    , attCountry
    , dengue.incidence
    , zika.incidence
    , chik.incidence
    , state.latsort
    , xlimits = as.Date(c('1999-01-01','2018-01-01'))
    , plotSlices=13, legendExp=1){
    # define internal functions
    getLegendBreaks <- function(maxInc,le=legendExp){
        if(le==1){ return( seq(0,maxInc,by=.2) ) }
        0:ceiling(maxInc)
    }
    getLegendLabels <- function(maxInc,le=legendExp){
        br <- getLegendBreaks(maxInc)
        if(le==1){ return(br) }
        brexp <- le^br
    }

    # prepare dataframes required for the plots
    all.incidence <- rbind(dengue.incidence,zika.incidence,chik.incidence) %>%
        mutate_("incidence" = incidenceCol) %>%
        ungroup %>%
        mutate(
            biweekEndDate = as.Date(biweekEndDate)
            ,location = as.integer(factor(location, levels=state.latsort))
        )
    dengue.incidence <- all.incidence %>%
        filter( pathogen == 'dengue' )
    all.recent <- all.incidence %>% 
        filter(as.integer(format(biweekEndDate,"%Y")) >= 2014)
    dengue.recent <- all.recent %>% 
        filter( pathogen == 'dengue' )

    # function to generate dengue heatmap
    plotInc <- function(dengue.incidence){
        ggplot(
            dengue.incidence
            ,aes(
                x = biweekEndDate
                ,y = as.integer(location)
                ,fill = incidence
            )) + 
            geom_tile() +
        theme_bw()+
        scale_fill_continuous(
            name="Dengue"
            , low="#FFFFFF"
            , high=attCountry$color$dengue
            , na.value = "#FFFFFF"
            , limits=c(0, maxDengueInc)
            , breaks = getLegendBreaks(max(dengue.incidence[,incidenceCol],na.rm=T))
            , label = getLegendLabels(max(dengue.incidence[,incidenceCol],na.rm=T))
        )+
        scale_y_continuous(
            'Location'
            ,position = 'right'
            ,breaks = data.frame(            
                i = seq_along(state.latsort)
                ,region = attCountry$getRegionFromActual(state.latsort)
                ) %>%
                group_by(region) %>%
                summarize(imax = max(i)+.5) %>%
                select(imax) %>%
                unlist
            ,labels = paste0('',unique(attCountry$getRegionFromActual(state.latsort)))
            ,minor_breaks = (1:length(state.latsort))+.5
            ,expand = c(0,0)
        )+
        scale_x_date(
            ''
            ,position = "top"
            ,limits = xlimits
            ,expand = c(0,0)
        )+
        theme(plot.title = element_text(size = 20, colour = "gray50")
            ,axis.text.y=element_text(vjust=1, hjust=1)
            ,axis.text.x=element_text(size=14, angle=30, hjust=0)
            ,panel.background = element_blank()
            ,panel.grid.major.y = element_line( size=.5, color="black" )
            ,strip.text = element_text(size=20)
            ,legend.key.height = unit(1,"line")
            ,plot.margin = unit(c(-1,0,0,0), "lines")
        )
    }
    
    # function to generate incidence time-series plot
    plotTs <- function(all.incidence){
        df <- all.incidence %>%
            group_by(pathogen,biweekEndDate) %>%
            summarize(incidence = sum(incidence,na.rm=T))
        df[df$pathogen=='dengue','incidence'] <- df[df$pathogen=='dengue','incidence']

        ggplot( df
            ,aes(
                x = biweekEndDate
                ,y = incidence
                ,col = pathogen
                ,group = pathogen
            )) +
            geom_line()+
            theme_bw()+
            theme(
                axis.title.x=element_blank()
                ,axis.text.x=element_blank()
                ,plot.margin = unit(c(-4,0,0,0), "lines")
            )+
            scale_x_date(
                ,limits = xlimits
                ,expand = c(0,0)
            )+
            scale_y_continuous(
                'Incidence per\n100,000 population'
                ,position = "right"
            )+
            scale_color_manual(
                'Pathogen'
                ,values = c(attCountry$color$chik,attCountry$color$dengue,attCountry$color$zika)
                ,breaks = c('chik','dengue','zika')
                ,labels = c('Chikungunya','Dengue','Zika')
            )+
            guides(col = guide_legend(override.aes = list(size = 2)))
    }

    # return list of plots
    list(
        ts = plotTs(all.incidence)
        ,heatmap = plotInc(dengue.incidence)
    )
}


# Specify countries and incidence column to use
countries = c('Brazil','Colombia')
incidenceCol = 'incidence100t'

# Identify maximum incidence in the plot to be used for y-axis scaling in time-series plot
# (scale such that both countries are on the same scale)
maxDengueInc = max(sapply(countries, function(ct){
    max(config[[ct]]$dengue.incidence[[incidenceCol]], na.rm=T)
}))

# Generate plots for each country
plots <- lapply(countries, function(country){
    do.call(generatePlotList, c(
        list(incidenceCol = incidenceCol, legendExp=10)
        ,config[[country]]
    ))
})
names(plots) = countries

# Combine the plots into Figure 1 and save
g <- ggarrange( 
    plotlist = list(
        plots$Brazil$ts
        ,plots$Brazil$heatmap
        ,plots$Colombia$ts
        ,plots$Colombia$heatmap
    )
    ,ncol = 1, nrow = 4, align = "hv"
    ,widths = 4
    ,heights = c(3,3,3,3)
    ,legend = "right"
    ,common.legend = F
)
ggsave(
    g
    , filename=paste0('heatmap/Dengue-',incidenceCol,'.pdf')
    , width = 6
    , height = 7
)    


  
    #   SI Figure 1.
    #   Explore consistency of peak biweeks across years  
    #   ................................................

library(tidyr)

for (country in c('Brazil','Colombia'){

    attach(config[[country]])

    # Season cut-off in Colombia is unclear. Year-round transmission.
     # Make Colombia same as Brazil to ease comparison.
    seasonSplit = c('Brazil'=18, 'Colombia'=18)
    seasonSplit = seasonSplit[country]

    x <- dengue.incidence %>%
        # shift biweek>18 to the next season
        mutate(
            year = mapply(function(b,y){ ifelse(b>seasonSplit,y+1,y) }, b=biweek, y=year)
            ,biweek = sapply(biweek, function(b){ ifelse(b>seasonSplit,b-26,b) })
        ) %>%
        group_by(location, year) %>%
        # filter for top 3 biweeks for each season
        filter(meanIncidence >= meanIncidence[order(meanIncidence,decreasing=T)][3]) %>%
        ungroup %>%
        mutate(
            region = factor(attCountry$getRegionFromActual(location), levels=attCountry$uniqueRegions())
            ,location = factor(location, levels=attCountry$uniqueActualsNonNA('regionLatOrder'))
        ) %>%
        filter(!is.na(location))

    # generate colors to be used in based on these
    # RGB color anchor points (corresponding to each region)
    colorAnchors = list(
        c(229, 87, 48)   # red
        ,c(186, 211, 21)  # yellow-green
        ,c(24, 242, 140)  # frost green
        ,c(24, 111, 242)  # blue
        ,c(216, 6, 174)   # purple
        ,c(122, 51, 80)   # brownish purple
    )
    colors <- unlist(mapply(function(rgbColor,locs,coljitter=30){
            if(length(locs)!=1){
                n = ceiling((length(locs)-1)/2)
                n = (-n:n)[1:length(locs)]
                step = coljitter/max(n)
            } else {
                n = 1
                step = 0
            }
            n = sapply(n*step, function(j){
                j = (rgbColor + j)/255
                j = ifelse(j<0,0,j)
                j = ifelse(j>1,1,j)
                rgb(j[1],j[2],j[3])
            })
            names(n) <- locs
            n
        }
        ,rgbColor = colorAnchors[1:length(attCountry$uniqueRegions())]
        ,locs = by(x, x$region, function(df){ unique(df$location) })
    ))

    # plot and save
    g <- ggplot(
        x %>%
            group_by(region,location,biweek) %>%
            summarize(ntop3 = n())
        ,aes(x = biweek, y = ntop3))+
        geom_line(aes(col=location, group=location))+
        scale_color_manual('', values = colors)+
        facet_grid(region~., scale="free_y")+
        theme_bw()+
        theme(
            panel.grid = element_blank()
            #,panel.spacing = unit(1, "lines")
        )+
        guides(col=guide_legend(
            ncol=1
            ,keyheight=0.8
        ))+
        scale_x_continuous('Biweek', expand = c(0, 0)) +
        scale_y_continuous('Number of years in the top 3', expand = c(0, 0.5))+
        geom_line(data = x %>%
            group_by(region,location,biweek) %>%
            summarize(ntop3 = n()) %>%
            spread(biweek, ntop3, fill=0) %>%
            gather(biweek,ntop3,-region,-location) %>%
            mutate(biweek = as.integer(biweek)) %>%
            group_by(region,biweek) %>%
            summarize(ntop3 = mean(ntop3, na.rm=T))
            ,aes(group=region)
            ,col = 'black'
            ,size = 2
        )
 
    ggsave(
        g
        , filename=paste0('heatmap/',country,'-biweekTop3-lines.pdf')
        , width = 6
        , height = 8
    )

    detach(config[[country]])
}
