#!/bin/bash

  # This script sweaves the whole analysis from start-finish
  # (Not automation quality..some parts may contain remarks for manual tasks)
  # ::: REMARKS :::
  
  # Syntax here are written for terminal on MacOS.
  # Modifications are needed for Linux.


        #####################################################
        #                                                   #
        #   Section 1: Gathering and processing raw data    #
        #                                                   #
        #####################################################

mkdir -p 00-RawData
mkdir -p 01-TabulatedData

    ###############
    #   Brazil    #
    ###############
    
# Location name mappings

    # Mapping of accent-removed names and actual names
    # as well as abbreviations
    # saved in: 01-TabulatedData/Mappings-Brazil-Location.csv


# Dengue
 
    # Dataset archive (1986-2006)
    # reported by the National Health Foundation - FUNASA. The system was centralized into the
    # Sistema de Informação de Agravos de Notificação (SINAN) in 2001.
    # These counts represent the total number of dengue cases (including classical dengue, complicated
    # dengue, haemorrhagic fever and toxic shock syndrome). Incomplete notification forms that do not
    # specify dengue type are also included in the totals. The month represents the month of initial
    # sypmtoms. November 2006 is a partial result and data for 1991 was indicated as defective).
    # saved in 01-TabulatedData/Dengue-Brazil-DatasetArchiveTotal/Archive1986-2006_totals.csv

 
    # Batch retrieve tabnet severity data (2001-2012)
    # : Needs Selenium on python and Chromedriver installed.
    # : "fig43" and "fig45" correspond to field ids of severity (Grau FHD dropdown)
    # : on the respective websites.
    # Tabnet data (2001-2006)
    python Scripts/00-retrieveTabnetSeverity.py \
        "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinanwin/cnv/denguebr.def" \
        "00-RawData/Dengue-Brazil-TabnetSeverity" \
        "fig43"
    # Tabnet data (2007-2012)
    python Scripts/00-retrieveTabnetSeverity.py \
        "http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/denguebr.def" \
        "00-RawData/Dengue-Brazil-TabnetSeverity" \
        "fig45"
    mkdir 00-RawData/Dengue-Brazil-TabnetTotal
    mv 00-RawData/Dengue-Brazil-TabnetSeverity/total*.csv \
        00-RawData/Dengue-Brazil-TabnetTotal


    # EpiBulletin previous year data (2013 Epiweek 14 to 2017 Epiweek 42) from
    # Boletim Epidemiológico of 2014 Epiweek 14 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Dengue-Brazil-EpiBulletinPrevYear
    # File names refers to time in which the numbers corresponds to,
    # which is one year prior the time in which the numbers were reported.

    
    # EpiBulletin original reports (2014 Epiweek 14 to 2018 Epiweek 42) from
    # Boletim Epidemiológico of 2014 Epiweek 14 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Dengue-Brazil-EpiBulletin    


# Zika

    # EpiBulletin previous year data (2016 Epiweek 4 to 2017 Epiweek 42) from
    # Boletim Epidemiológico of 2017 Epiweek 4 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Zika-Brazil-EpiBulletinPrevYear
    # File names refers to time in which the numbers corresponds to,
    # which is one year prior the time in which the numbers were reported.

    
    # EpiBulletin original reports (2016 Epiweek 13 to 2018 Epiweek 42) from
    # Boletim Epidemiológico of 2016 Epiweek 13 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Zika-Brazil-EpiBulletin    


# Chikungunya

    # EpiBulletin previous year data (2015 Epiweek 9 to 2017 Epiweek 42) from
    # Boletim Epidemiológico of 2016 Epiweek 9 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Chik-Brazil-EpiBulletinPrevYear
    # File names refers to time in which the numbers corresponds to,
    # which is one year prior the time in which the numbers were reported.

    
    # EpiBulletin original reports (2016 Epiweek 9 to 2018 Epiweek 42) from
    # Boletim Epidemiológico of 2016 Epiweek 9 to 2018 Epiweek 42
    # reported by Secretaria de Vigilância em Saúde, Ministério da Saúde.
    # saved in: 01-TabulatedData/Chik-Brazil-EpiBulletin    


# Microcephaly

    # Monitoramento dos casos de microcefalias no Brasil
    # CENTRO DE OPERAÇÕES DE EMERGÊNCIAS EM SAÚDE PÚBLICA SOBRE MICROCEFALIA
    # reported by Ministério da Saúde Brasil.
    # http://portalms.saude.gov.br/saude-de-a-z/microcefalia/informes-epidemiologicos
    # Data (2016) most recently downloaded October 17, 2017
    # saved in: 01-TabulatedData/Microcephaly-Brazil-Microcephaly


# Population size

    # Data source: Instituto Brasileiro de Geografia e Estatística
    
    # Data (2001-2018) most recently downloaded on December 21, 2018
    # from https://sidra.ibge.gov.br/tabela/6579
    # saved in: 00-RawData/Population-size/Brazil_pop_2001_2018.xlsx

    # Data (1986-2000) downloaded on December 1, 2006
    # saved in: 00-RawData/Population-size/Brazil_pop_1986_2000.csv



    #################
    #   Colombia    #
    #################

# Location name mappings

    # Mapping of accent-removed names and actual names
    # as well as abbreviations
    # saved in: 01-TabulatedData/Mappings-Colombia-Location.csv

    
# Dengue

    # Ministry data (2007-2017)
    # downloaded December 24, 2018
    # saved in: 00-RawData/Colombia-Ministry2007_2017


# Zika

    # EpiBulletin (2015 Epiweek 45 - 2017 Epiweek 52) from
    # Boletín Epidemiológico reported by
    # Instituto Nacional de Salud Colombia
    # and El Ministerio de Salud y Protección Social.
    # saved in: 01-TabulatedData/Zika-Colombia-EpiBulletin


# Chikungunya

    # EpiBulletin (2014 Epiweek 48 - 2017 Epiweek 52) from
    # Boletín Epidemiológico reported by
    # Instituto Nacional de Salud Colombia
    # and El Ministerio de Salud y Protección Social.
    # saved in: 01-TabulatedData/Chik-Colombia-EpiBulletin


# Population size

    # Sistema Estadístoco Nacional Colombia
    # Departamento Administrativo Nacional de Estadística
    # Downloaded from
    # https://www.dane.gov.co/files/investigaciones/poblacion/proyepobla06_20/Municipal_area_1985-2020.xls
    # saved in: 00-RawData/Population-size/Colombia_population_1985-2020_raw.xls
    
    


        #########################################################
        #                                                       #
        #   Section 2: Transform datasets into unified format   #
        #                                                       #
        #########################################################

# Process the data to obtain unified CSV files for downstream analyses
mkdir -p 02-UnifiedCsvData
  
    # function for processing EpiBulletins
    unifyBulletins(){
        # parameter1 : directory name of tabulated data
        # parameter2 : date (to force as start date)
        for infile in $(ls 01-TabulatedData/${1}/* | grep -vi 'template' ) ; do
            echo "Processing:" ${infile} "-------------------------"
            outfile=02-UnifiedCsvData${infile#01-TabulatedData}
            #year=$(echo $(basename ${infile}) | cut -d'-' -f1 | cut -d'_' -f3 )
            Rscript --vanilla Scripts/02-ProcessTab-EpiBulletin.R \
                ${infile} \
                ${outfile} \
                ${2}
        done
    }
    
        
    ###############
    #   Brazil    #
    ###############

# Geographical attributes mapping

    # Mapping of geographical attributes to unified location names
    # used earlier in mapping files of the earlier phase
    # saved in: 02-UnifiedCsvData/Mappings-Brazil-Location.csv
    

# Dengue

    # Dengue-Brazil-DatasetArchiveTotal
    # : process and truncate to 2000
    Rscript --vanilla Scripts/02-ProcessTab-DatasetArchiveBrazilTotals.R \
        01-TabulatedData/Dengue-Brazil-DatasetArchiveTotal/Archive1986-2006_totals.csv \
        02-UnifiedCsvData/Dengue-Brazil-DatasetArchiveTotal/Archive1986-2000_totals.csv \
        "01-TabulatedData/Mappings-Brazil-Location.csv"
 
    # Dengue-Brazil-TabnetTotal
    Rscript --vanilla Scripts/02-ProcessTab-TabnetSeverity.R \
        '00-RawData/Dengue-Brazil-TabnetTotal' \
        '02-UnifiedCsvData/Dengue-Brazil-TabnetTotal'

    # Dengue-EpiBulletin
    unifyBulletins Dengue-Brazil-EpiBulletin
    unifyBulletins Dengue-Brazil-EpiBulletinPrevYear


# Zika

    # Zika-EpiBulletin
    unifyBulletins Zika-Brazil-EpiBulletin
    unifyBulletins Zika-Brazil-EpiBulletinPrevYear


# Chikungunya

    # Chik-EpiBulletin
    unifyBulletins Chik-Brazil-EpiBulletin
    unifyBulletins Chik-Brazil-EpiBulletinPrevYear


# Microcephaly

    # Microcephaly-Brazil (2016) data
    Rscript --vanilla Scripts/00-Convert2Tab-BrazilMicrocephaly.R \
        "01-TabulatedData/Microcephaly-Brazil-Microcephaly" \
        "02-UnifiedCsvData/Microcephaly-Brazil-Microcephaly"


    #################
    #   Colombia    #
    #################

# Geographical attributes mapping

    # Mapping of geographical attributes to unified location names
    # used earlier in mapping files of the earlier phase
    # saved in: 02-UnifiedCsvData/Mappings-Colombia-Location.csv


# Dengue

    # Denuge-Colombia-Ministry
    for f in 00-RawData/Colombia-Ministry2007_2017/* ; do
        year=${f##*_}
        year=${year%%.*}
        Rscript --vanilla Scripts/02-ProcessTab-ColombiaMinistryRawXlsx.R \
            ${f} \
            02-UnifiedCsvData/Dengue-Colombia-Ministry2007_2017/${year}.csv \
            "dengue" \
            ${year}
    done


# Zika

    # Zika-EpiBulletin
    # Zika cases in Colombia were first reported in the
    # 2015 Epiweek 32 bulletin (starts on August 9, 2015).
    # We incorporate that information here.
    for infile in $(ls 01-TabulatedData/Zika-Colombia-EpiBulletin/* | grep -vi 'template' ) ; do
        outfile=02-UnifiedCsvData${infile#01-TabulatedData}
        year=$(echo $(basename ${infile}) | cut -d'-' -f1 | cut -d'_' -f3 )
        if [[ ${year} -lt 2017 ]]; then
            startDate="2015-08-09"
        else
            startDate=""
        fi
        Rscript --vanilla Scripts/02-ProcessTab-EpiBulletin.R \
            ${infile} \
            ${outfile} \
            ${startDate}
    done
    

# Chikungunya

    # Chik-EpiBulletin
    unifyBulletins Chik-Colombia-EpiBulletin
    
    


        ###########################################################
        #                                                         #
        #   Section 3: Combine into time-series and fit splines   #
        #                                                         #
        ###########################################################

# Fortmat each of the datasets as time-series by
# converting the numbers to cumulative cases throughout the time period
mkdir 03-TimeSeries

        
    ###############
    #   Brazil    #
    ###############


    # Function to combine the use of previous year and
    # current year data. Previous year data is prioritized.
    # Current year data is used when previous year data is
    # not available.
    lumpPrevCurrentYearCsv()
    {
        csvdir=${1}"PrevYear"
        firstPrev=$(ls ${csvdir} | head -n 1 )
        firstPrev=${firstPrev##*_}
        lastPrev=$(ls ${csvdir} | tail -n 1 )
        lastPrev=${lastPrev##*_}

        lumpdir=${1}-lumped
        mkdir -p ${lumpdir}
        ln ${csvdir}/*.csv ${lumpdir}

        csvdir=${1}
        for f in ${csvdir}/*.csv ; do
            fbase=${f##*_}
            if [[ ${fbase} > ${lastPrev} ]]; then
                ln ${f} ${lumpdir}
            elif [[ ${fbase} < ${firstPrev} ]]; then
                ln ${f} ${lumpdir}
            else
                echo 'omitted:' ${f}
            fi
        done
    }


# Dengue

    # Dengue-Brazil-probable: total counts (DataArchive + Tabnet + EpiBulletin)
    # : Previous Year EpiBulletins
    lumpPrevCurrentYearCsv \
        02-UnifiedCsvData/Dengue-Brazil-EpiBulletin
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Dengue-Brazil-probable "probable_countCumu" \
        "02-UnifiedCsvData/Dengue-Brazil-DatasetArchiveTotal" \
        "02-UnifiedCsvData/Dengue-Brazil-TabnetTotal" \
        "02-UnifiedCsvData/Dengue-Brazil-EpiBulletin-lumped"
    rm -rf "02-UnifiedCsvData/Dengue-Brazil-EpiBulletin-lumped"


# Zika

    # Zika-Brazil
    lumpPrevCurrentYearCsv \
        02-UnifiedCsvData/Zika-Brazil-EpiBulletin
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Zika-Brazil "countCumu" \
        02-UnifiedCsvData/Zika-Brazil-EpiBulletin-lumped
    rm -rf "02-UnifiedCsvData/Zika-Brazil-EpiBulletin-lumped"


# Chikungunya

    # Chik-Brazil
    lumpPrevCurrentYearCsv \
        02-UnifiedCsvData/Chik-Brazil-EpiBulletin
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Chik-Brazil "countCumu" \
        02-UnifiedCsvData/Chik-Brazil-EpiBulletin-lumped
    rm -rf "02-UnifiedCsvData/Chik-Brazil-EpiBulletin-lumped"


# Microcephaly

    # Microcephaly-Brazil: 2016
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Microcephaly-Brazil "countCumu" \
        "02-UnifiedCsvData/Microcephaly-Brazil-Microcephaly"


    #################
    #   Colombia    #
    #################

# Dengue

    # Dengue-Colombia-Total
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Dengue-Colombia-Total "countCumu" \
        "02-UnifiedCsvData/Dengue-Colombia-Ministry2007_2017"


# Zika

    # Zika-Colombia
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Zika-Colombia "countCumu" \
        02-UnifiedCsvData/Zika-Colombia-EpiBulletin


# Chikungunya

    # Chik-Colombia
    Rscript --vanilla Scripts/03-makeTimeSeries.R \
        03-TimeSeries/Chik-Colombia "countCumu" \
        02-UnifiedCsvData/Chik-Colombia-EpiBulletin
    
    


        ###########################################################
        #                                                         #
        #   Section 4: Simulate counts from the fitted splines    #
        #                                                         #
        ###########################################################

    # Function to generate 1000 simulations of counts for each subnational-level location
    run1000sim()
    {
        Rscript --vanilla Scripts/04-simulateCounts.R \
            03-TimeSeries/${1} \
            04-TsSimulations/${1} \
            1000 \
            "tsCountMid"
    }

    # Function to summarize the simulations
    summarizeSimulations()
    {
        echo 01-TabulatedData/Population-size/$( echo ${1} | cut -d'-' -f2 )*.csv

        for tsdir in 04-TsSimulations/${1}/*tsCount* ; do
            Rscript --vanilla Scripts/05-summarizeSimulation.R \
                ${tsdir} \
                05-simulationSummary/${tsdir#04-TsSimulations} \
                01-TabulatedData/Population-size/$( echo ${1} | cut -d'-' -f2 )*.csv
        done
    }

     
    ###############
    #   Brazil    #
    ###############

# Dengue
    run1000sim Dengue-Brazil-probable
    summarizeSimulations Dengue-Brazil-probable

# Zika

    run1000sim Zika-Brazil
    summarizeSimulations Zika-Brazil

# Chikungunya

    run1000sim Chik-Brazil
    summarizeSimulations Chik-Brazil

# Microcephaly

    run1000sim Microcephaly-Brazil
    summarizeSimulations Microcephaly-Brazil


    #################
    #   Colombia    #
    #################
    
# Dengue    

    run1000sim Dengue-Colombia-Total
    summarizeSimulations Dengue-Colombia-Total

# Zika
    
    run1000sim Zika-Colombia
    summarizeSimulations Zika-Colombia

# Chikungunya
    
    run1000sim Chik-Colombia
    summarizeSimulations Chik-Colombia
