# dengueZikaData

This repository includes raw and processed dengue, Zika, and chikungunya case data for Brazil (1999-2017) and Colombia (2000-2017). The original data sources are outlined below. Further detail on the original data sources is included in the coreScript.sh file. This file also contains the code used to collate these data into incidence time series. 

If you find the time series useful, please cite this repository along with the original data sources.

[![DOI](https://zenodo.org/badge/160224999.svg)](https://zenodo.org/badge/latestdoi/160224999)

# Data Sources:

## Brazil

**Dengue**
*  Fundação Nacional de Saúde (FUNASA), Ministério da Saúde, Brasil
    * Archival dataset including case counts for 1986-2001. After 2001 the data was centralized into the National Health Notification System below. These counts represent the total number of dengue cases (including classical dengue, complicated dengue, hemorrhagic fever and toxic shock syndrome).  
* Sistema de Informação de Agravos de Notificação (SINAN), Ministry of Health, Brazil.
    * Passive surveillance system data that includes dengue severity classes for 2001-2012.
    * Data for 2001-2006 downloaded from: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinanwin/cnv/denguebr.def
    * Data from 2007-2012 downloaded from: http://tabnet.datasus.gov.br/cgi/deftohtm.exe?sinannet/cnv/denguebr.def
* Secretaria de Vigilância em Saúde, Ministério da Saúde, Brasil
    * Cumulative probable and severe dengue case counts for 2013-2018 were entered from epidemiological bulletins reported weekly by the Direction of Epidemiological surveillance on the Brazilian Ministry of Health website.
    * Data for 2014 Epiweek 14 to 2018 Epiweek 42 downloaded from: http://portalms.saude.gov.br/boletins-epidemiologicos

**Zika** 
* Secretaria de Vigilância em Saúde, Ministério da Saúde, Brasil
    * Cumulative probable case counts were entered from weekly epidemiological bulletins reported by the Direction of Epidemiological surveillance on the Brazilian Ministry of Health website.
    * Data for 2016 Epiweek 4 to 2018 Epiweek 42 downloaded from: http://portalms.saude.gov.br/boletins-epidemiologicos

**Chikungunya**
* Secretaria de Vigilância em Saúde, Ministério da Saúde, Brasil
    * Cumulative probable case counts were entered from weekly epidemiological bulletins by the Direction of Epidemiological surveillance on the Brazilian Ministry of Health website.
    * Data for 2015 Epiweek 9 to 2018 Epiweek 42 downloaded from: http://portalms.saude.gov.br/boletins-epidemiologicos


**Microcephaly**
* Centro de operações de emergência de saúde pública em microcefalia, Ministério da Saúde, Brasil
  * Cumulative cases of microcephaly and other central nervous system disorders in newborns reported in weekly epidemiological reports from the Brazilian Ministry of Health website.
  * Data for 2016 downloaded from: http://portalms.saude.gov.br/saude-de-a-z/microcefalia/informes-epidemiologicos

**Population size** 
* Instituto Brasileiro de Geografia e Estatística
    * Yearly state-level population size estimates
    * Data from 2001-2018 downloaded from: https://sidra.ibge.gov.br/tabela/6579
    * Data from 1986-2001 from archival dataset. 


## Colombia

**Dengue**
* Instituto Nacional de Salud, Colombia
    * Weekly routine surveillance case counts reported by the Colombian National Institute of Health.
    * Data for 2007-2017 downloaded from: http://portalsivigila.ins.gov.co/sivigila/documentos/Docs_1.php?

**Zika**
* Instituto Nacional de Salud, Colombia
    * Cumulative case counts were entered from weekly epideimological bulletins reported by the Colombian National Institute of Health.
    * Data for 2015 Epiweek 43 to 2018 Epiweek 52 downloaded from: https://www.ins.gov.co/buscador-eventos/Paginas/Vista-Boletin-Epidemilogico.aspx

**Chikungunya**
* Instituto Nacional de Salud, Colombia
    * Cumulative case counts were entered from weekly epideimological bulletins reported by the Colombian National Institute of Health.
    * Data for 2015 Epiweek 43 to 2018 Epiweek 52 downloaded from: https://www.ins.gov.co/buscador-eventos/Paginas/Vista-Boletin-Epidemilogico.aspx

**Population size**
* Data source: Departamento administrativo Nacional de Estadistica (DANE)
    * Yearly department-level population estimates 
    * Data for 2007-2018 downloaded from: https://www.dane.gov.co/files/investigaciones/poblacion/proyepobla06_20/Municipal_area_1985-2020.xls


