## Generate Figure 4 
## Compartmental model simulation results
## Necessary summary metrics and simulation inter-quartile ranges
## are calculated in "processSimulations.R". Sample input for 
## producing this figure is provided in the sims_output folder.

setwd("~/dengue-Zika-chik_Americas")
source("Scripts/04-generateBiweeks.R")
source("Scripts/04-getLocationAttributes.R")

## load peak and trough data for both cases
load("06-compartmentalModels/sims_output/data_ZIKV100.Rdata")
load("06-compartmentalModels/sims_output/data_ZIKV40.Rdata")

original.sims=F#T
attCountry=attBrazil

if(original.sims == T){
  intro.ZIKV.t = 100
  prev.summary.vals = prev.summary.vals.ZIKV100
  trough.cutoff = trough.cutoff.ZIKV100
}else{
  intro.ZIKV.t = 40
  prev.summary.vals = prev.summary.vals.ZIKV40
  trough.cutoff = trough.cutoff.ZIKV40
}

record.times=seq(intro.ZIKV.t,intro.ZIKV.t+10,by=0.01)

## arrange scenarios so that:
## rows correspond to DENV-ZIKV cross-protection scenarios (none, 20% hazard reduction, 80% hazard reduction)
## columns correspond to enhancement scenarios (no enhancement, DENV enhances DENV, DENV enhances DENV and ZIKV, enhancement in all directions)
these.scenarios=c(9:12,13:16,5:8)
par(lwd=1.5,cex.axis=1.7,cex.lab=1.7,cex.main=1.5)
panel.ncol=4
panel.nrow=3
par(mfrow=c(panel.nrow, panel.ncol)
    ,oma=c(7,7,1,1)+0.1
    ,mar=c(1,1,1.5,1)+0.1)

panel.labels=c("a","b","c","d"
               ,"e","f","g","h"
               ,"i","j","k","l")

panel.labels.alt=c("A)","B)","C)","D)"
                   ,"E)","F)","G)","H)"
                   ,"I)","J)","K)","L)")

attCountry=attBrazil

newx=record.times

panel.index=1
for(j in these.scenarios){ #1:length(prev.summary.vals)
  temp=as.data.frame(prev.summary.vals[[j]])
  
  legX <- (j %in% 5:8) #(length(these.scenarios) -panel.ncol)< j
  legY <- (j %in% c(9,13,5)) #((j %% panel.ncol)) == 1
  
  plot(newx,temp$median,ylim=c(0,0.0032),type='l',lwd=2,col=attCountry$color$dengue #temp$mean #ylim=c(0,0.003)
       ,xaxs="i",yaxs="i",yaxt='n',xaxt='n',las=1#,axes=F
       ,ylab="",xlab="")
  polygon(c(rev(newx), newx), c(rev(temp$`97.5%`), temp$`2.5%`), col = "#D3B7D8", border = NA) #"#BD93C4"
  #  lines(newx,temp$mean)
  lines(newx,temp$median,type='l',lwd=2,col=attCountry$color$dengue) #temp$mean
  abline(h=trough.cutoff,lty=2) #trough cutoff 
  text(1,0.0027,labels=panel.labels[panel.index],cex=2) 
  print(panel.index)
  #  Axis(side=1, labels = legX)
  if(legX==T){
    Axis(side=1,at=seq(min(record.times),max(record.times),length=6),labels = seq(0,10,length=6))
  }else{
    Axis(side=1, labels = legX)
  }
  if(legY==T){
    Axis(side=2, at=c(0,0.001,0.002,0.003),labels=c(0,100,200,300),las=1)#, labels = legY
    #  Axis(side=2, at=c(0,0.001,0.002,0.003),las=1)
  }
  legend("topleft",legend=panel.labels[panel.index],pch=NULL,bty='n',cex=2)
  panel.index=panel.index+1
  
}
mtext("Dengue prevalence (per 100,000 population)", SOUTH<-2, line=4.5, cex=1.5, col="black", outer=T)
mtext("Time since ZIKV introduction (years)", SOUTH<-1, line=4, cex=1.5, col="black", outer=T)

# export as 10" by 8" 
# add line to export plot: file = "compartmental_model_sims_output/simFig_ZIKVXXX.pdf"
# Adobe Illustrator artboard size 850 pt by 650 pt (need to add column headings, row headings, move and bold panel letters)

