## Uses the indexing functions in indexing_functions.R to find the compartment indeces 
## for particular categories of indiviudals (e.g. individuals currently infectious with
## DENV or ZIKV)
# [index] = Dengue_Zika_indexing1(strains, z, i, j, var)

# var = ['Sm', 'Im', 'Sh', 'Ih', 'Ch', 'Rh', 'IIh', 'Rss', 'Inc_zika', 'Inc']

strains = 4

mosquito_indeces = 1:6
human_indeces = 7:110

mosquito_dengue = c()
human_dengue = c()

inc_dengue = c()
 
 for(str in 1:4){
#  [index] = 
  mosquito_dengue = c(mosquito_dengue,dengue_Zika_indexing1(strains=4, NA, str, NA, 'Im'))
  
  indeces = c()
  for(z in 1:4){
    temp.index=dengue_Zika_indexing1(strains,z,str,NA,'Ih')
    indeces=c(indeces,temp.index)
  } 

  for(z in 1:4){
    for(i in 1:strains){
      if(i != str){
        temp.index=dengue_Zika_indexing1(strains,z,i,str,'IIh')
        indeces=c(indeces, temp.index)
      }
    }
  }
  
  human_dengue = rbind(human_dengue, indeces)
  
  indeces=c()
  for(z in 1:4){
    indeces=c(indeces,dengue_Zika_indexing1(strains,z,str,NA,'Inc'))
  }

  inc_dengue = rbind(inc_dengue,indeces)  
} 
colnames(inc_dengue)=c("zika.S","zika.I","zika.C","zika.R")
row.names(inc_dengue)=paste0("DENV-",1:4)



## ----------------------------
## Zika

mosquito_zika = dengue_Zika_indexing1(strains,2,NA,NA,'Sm')

human_zika = c()

for(z in 1:4){
  indeces=c(dengue_Zika_indexing1(strains,z,NA,NA,'Sh'))

  for(i in 1:strains){
    indeces=c(indeces,dengue_Zika_indexing1(strains,z,i,NA,'Ih'))
  }
  for(i in 1:strains){
    indeces=c(indeces,dengue_Zika_indexing1(strains,z,i,NA,'Ch'))
  }  
  for(i in 1:strains){
    indeces=c(indeces,dengue_Zika_indexing1(strains,z,i,NA,'Rh'))
  }  
  for(i in 1:strains){
    for(j in 1:strains){
      if(i != j){
        indeces=c(indeces,dengue_Zika_indexing1(strains,z,i,j,'IIh'))
      }
    }
  }  
  indeces=c(indeces,dengue_Zika_indexing1(strains,z,NA,NA,'Rss'))

  human_zika = rbind(human_zika,indeces)
}
row.names(human_zika)=paste("zika",c("S","I","C","R"),sep=".")

IIh.vec=c()
for(i in 1:strains){
  for(j in 1:strains){
    if(i != j){
      IIh.vec=c(IIh.vec,paste0('IIh',i,j))
    }
  }
}  

colnames(human_zika)=c("Sh",paste0("Ih",1:4),paste0("Ch",1:4),paste0("Rh",1:4),IIh.vec,"Rss")

inc_zika = dengue_Zika_indexing1(strains,NA,NA,NA,'Inc_zika')

 
 
 
 
