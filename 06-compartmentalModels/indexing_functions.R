## Dengue-Zika compartmental model simulation index processing functions 
## Originally written by Luis Mier-y-Teran-Romero in Matlab and translated by 
## Rebecca Borchering to R (July 21, 2018)
## 
## z = Zika index in 1...4 (Susceptible, Infectious, Cross-protected, Recovered)
## In the simulation output files: first column is time, most of the columns correspond to compartments,
## except the 'Inc' columns at the end are aggregated. For example var='Inc', i=4, z=4 is the 
## aggregated number of individuals that are recovered from Zika and either have DENV4 as a primary
## or secondary infection.

dengue_Zika_indexing1 <- function(strains=4,z=NA,i,j=NA,var){
  # var = ['Sm', 'Im', 'Sh', 'Ih', 'Ch', 'Rh', 'IIh', 'Rss', 'Inc_zika', 'Inc']
  
  zika = 4
  
  # Checking indeces in the right range 
  if(!is.na(z)){
    if(z > zika){
      stop('z = 1..4')
    }
  }

  if(!is.na(i)){
    if(i > strains){
      stop('i,j = 1..strains')
    }
  }
  
  if(!is.na(j)){
    if(j > strains){
      stop('i,j =1..strains')
    }
  }

  # Find variable
  switch(var,
         'Sm'={index=z},
         'Im'={index=(zika-2)+i},
         'Sh'={index=(zika-2)+strains
            index=index+z},
         'Ih'={index=(zika-2)+strains+zika
              index=index+(z-1)*strains+i},
         'Ch'={index=(zika-2)+strains+zika+zika*strains
              index=index+(z-1)*strains +i},
         'Rh'={index=(zika-2)+strains+zika+2*zika*strains
              index=index+(z-1)*strains+i},
         # IIh isn't working right now
         'IIh'={index=(zika-2)+strains+zika+3*zika*strains
              k=0
              for(ik in 1:strains){
                for(jk in 1:strains){
                  if(ik != jk){
                    k=k+1
                    if((ik==i) && (jk==j)){
                      index_1=k
                    }
                  }
                }
              }
                index=index+(z-1)*strains*(strains-1)+index_1
                },
         'Rss'={index=(zika-2)+strains+zika+3*zika*strains+zika*strains*(strains-1)
              index=index+z},
         'Inc_zika'={index=(zika-2)+strains +2*zika +3*zika*strains+zika*strains*(strains-1)
              index=index+1},
         'Inc'={index = (zika-2)+strains+2*zika+3*zika*strains+zika*strains*(strains-1)+1
              index=index+(z-1)*strains+i}
         )
  return(index)
}

dengue_Zika_indexing2 <- function(vector_index){
  
  # var = ['Sm', 'Im', 'Sh', 'Ih', 'Ch', 'Rh', 'IIh', 'Rss', 'Inc_zika', 'Inc']
  strains = 4
  zika_levels = 4
  
  index = 0
  
  #Sm
  if(index < vector_index){
    for(z in 1:2){
      index = index +1
      
      if(index == vector_index){
        var='Sm'
        indeces=z
        return(c(var='Sm',indeces=z))
      }
    }
  }
  
  #Im
  if(index < vector_index){
    for(i in 1:strains){
      index = index+1
      if(index == vector_index){
        var='Im'
        indeces=i
        return(c(var='Im',indeces=i))
      }
    }
  }
  
  #Sh
  if(index < vector_index){
    for(z in 1:zika_levels){
      index=index+1
      if(index == vector_index){
        var='Sh'
        indeces = z
        return(c(var="Sh",indeces=z))
      }
    }
  }
  
  #Ih
  if(index < vector_index){
    for(z in 1:zika_levels){
      for(i in 1:strains){
        index = index+1
        if(index == vector_index){
          var='Ih'
          indeces=paste0(z,i)
          return(c(var='Ih',indeces=paste0(z,i)))
        }
      }
    }
  }
  
  #Ch
  if(index < vector_index){
    for(z in 1:zika_levels){
      for(i in 1:strains){
        index = index+1
        if(index == vector_index){
          var='Ch'
          indeces=paste0(z,i)
          return(c(var='Ch',indeces=paste0(z,i)))
        }
      }
    }
  }
  
  #Rh
  if(index < vector_index){
    for(z in 1:zika_levels){
      for(i in 1:strains){
        index = index+1
        if(index == vector_index){
          var='Rh'
          indeces=paste0(z,i)
          return(c(var='Rh',indeces=paste0(z,i)))
        }
      }
    }
  }
  
  #IIh
  if(index < vector_index){
    for(z in 1:zika_levels){
      for(i in 1:strains){
        for(j in 1:strains){
          if(i != j){
            index = index+1
            if(index == vector_index){
              var='IIh'
              indeces=paste0(z,i,j)
              return(c(var='IIh',indeces=paste0(z,i,j)))
            }
          }
        }
      }
    }
  }
  
  #Rss
  if(index < vector_index){
    for(z in 1:zika_levels){
      index = index+1
      if(index == vector_index){
        var='Rss'
        indeces=z
        return(c(var='Rss',indeces=z))
      }
    }
  }
  
  #Inc Zika
  if(index < vector_index){
    index = index+1
    if(index == vector_index){
      var='Inc_zika'
      indeces=NA
      return(c(var='Inc_zika',indeces=NA))
    }
  }
  
  #Inc
  if(index < vector_index){
    for(z in 1:zika_levels){
      for(i in 1:strains){
        index = index +1
        if(index == vector_index){
          var='Inc'
          indeces=paste0(z,i)
          return(c(var='Inc',indeces=paste0(z,i)))
        }
      }
    }
  }
  
}
