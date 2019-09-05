## choose order locations


# get latitude and longitude coordinates for the locations
library("ggmap")

# ?geocode
# 
# geocode('state of Rio Grande do Norte,Brazil')
# geocode('State of Santa Catarina,Brazil')

Brazil.names =   sort(c("Rio Grande do Sul","Santa Catarina","Paraná","Rio de Janeiro","São Paulo","Mato Grosso do Sul",
                       "Espírito Santo","Minas Gerais","Goiás","Distrito Federal","Mato Grosso","Bahia","Rondônia", 
                       "Sergipe","Tocantins","Alagoas","Acre","Pernambuco","Piauí","Paraíba","Rio Grande do Norte",
                       "Pará","Maranhão","Ceará","Amazonas","Amapá","Roraima"))
Brazil.state.names= sort(c("Rio_Grande_do_Sul","Santa_Catarina","Parana","Rio_de_Janeiro","Sao_Paulo","Mato_Grosso_do_Sul",
                                    "Espirito_Santo","Minas_Gerais","Goias","Distrito_Federal","Mato_Grosso","Bahia","Rondonia", 
                                    "Sergipe","Tocantins","Alagoas","Acre","Pernambuco","Piaui","Paraiba","Rio_Grande_do_Norte",
                                    "Para","Maranhao","Ceara","Amazonas","Amapa","Roraima"))

#new.state.names = paste("State of ", Brazil.state.names, ", Brazil",sep="")

#lat.longs=geocode(new.state.names)
#row.names(lat.longs)=Brazil.state.names

#lat.longs[which(is.na(lat.longs[1])),]=geocode(paste(Brazil.state.names[which(is.na(lat.longs[1]))],",Brazil"))

#lat.longs
Brazil.lat = c(-9.0237964,-9.5713058,0.9019925,-3.4168427,-12.5797380,-5.4983977,-15.7986258,-19.1834229,-15.8270369,-4.9609498,-12.6818712,-20.7722295,
              -18.5121780,-1.9981271,-7.2399609,-25.2520888,-8.8137173,-7.7183401,-22.3534263,-5.4025803,-30.0346316,-11.5057341,2.7375971,-27.2423392,
              -23.5431786,-10.5740934,-10.1752800)
names(Brazil.lat)=Brazil.state.names


#lat.ordered.states=Brazil.state.names[order(lat.longs[,2])]
lat.ordered.Brazil=Brazil.state.names[rev(order(Brazil.lat))]


our.Colombia.names=c("Amazonas","Antioquia","Arauca","Atlantico","Bogota","Bolivar","Boyaca","Caldas","Caqueta","Casanare","Cauca",
                     "Cesar","Choco","Cordoba","Cundinamarca","Guainia","Guajira","Guaviare","Huila","Magdalena","Meta","Narino",
                     "Norte_Santander","Putumayo","Quindio","Risaralda","San_Andres","Santander","Sucre","Tolima","Valle","Vaupes","Vichada")

Colombia.names=c("Amazonas","Antioquia","Arauca","Atlántico","Bogotá","Bolívar","Boyacá","Caldas","Caquetá","Casanare","Cauca",
                 "Cesar","Chocó","Córdoba","Cundinamarca","Guainía","La Guajira","Guaviare","Huila","Magdalena","Meta","Nariño",
                 "Norte de Santander","Putumayo","Quindio","Risaralda","Archipiélago de San Andrés","Santander","Sucre","Tolima",
                 "Valle del Cauca","Vaupés","Vichada")   
# new.CO.names = paste(Colombia.names, ",Colombia",sep="")
# 
# Colombia.lat.longs=geocode(new.CO.names)
# row.names(Colombia.lat.longs)=Colombia.names
# Colombia.lat.longs[which(is.na(Colombia.lat.longs[1])),]=geocode(paste(Colombia.names[which(is.na(Colombia.lat.longs[1]))],"Colombia",sep=", "))
# Colombia.lat.longs[which(is.na(Colombia.lat.longs[1])),]=geocode(paste(Colombia.names[which(is.na(Colombia.lat.longs[1]))],",Colombia"))
# Colombia.lat.longs[which(Colombia.names=="Putumayo"),]=geocode("Putumayo,Colombia")

Colombia.lat=c(-1.4429123, 7.1986064, 7.076172, 10.6966159, 4.7109886, 8.6704382, 5.454511, 5.29826, 0.869892, 
                     5.7589269, 2.7049813, 9.3372948, 5.2528033, 8.049293, 5.026003, 2.585393, 11.3547743, 2.043924, 
                     2.5359349, 10.4113014, 3.2719904, 1.289151, 7.9462831, 0.4359506, 4.4610191, 5.3158475, 12.5567324, 
                     6.6437076, 8.813977, 4.0925168, 3.8008893, 0.8553561, 4.4234452)
#Colombia.lat=cbind(our.Colombia.names,Colombia.lat)
names(Colombia.lat)=our.Colombia.names

CO.lat.order=rev(order(Colombia.lat))
lat.ordered.CO=our.Colombia.names[CO.lat.order]

#save(Brazil.lat,Colombia.lat,file="lat_values.Rdata")
