install.packages("sf")

install.packages("brazilmaps")

library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(brazilmaps)
theme_set(theme_bw())


#############################################
# Usando a package sf

getwd()
list.files()
municipios_shp <- st_read("municipios_2010.shp")
glimpse(municipios_shp)



class(municipios_shp)

pb <- municipios_shp[municipios_shp$uf == "PB", ]
# converte a população para numérico
pb$populacao <- as.numeric(as.character(pb$populacao))

p <- ggplot(data=pb) + geom_sf(mapping = aes(fill=populacao))
p + labs(title = "Meu primeiro mapa da PB", x="Longitude", y="Latitude") +
  scale_fill_viridis_c()


################################################
# Usando Brazil maps


## Retrieving the map from the State of Rio de Janeiro
rio_map <- get_brmap(geo = "State",
                     geo.filter = list(State = 33),
                     class = "sf")

str(rio_map)

rio_map$geometry
p <- ggplot(data=rio_map, aes(x=, y=) ) + geom_polygon()







