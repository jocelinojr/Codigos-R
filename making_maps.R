####################################################################################
# Fazendo mapas no R

# Instalando as packages necessárias ao trabalho
install.packages(c("rgeos", "gpclib", "maptools", "sp"))

install.packages("rgdal")
install.packages("sf")

# carregando as packages
library(rgeos)
library(maptools)
library(rgdal)
library(sf)
library(ggplot2)
library(readxl)
library(tidyverse)
library(dplyr)




# seta o diretório de trabalho
getwd()
setwd("C:/Users/Jocelino Junior/Documents/R")

# carrega nosso arquivo de dados
processos <- read_excel("fundef_processos.xls")

# carrega o shapefile com a função read_sf....não consegui de primeira
#mun <- read_sf("Municipios.shp")
#mun <- fortify(mun, region = "Nome_Meso")

# carrega o shapefile com st_read...funcionou mais fácil
mun <- st_read("Municipios.shp")
plot(st_geometry(mun))










