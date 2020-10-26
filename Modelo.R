install.packages("leaflet")
library("leaflet")
install.packages("sf")
library("sf")
install.packages("tidyverse")
library("tidyverse")
install.packages("rgdal")
library("rgdal")
install.packages("raster")
library("raster")
install.packages("rasterVis")
library("rasterVis")
install.packages("rworldxtra")
library("rworldxtra")
install.packages("gdal")
library(gdal)
install.packages("sp")
library(sp)



install.packages("rgdal", configure.args = c("--with-proj-lib=/usr/local/lib/", "--with-proj-include=/usr/local/include/"))

install.packages("devtools")
install.packages("rgeos")

require(devtools)

install_version("gdal", version="1.1.1", repos="https://cran.r-project.org")

require(devtools)
install_version("ENmeval", version= "0.3.1",repos= "https://cran.r-project.org/web/packages/ENMeval/ENMeval.pdf")

install.packages("gdal")
install.packages("sp")

library("sf")
library("tidyverse")
library("rworldxtra")
library(broom)

install.packages("rgbif")
library (rgbif)
install.packages("dismo")
library(dismo)
install.packages("ENmeval")
library(ENMeval)
install.packages("maxnet")
library(maxnet) 

install.packages("spocc")
library(spocc)


# conectarse con gbif.org user
#"slgutierrez" # your gbif.org username pwd 
#"" # your gbif.org password email 
#"slgutierrez@uc.cl" # your email

write.table(Presencias, file="Presencias.csv")

read.csv(file="Presencias.csv")

# obtener la data completa de gbif

#occ <- occ_data(scientificName = 'Hippocamelus bisulcus', 
               # limit = 10000, 
               # hasCoordinate = TRUE, 
               # hasGeospatialIssue = FALSE)
#view(occ)

Presencias <- occ$data

#Filtros a presencias

Presencias <- Presencias %>% dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude),decimalLongitude != 0, decimalLatitude !=0) %>% 
  dplyr::select(decimalLongitude, decimalLatitude)

view(Presencias)

#Transformación a SF

Presencias_SF <-  Presencias %>% st_as_sf(coords  = c(1,2), crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")

#mapa

Chile <- getData(name = "GADM", country = "CHL", level = 1) %>% st_as_sf() %>% 
  st_crop(Presencias_SF)

ggplot() + geom_sf(data = Chile) +
  geom_sf(data = Presencias_SF) +
  theme_bw()

#Generar un buffer 

Hull <- Presencias_SF %>% st_union() %>% st_convex_hull

Buffer <- Hull %>% st_buffer(dist = 1) %>% st_as_sf()

plot(Buffer)

#mapa de polígono

Chile<- getData(name="GADM", country= "CHL", LEVEL=1  %>% st_as_sf() %>% st_make_valid() %>% st_crop(Buffer))

ggplot() + geom_sf(data= Chile) + geom_sf(data= Presencias_SF)+ theme_bw()

#capas bioclimaticas 

Bioclimatic <- getData(name = "worldclim", var = "bio", res = 0.5, 
                       lon = -70, lat = -50)


## cortar capas climaticas
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()

## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic), "_43")

plot(Bioclimatic[[c(1:4)]], colNA= "black")

#Generacion de background

#Hacer un data frame con presencias 1

Pres<- Presencias %>% dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude),decimalLongitude != 0, decimalLatitude !=0) %>% 
  dplyr::select(decimalLongitude, decimalLatitude) %>% mutate(Pres = 1)

set.seed(2020) #para la reproducibilidad 

bkg <- dismo::randomPoints(mask = Bioclimatic[[1]], n = 5000) %>% 
  as.data.frame() %>% rename(longitude = x, latitude = y) %>% 
  mutate(Pres = 0)


Pres_bkg <- bind_rows(Pres, bkg) %>% dplyr::filter(!is.na(decimalLatitude), 
!is.na(decimalLongitude)) %>% dplyr::select(-longitude, -latitude)

view(Pres_bkg)

Pres_bkg_Sf <- Pres_bkg %>% st_as_sf(coords = c(1, 2),
            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  


#Extraccion de datos 

Condiciones <- raster::extract(Bioclimatic, Pres_bkg_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_bkg)

#Primer modelo

Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones[, 1:19], 
               regmult = 1, maxnet.formula(p = Condiciones$Pres, data = Condiciones[, 
               1:19], classes = "lq"), clamp= FALSE)





