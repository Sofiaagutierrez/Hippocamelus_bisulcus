
library("leaflet")
library("sf")
library("tidyverse")
library("rgdal")
library("raster")
library("rasterVis")
library("rworldxtra")
library("rgdal")
library("sp")
library("ggforce")   #nombres graficos
library("scales")   #graficos



#install.packages("rgdal", configure.args = c("--with-proj-lib=/usr/local/lib/", "--with-proj-include=/usr/local/include/"))

library("rgeos")
#require(devtools)
#install_version("gdal", version="1.1.1", repos="https://cran.r-project.org/web/packages/gdalUtilities/index.html")
#library("gdal")

#require(devtools)
#install_version("ENMeval", version= "0.3.1",repos= "https://cran.r-project.org/web/packages/ENMeval/ENMeval.pdf")

#install.packages("gdal")
#library("gdal")


library("sf")
library("tidyverse")
library("rworldxtra")
library("broom")


library ("rgbif")
library("dismo")
library("ENMeval")
library("maxnet") 
library("spocc")
library("dplyr")


# conectarse con gbif.org user
#"slgutierrez" # your gbif.org username pwd 
#"" # your gbif.org password email 
#"slgutierrez@uc.cl" # your email

#write.table(Presencias, file="Presencias.csv")

#read.csv(file="Presencias.csv")

#Presencias <-read.csv(file="Presencias.csv")

Presencias <- read.csv("~/Documents/Presencias.csv", sep="")

# obtener la data completa de gbif

#occ <- occ_data(scientificName = 'Hippocamelus bisulcus', 
               # limit = 10000, 
               # hasCoordinate = TRUE, 
               # hasGeospatialIssue = FALSE)
#view(occ)

#Presencias <- occ$data  #ver porque se comenta 

#Filtros a presencias

Presencias <- Presencias %>% dplyr::filter(!is.na(decimalLatitude), !is.na(decimalLongitude),decimalLongitude != 0, decimalLatitude !=0) %>% 
  dplyr::select(decimalLongitude, decimalLatitude)

#view(Presencias)

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

Chile<- getData(name="GADM", country= "CHL", level=1)  %>% st_as_sf()  %>% st_crop(Buffer)

ggplot() + geom_sf(data = Buffer) + geom_sf(data= Chile) + geom_sf(data= Presencias_SF)  + theme_bw()

# este tenía antes ver diferencia por el de arriba ggplot() + geom_sf(data= Chile) + geom_sf(data= Presencias_SF)+ theme_bw()

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
  as.data.frame() %>% rename(decimalLongitude = x, decimalLatitude = y) %>% 
  mutate(Pres = 0)


Pres_bkg <- bind_rows(Pres, bkg) %>% dplyr::filter(!is.na(decimalLatitude), 
!is.na(decimalLongitude)) 

view(Pres_bkg)

Pres_bkg_Sf <- Pres_bkg %>% st_as_sf(coords = c(1, 2),
            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  


#Extraccion de datos 

Condiciones <- raster::extract(Bioclimatic, Pres_bkg_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_bkg) 

view(Condiciones)

#Primer modelo

Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones [, 1:19], 
        regmult = 1, maxnet.formula(p = Condiciones$Pres, 
      data = Condiciones[,1:19], classes = "lqh"), clamp= F)

plot(predict(Bioclimatic,Mod1, type = "cloglog"), colNA = "black",
     main= "Modelo de distribución actual del huemul") 


#respuestas de variables 


plot(Mod1, type= "cloglog",  c("bio1", "bio2", "bio3"))

Prediction <- predict(Bioclimatic, Mod1, type = "cloglog")


plot(Prediction, colNA= "white")


#Transformar en presencia y ausencia (ya que eso es lo que tratamos de predecir)

Eval <- dismo::evaluate(p = Pres[, 1:2], a = bkg [, 1:2], model = Mod1, 
                        x = Bioclimatic, type = "cloglog")

EvalDF <- Eval@confusion %>% as.data.frame %>% mutate(Threshold = Eval@t) %>% 
mutate(TP_TN = (tp/nrow(Presencias)) + (tn/5000))
head(EvalDF) 

#generacion de data frame
Eval@confusion

view(EvalDF)

#Prediction

EvalThres <- EvalDF %>% dplyr::filter(TP_TN == max(TP_TN))

view(EvalThres)

#ver lo siguiente me da error

 Prediction <- Prediction %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% mutate(Binary = ifelse(layer >= 
                                                  EvalThres$Threshold,  "Presencia", "Ausencia"))
 
 
 #este me resulta
 
 Prediction <- Prediction %>% mutate(Binary = ifelse(layer >= 
                             EvalThres$Threshold, "Pres", "bkg"))

#hacer gráfico Modelo del Presente

 
Prediction <- predict(Bioclimatic, Mod1, type = "cloglog")

plot(Prediction, colNA= "black")





