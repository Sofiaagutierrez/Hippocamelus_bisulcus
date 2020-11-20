
library("leaflet")
library("sf")
library("tidyverse")
library("rgdal")
library("raster")
library("lattice")
library("latticeExtra")
library("rasterVis")
library("rworldxtra")
library("rgdal")
library("sp")
library("ggforce")   #nombres graficos
library("scales")   #graficos
library("prediction")


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

#view(Presencias)

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

#ggplot() + geom_sf(data = Chile) +
  geom_sf(data = Presencias_SF) +
  theme_bw()


#Generar un buffer 

Hull <- Presencias_SF %>% st_union() %>% st_convex_hull

Buffer <- Hull %>% st_buffer(dist = 1) %>% st_as_sf()

#plot(Buffer)

#mapa de polígono

Chile<- getData(name="GADM", country= "CHL", level=1)  %>% st_as_sf()  %>% st_crop(Buffer)

#ggplot() + geom_sf(data = Buffer) + geom_sf(data= Chile) + geom_sf(data= Presencias_SF)  + theme_bw()

#capas bioclimaticas 

Bioclimatic <- getData(name = "worldclim", var = "bio", res = 0.5, 
                       lon = -70, lat = -50)

## cortar capas climaticas
Bioclimatic <- Bioclimatic %>% crop(Buffer) %>% trim()

## Cortamos el Tile
names(Bioclimatic) <- str_remove_all(names(Bioclimatic), "_43")

#plot(Bioclimatic[[c(1:4)]], colNA= "black")


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

#view(Pres_bkg)

Pres_bkg_Sf <- Pres_bkg %>% st_as_sf(coords = c(1, 2),
            crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")  


#Extraccion de datos 

Condiciones <- raster::extract(Bioclimatic, Pres_bkg_Sf) %>% 
  as.data.frame() %>% bind_cols(Pres_bkg) 

#view(Condiciones)

#Primer modelo

Mod1 <- maxnet(p = Condiciones$Pres, data = Condiciones [, 1:19], 
        regmult = 1, maxnet.formula(p = Condiciones$Pres, 
      data = Condiciones[,1:19], classes = "lqh"), clamp= F)

#plot(predict(Bioclimatic,Mod1, type = "cloglog"), colNA = "black", main= "Modelo de distribución actual del huemul") 


#respuestas de variables 


#plot(Mod1, type= "cloglog",  c("bio1", "bio2", "bio3"))

Prediction <- predict(Bioclimatic, Mod1, type = "cloglog")


#plot(Prediction, colNA= "white")


#Transformar en presencia y ausencia (ya que eso es lo que tratamos de predecir)

Eval <- dismo::evaluate(p = Pres[, 1:2], a = bkg [, 1:2], model = Mod1, 
                        x = Bioclimatic, type = "cloglog")

EvalDF <- Eval@confusion %>% as.data.frame %>% mutate(Threshold = Eval@t) %>% 
mutate(TP_TN = (tp/nrow(Presencias)) + (tn/5000))
head(EvalDF) 

#generacion de data frame
Eval@confusion

#view(EvalDF)
EvalThres <- EvalDF %>% dplyr::filter(TP_TN == max(TP_TN))
#view(EvalThres)

#Prediction

#umbral 1

 Prediction_DF <- Prediction %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>%
   mutate(Binary = ifelse(layer >=  EvalThres$Threshold [1],  "Presencia", "Ausencia"))

 #umbral 2
  
Prediction_DF2 <- Prediction %>% as("SpatialPixelsDataFrame") %>% as.data.frame() %>% mutate(Binary1 = ifelse(layer >= 
  EvalThres$Threshold[1],  "Presencia", "Ausencia"), Binary2 =ifelse(layer >= 
  EvalThres$Threshold[2],  "Presencia", "Ausencia")) %>% 
  pivot_longer(starts_with("Binary"), names_to ="Umbral", values_to = "Presencia")   #esto ultimo es para hacer un buen grafico
 
 
#Graficos  presente 

#ver si funciona y cambiar escala de colores, cambiar detalles, white por black
#ggplot() + geom_raster(data = Prediction_DF, aes(x = latitud, y = longitud, fill = Binary)) + geom_sf(data = Chile, alpha = 0, color = "white", size = 0.5) + scale_fill_viridis_d() + labs(x = NULL, y = NULL)

#Binary #grafico de ambos umbrales
#ggplot() + geom_raster(data = Prediction_DF2, aes(x = x, y = y, fill = Presencia)) +geom_sf(data = Chile, alpha = 0, color = "white", size = 0.5) +scale_fill_viridis_d() + labs(x = NULL, y = NULL) +facet_wrap(~Umbral)


#Transformacion raster en Pres Aus
#transformacion de raster en prediccion binaria 


Prediction_Bin <- Prediction

#ocupar funcion reclassify, para eso debo armar una matriz

m <- c(-Inf, EvalThres$Threshold[1], 0, EvalThres$Threshold[1], Inf, 1)
m <- matrix(m, ncol =3, byrow = T) #transformacion a matriz desde puntos de raster

Prediction_Bin <- reclassify(Prediction_Bin, m) #esto es lo mismo que lo anterior, pero la diferencia es que el anterior es un raster y este es un data frame 

#plot(Prediction_Bin, colNA= "black") #hacer mas lindo

#Modelando el futuro 

library("readxl")
library("xlsx")
library("csvread")


#Futuros <- read_excel("~/Documents/GcmR.xlsx") #ahora me funciona
view(GCMR)

#Futuros <- csvread("~/Documents/GCMR.csv", coltypes = "longhex", header= TRUE) #resulta

#Futuros <- read.csv("~/Documents/GCMR.csv", sep="")

Futuros <- GCMR
#view(Futuros)

#Futuros$Within_circle <- c(Futuros$Within_circle[-1], NA)
#Futuros <- Futuros %>% 
  #dplyr::filter(!is.na(Within_circle), Within_circle == "TRUE") %>% #sacar esta linea
  

Futuros <- Futuros %>% mutate(Cuadrante = case_when(x_axis >= 0 & y_axis >= 0 ~ "I",
                                                  x_axis < 0 & y_axis >= 0 ~ "II",
                                                  x_axis < 0 & y_axis < 0 ~ "III",
                                                 x_axis >= 0 & y_axis < 0 ~ "IV"))
#view(Futuros)


#Elección de futuros (escoger 1 por cada cuadrante)

Futuros <- Futuros %>% group_split(Cuadrante)

#Modelo 1

Futuros[[1]] %>% View()

Ipsl <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='IP', year=70) %>% 
  crop(Bioclimatic)

#plot(Ipsl)

names(Ipsl) <- names(Bioclimatic)

#plot(Ipsl)

Prediction_Ipsl <- predict(Ipsl, Mod1, type = "cloglog")

#plot(Prediction_Ipsl) #mejorar

#Transformar en presencia ausencia

Prediction_Bin_Ipsl <- Prediction_Ipsl

m <- c(-Inf, EvalThres$Threshold[1], 0, EvalThres$Threshold[1], Inf, 1)
m <- matrix(m, ncol =3, byrow = T) 

Prediction_Bin_Ipsl <- reclassify(Prediction_Bin_Ipsl, m)
Prediction_Bin_Ipsl <- resample(Prediction_Bin_Ipsl, Prediction_Bin, method = "ngb")

#plot(Prediction_Bin_Ipsl) #mejorar grafico, ver que escala quede 1 y 0 (dos colores) agregar gráfico de columnas al final 

#Modelo 2

Futuros[[2]] %>% View()

Gfdl <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='GF', year=70) %>% 
  crop(Bioclimatic)

#plot(Gfdl)

names(Gfdl) <- names(Bioclimatic)

#plot(Gfdl)

Prediction_Gfdl <- predict(Gfdl, Mod1, type = "cloglog")

#plot(Prediction_Gfdl) #mejorar

#Transformar en presencia ausencia

Prediction_Bin_Gfdl <- Prediction_Gfdl

m <- c(-Inf, EvalThres$Threshold[1], 0, EvalThres$Threshold[1], Inf, 1)
m <- matrix(m, ncol =3, byrow = T) 

Prediction_Bin_Gfdl <- reclassify(Prediction_Bin_Gfdl, m)
Prediction_Bin_Gfdl<- resample(Prediction_Bin_Gfdl, Prediction_Bin, method = "ngb")

#plot(Prediction_Bin_Gfdl) #mejorar grafico, ver que escala quede 1 y 0 (dos colores) agregar gráfico de columnas al final 

#Modelo 3

Futuros[[3]] %>% View()

Miroc <- getData('CMIP5', var='bio', res=2.5, rcp=85, model='MI', year=70) %>% 
  crop(Bioclimatic)

#plot(Miroc)

names(Miroc) <- names(Bioclimatic)

#plot(Miroc)

Prediction_Miroc <- predict(Miroc, Mod1, type = "cloglog")

#plot(Prediction_Miroc) #mejorar

#Transformar en presencia ausencia

Prediction_Bin_Miroc <- Prediction_Miroc

m <- c(-Inf, EvalThres$Threshold[1], 0, EvalThres$Threshold[1], Inf, 1)
m <- matrix(m, ncol =3, byrow = T) 

Prediction_Bin_Miroc <- reclassify(Prediction_Bin_Miroc, m)
Prediction_Bin_Miroc <- resample(Prediction_Bin_Miroc, Prediction_Bin, method = "ngb")

#plot(Prediction_Bin_Miroc) #mejorar grafico, ver que escala quede 1 y 0 (dos colores) agregar gráfico de columnas al final 

#Modelo 4 

Futuros[[4]] %>% View()

Cesm<- getData('CMIP5', var='bio', res=2.5, rcp=85, model='CC', year=70) %>% 
  crop(Bioclimatic)

#plot(Cesm)

names(Cesm) <- names(Bioclimatic)

#plot(Miroc)

Prediction_Cesm <- predict(Cesm, Mod1, type = "cloglog")

#plot(Prediction_Cesm) #mejorar

#Transformar en presencia ausencia

Prediction_Bin_Cesm <- Prediction_Cesm

m <- c(-Inf, EvalThres$Threshold[1], 0, EvalThres$Threshold[1], Inf, 1)
m <- matrix(m, ncol =3, byrow = T) 

Prediction_Bin_Cesm <- reclassify(Prediction_Bin_Cesm, m)
Prediction_Bin_Cesm<- resample(Prediction_Bin_Cesm, Prediction_Bin, method = "ngb")

#plot(Prediction_Bin_Cesm) #mejorar grafico, ver que escala quede 1 y 0 (dos colores) agregar gráfico de columnas al final 


#Graficos

par(mfrow =c(1,5))
plot(Prediction_Bin, main= "Presente", colNA= "black", xlab= "Lat", ylab= "Long", scale_fill_viridis_d())
plot(Prediction_Bin_Gfdl, main= "Gfdl", colNA= "black", xlab= "Lat", ylab= "Long")
plot(Prediction_Bin_Ipsl, main= "Ipsl", colNA= "black", xlab= "Lat", ylab= "Long")
plot(Prediction_Bin_Miroc, main= "Miroc",colNA= "black", xlab= "Lat", ylab= "Long")
plot(Prediction_Bin_Cesm, main= "Cesm", colNA= "black", xlab= "Lat", ylab= "Long")

scale_fill_viridis_d() 





