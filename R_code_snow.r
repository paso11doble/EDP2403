# R CODE SNOW:
# ANALISI DELLA COPERTURA NEVOSA NEL TEMPO, SU COPERNICUS (20/05/20)

# DATI EUROPEI AGGIORNATI AL 18/05/20

setwd("C:/LAB/")

install.packages("ncdf4")

require(ncdf4)
require(raster)

# VOGLIAMO IMPORTARE E VISUALIZZARE IL FILE CON ESTENSIONE "NC"
# 2 FUNZIONI POSSIBILI: "raster" (->PER SINGOLO LIVELLO) O "brick" (-> PER PIU' LIVELLI O UNO STACK DI DATI)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# Exercise: plot snow cover with the cl palette
plot(snowmay, col=cl)

# CI ASPETTIAMO CHE QUESTA COPERTURA DIMINUISCA NEL TEMPO

# create new folder in LAB ("snow")
# import snow data

setwd("C:/LAB/snow")

# PER IMPORTARE TUTTI I FILES: "stack"

rlist <- list.files(pattern=".tif")

rlist
# [1] "predicted.snow.2025.norm.tif" "snow2000r.tif"               
# [3] "snow2005r.tif"                "snow2010r.tif"               
# [5] "snow2015r.tif"                "snow2020r.tif"    

list_rast <- lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

plot(snow.multitemp, col=cl)

# VOGLIAMO FARE UN PLOT DEI "TIF" AGLI ESTREMI (2000 E 2020); CON "$" UNIAMO LE SINGOLE IMMAGINI ALLO STACK

# PLOT
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# PLOT WITH LIMITS (zlim)
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=cl,zlim=c(0,250))
# impostiamo la "zlim":le mappe ora hanno la stessa legenda sulla destra: prima era diversa

# DIFFERENCE
diffsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(diffsnow, col=cldiff)


# PREDICTION (PREVSIONE PER 2025)
# go to IOL and download code "prediction.r" into folder "snow"

source("prediction.r")
# FUNZIONE "source": SERVE PER CARICARE CODICI DALL'ESTERNO

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
