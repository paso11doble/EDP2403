# R_code_snow.r (Analisi della copertura nevosa nel tempo, su Copernicus) (Pratica del 20/05/20)

# setwd("C:/LAB/")

# install.packages("ncdf4")
require(ncdf4)
require(raster)

vogliamo importare e visualizzare il file con estensione "NC": 2 funzioni possibili: raster (singolo livello) e brick (più livelli)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# Exercise: plot snow cover with the cl palette
plot(snowmay, col=cl)

ci aspettiamo che questa copertura (aggiornata al 18/05 in Europa) diminuisca nel tempo

# import snow data

# setwd("C:/LAB/snow")

vogliamo importare tutti insieme i file: funzione "stack"

rlist <- list.files(pattern=".tif")
rlist
list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl)

vogliamo fare un plot delle immagini agli estremi (2000 e 2020); col $ uniamo le singole immagini allo stack

par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# plot with limits
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=cl,zlim=c(0,250))
imposto la "zlim":le mappe ora hanno la stessa legenda sulla destra: prima era diversa

# difference
diffsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(diffsnow, col=cldiff)

# prediction (previsione per il 2025)
# go to IOL and download code "prediction.r" into folder "snow"
source("prediction.r")

funzione "source": serve per caricare codici dall'esterno

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

