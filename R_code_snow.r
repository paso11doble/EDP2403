# R CODE SNOW:
# ANALISI DELLA COPERTURA NEVOSA NEL TEMPO, SU COPERNICUS (20/05/20)

setwd("C:/LAB/")

install.packages("ncdf4")

require(ncdf4)
require(raster)

# vogliamo importare e visualizzare il file con estensione "NC"
# 2 funzioni possibili: raster (singolo livello) o brick (più livelli)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# Exercise: plot snow cover with the cl palette
plot(snowmay, col=cl)

# ci aspettiamo che questa copertura (aggiornata al 18/05/20 in Europa) diminuisca nel tempo

# create new folder in LAB ("snow")
# import snow data

setwd("C:/LAB/snow")

# vogliamo importare tutti i file: funzione "stack"

rlist <- list.files(pattern=".tif")

rlist
# [1] "predicted.snow.2025.norm.tif" "snow2000r.tif"               
# [3] "snow2005r.tif"                "snow2010r.tif"               
# [5] "snow2015r.tif"                "snow2020r.tif"    

list_rast <- lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

plot(snow.multitemp, col=cl)

# vogliamo fare un plot delle immagini agli estremi (2000 e 2020); con "$" uniamo le singole immagini allo stack

# PLOT
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# PLOT WITH LIMITS
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=cl,zlim=c(0,250))
# impostiamo la "zlim":le mappe ora hanno la stessa legenda sulla destra: prima era diversa

# DIFFERENCE
diffsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(diffsnow, col=cldiff)


# PREDICTION (previsione per il 2025)
# go to IOL and download code "prediction.r" into folder "snow"

source("prediction.r")
# funzione "source": serve per caricare codici dall'esterno

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)
