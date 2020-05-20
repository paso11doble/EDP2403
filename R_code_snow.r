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

rlist=list.files(pattern=".png", full.names=T)


