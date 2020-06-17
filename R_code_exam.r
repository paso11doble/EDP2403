# R_code_exam.r





##########################################################################################################################


### 11. R Code Crop (Ritaglio)

setwd("C:/LAB/snow")

require(ncdf4)
require(raster)

# Exercise: upload the whole snow set (from 2000 to 2020, not predicted!) (utilizzare prima lapply poi stack)

rlist <- list.files(pattern="snow")
rlist

# save raster into list
# lapply

list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('darkblue','blue','light blue'))(100)

plot(snow.multitemp, col=clb)

# N.B. non c'è bisogno di utilizzare il par



snow.multitemp
# dai names selezioniamo il 2010r
plot(snow.multitemp$snow2010r, col=clb)


# funzione zoom (esclusiva di raster)
zoom(nome immagine, estensione)

# funzione ext: come definire l'estensione di una certa area
extension <- c(xmin, xmax, ymin, ymax)

      
# dove casca l'italia(prima zoom poi crop)
      
extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
      
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)     


plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent())
# disegnare un rettangolo a partire dall'alto a sx dell'area di riferimento

# funzione crop
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

# Exercise: crop the italy extent on the WHOLE STACK of snow layers (quindi a partire dallo stack SNOW.MULTITEMP)
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)

plot(snow.multitemp.italy, col=clb, zlim=c(20,200))
# uniformiamo i limiti

# BOXPLOT DELLA COPERTURA NEVOSA
# vedi rif. multitemp.NO2
# come varia in media la copertura nevosa negli anni?

boxplot(snow.multitemp.italy, horizontal=T, outline=F)



##########################################################################################################################


### 12. Species Distribution Modelling

install.packages("sdm")
library(sdm)

library(raster)
library(rgdal)


# SPECIES

file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)
# carichiamo lo shapefile "species" dall'esterno all'interno del pacchetto "sdm"
# funzione "shapefile": funzione di "rgdal" (per files vettoriali), serve per mappare la distribuzione di una determinata specie a terra

species
# class       : SpatialPointsDataFrame 
# features    : 200 
# extent      : 110112, 606053, 4013700, 4275600  (xmin, xmax, ymin, ymax)
# crs         : +proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       : Occurrence 
# min values  :          0 
# max values  :          1 

N.B. Zone=Fuso -> siamo in Spagna (fuso 30)

species$Occurrence
# "species" è formato da dei punti con coordinate e ogni punto è legato al fatto che la specie sia stata vista o meno

plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# all'interno del dataset species: le occorrenze sono uguali a 1 -> specie presente (punto blu)
#                                  le occorrenze sono uguali a 0 -> specie assente (punto rosso)


# MODEL

path <- system.file("external", package="sdm") 
# importiamo la cartella external all'interno del pacchetto "sdm"


# PREDICTORS

lst <- list.files(path=path,pattern='asc$',full.names = T) #
preds <- stack(lst) 

lst
# [1] "C:/R-3.6.3/library/sdm/external/elevation.asc"    
# [2] "C:/R-3.6.3/library/sdm/external/precipitation.asc"
# [3] "C:/R-3.6.3/library/sdm/external/temperature.asc"  
# [4] "C:/R-3.6.3/library/sdm/external/vegetation.asc"   

abbiamo 4 files/layer ASCII nella lista
ne facciamo un singolo oggetto con stack (chiamato preds)

Predittori: variabili per prevedere quella che sarà la distribuzione della nostra specie

cl <- colorRampPalette(c('blue','orange', 'red', 'yellow'))(100)
plot(preds, col=cl)

plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
prendiamo i punti dove occorrenze=1, quindi solo punti dove la specie è presente
(low elevation, la specie in questione non ama la montagna)

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,],pch=16)
le piacciono le temperature medio-alte

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
situazione intermedia

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
la specie predilige l'ombreggiatura


creiamo un modello lineare che raccolga tutte queste variabili (attraverso un'equazione)
GLM (Generalized Linear Model)

# MODEL

d <- sdmData(formula=Occurrence~., train=species, predictors=preds)
d
# class                                 : sdmdata 
# =========================================================== 
# number of species                     :  1 
# species names                         :  Occurrence 
# number of features                    :  4 
# feature names                         :  elevation, precipitation, temperature, ... 
# type                                  :  Presence-Absence 
# has independet test data?             :  FALSE 
# number of records                     :  200 
# has Coordinates?                      :  TRUE 

train: tutti i dati raccolti a terra
predictors: variabili (4)
la tilde serve per indicare l'uguale nei modelli

funzione sdmdata

m1 <- sdm(Occurrence ~ elevation+precipitation+temperature+vegetation, data=d, methods='glm')

p1 <- predict(m1, newdata=preds)
funzione predict: facciamo una mappa previsionale del modello

plot(p1,col=cl)
points(species[species$Occurrence == 1,], pch=16)


####################################################################################################

#EXAM

# Here put your code

