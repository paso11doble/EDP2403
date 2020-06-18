#####################                                                                               ######################
### R_code_exam.r ###                                                                               ### MICHELE PASINI ###
#####################                                                                               ######################
                                           ########################################
                                           ### MODULO DI ECOLOGIA DEL PAESAGGIO ###
                                           ########################################

# APPUNTI IDENTIFICATI IN MAIUSCOLO

# Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

# 1. R_code_first.r
# 2. R_code_spatial.r
# 3. R_code_spatial2.r
# 4. R_code_point_pattern.r
# 5. R_code_teleril.r
# 6. R_code_landcover.r
# 7. R_code_multitemp.r
# 8. R_code_multitemp_NO2.r
# 9. R_code_snow.r
# 10. R_code_patches.r
# 11. R_code_crop.r - Exam Simulation
# 12. Species Distribution Modelling


##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

##########
## (11) ## R CODE CROP: COME EFFETTUARE UN "RITAGLIO" (03/06/20)
##########

# FUNZIONE "crop": SERVE PER FARE UN RITAGLIO SU UNA DETERMINATA ZONA A PARTIRE DA UNA SCALA GLOBALE

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

# N.B. NON C'E' BISOGNO DI UTILIZZARE "par"!


snow.multitemp
# DAI NOMI SELEZIONIAMO 2010r

plot(snow.multitemp$snow2010r, col=clb)


# FUNZIONE "zoom": PRESENTE NEL PACCHETTO RASTER
zoom(nome immagine, estensione)

# FUNZIONE "ext": SERVE PER DEFINIRE L'ESTENSIONE DI UNA CERTA AREA
extension <- c(xmin, xmax, ymin, ymax)

      
# DOVE SI TROVA L'ITALIA? (fare prima zoom poi crop)
      
extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
      
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)     


plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent())
# DISEGNARE UN RETTANGOLO DALL'ALTO A SX DELL'AREA DI RIFERIMENTO (con "drawEstent()")

# funzione crop
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

# Exercise: crop the Italy extent on the WHOLE STACK of snow layers (quindi a partire dallo stack SNOW.MULTITEMP)
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)

plot(snow.multitemp.italy, col=clb, zlim=c(20,200))
# UNIFORMIAMO I LIMITI (con zlim)


# BOXPLOT DELLA COPERTURA NEVOSA
# vedi rif. "multitemp.NO2"
# COME VARIA IN MEDIA LA COPERTURA NEVOSA NEGLI ANNI?

boxplot(snow.multitemp.italy, horizontal=T, outline=F)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

##########
## (12) ## SPECIES DISTRIBUTION MODELLING:
########## ANALISI DELLA DISTRIBUZIONE DELLE SPECIE IN UNA DETERMINATA AREA GEOGRAFICA (08/06/20)

install.packages("sdm")
library(sdm)

library(raster)
library(rgdal)

# SPECIES

file <- system.file("external/species.shp", package="sdm") 
# CARICHIAMO LO SHAPEFILE "species" DALL'ESTERNO ALL'INTERNO DEL PACCHETTO "sdm"

species <- shapefile(file)
# FUNZIONE "shapefile": E' DEL PACCHETTO "rgdal" (-> FILES VETTORIALI)
# SERVE PER MAPPARE LA DISTRIBUZIONE DI UNA DETERMINATA SPECIE A TERRA

species
# class       : SpatialPointsDataFrame 
# features    : 200 
# extent      : 110112, 606053, 4013700, 4275600  (xmin, xmax, ymin, ymax)
# crs         : +proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       : Occurrence 
# min values  :          0 
# max values  :          1 

# N.B. ZONE=FUSO -> SPAGNA (FUSO 30)

species$Occurrence
# "species" E' FORMATO DA DEI PUNTI CON COORDINATE E OGNUNO E' LEGATO AL FATTO CHE LA SPECIE SIA STATA VISTA O MENO (->Occurrence)

plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# ALL'INTERNO DI "species": OCCORRENZE = 1 -> SPECIE PRESENTE (PUNTO BLU)
#                           OCCORRENZE = 0 -> SPECIE ASSENTE (PUNTO ROSSO)


# MODEL

path <- system.file("external", package="sdm") 
# IMPORTIAMO LA CARTELLA "external" IN "sdm"


# PREDICTORS

lst <- list.files(path=path,pattern='asc$',full.names = T) #
preds <- stack(lst) 

lst
# [1] "C:/R-3.6.3/library/sdm/external/elevation.asc"    
# [2] "C:/R-3.6.3/library/sdm/external/precipitation.asc"
# [3] "C:/R-3.6.3/library/sdm/external/temperature.asc"  
# [4] "C:/R-3.6.3/library/sdm/external/vegetation.asc"   

# ABBIAMO 4 LAYER ASCII NELLA LISTA -> NE FACCIAMO UN SINGOLO OGGETTO CON "stack" -> "preds"

# PREDITTORI: VARIABILI PER PREVEDERE LA PROBABILE DISTRIBUZIONE DELLA NOSTRA SPECIE

cl <- colorRampPalette(c('blue','orange', 'red', 'yellow'))(100)
plot(preds, col=cl)

# PRENDIAMO SOLO I PUNTI CON OCCORRENZE = 1 -> SPECIE PRESENTE

plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# LOW ELEVATION: LA SPECIE IN QUESTIONE PREDILIGE PIANURA/COLLINA

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# PREDILIGE AMBIENTI CON TEMPERATURE MEDIO-ALTE

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# PRECIPITAZIONI: SITUAZIONE INTERMEDIA

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# LA SPECIE PREFERISCE L'OMBREGGIATURA


# CREIAMO (ATTRAVERSO UN'EQUAZIONE) UN MODELLO LINEARE CHE RACCOLGA TUTTE QUESTE VARIABILI
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

# TRAIN: TUTTI I DATI RACCOLTI A TERRA
# PREDICTORS: VARIABILI (4)
# TILDE "~": serve per indicare "=" NEI MODELLI

# FUNZIONE "sdmData"

m1 <- sdm(Occurrence ~ elevation+precipitation+temperature+vegetation, data=d, methods='glm')

p1 <- predict(m1, newdata=preds)
# FUNZIONE "predict": SERVE PER FARE UNA MAPPA PREVISIONALE DEL MODELLO

plot(p1,col=cl)
points(species[species$Occurrence == 1,], pch=16)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

### EXAM

### Here put your code

