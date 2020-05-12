# R code for analysing NO2 data from ESA - January to March 2020 (Pratica del 05/05/20)

setwd("C:/LAB/")
library(raster)

funzione "raster": la utilizziamo per importare le singole immagini
EN01 <- raster("EN_0001.png")
plot(EN01)

# Exercise: import all the other images 
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

ls()
 [1] "after"              "before"             "cl"                
 [4] "cover"              "d1c"                "d2c"               
 [7] "defor1"             "defor2"             "EN01"              
[10] "EN02"               "EN03"               "EN04"              
[13] "EN05"               "EN06"               "EN07"              
[16] "EN08"               "EN09"               "EN10"              
[19] "EN11"               "EN12"               "EN13"              
[22] "grafico1"           "grafico2"           "meuse"             
[25] "output"             "p1"                 "panel.correlations"
[28] "panel.smoothing"    "percent1"           "percent2"          
[31] "totd1"              "totd2"    

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN01, col=cl)
plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)


# differenza 

difno2 <- EN13 - EN01

cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

# blue: differenze minori NO2 
# yellow: differenze maggiori NO2

# Exercise: plot all the images
(N.B. le immagini sono 13, quindi col "par" dobbiamo selezionare 4 righe e 4 colonne!)
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)

----------------------------------------------------------------------------------------------------------------

# R code for analysing NO2 data from ESA (Pratica del 06/05/20)

setwd("C:/LAB/")
library(raster)
load("multitemp.NO2")              [in alternativa: load("EN.RData")  (download da IOL)]

ls()
 [1] "after"              "before"             "cl"                
 [4] "cldif"              "cover"              "d1c"               
 [7] "d2c"                "defor1"             "defor2"            
[10] "difno2"             "EN01"               "EN02"              
[13] "EN03"               "EN04"               "EN05"              
[16] "EN06"               "EN07"               "EN08"              
[19] "EN09"               "EN10"               "EN11"              
[22] "EN12"               "EN13"               "grafico1"          
[25] "grafico2"           "meuse"              "output"            
[28] "p1"                 "panel.correlations" "panel.smoothing"   
[31] "percent1"           "percent2"           "totd1"             
[34] "totd2"             

funzione "lapply": non viene applicata ad un singolo file (come per "raster") ma formula un "ciclo", apporta assieme diversi dati/vettori

setwd("C:/LAB/esa_NO2")
[creare una cartella all'interno di LAB ("esa_NO2") e importare tutti e 13 i PNG]

funzione "list.files": serve per fare una lista di files con attributi simili (in questo caso PNG)
rlist <- list.files(pattern=".png")

rlist                                                     
 [1] "EN_0001.png" "EN_0002.png" "EN_0003.png" "EN_0004.png" "EN_0005.png"
 [6] "EN_0006.png" "EN_0007.png" "EN_0008.png" "EN_0009.png" "EN_0010.png"
[11] "EN_0011.png" "EN_0012.png" "EN_0013.png"
(in questo modo visualizziamo solo i PNG!)

alla funzione "lapply" associamo "raster" (per le immagini satellitari)
listafinale <- lapply(rlist, raster)

listafinale

funzione "stack": dalle 13 immagini satellitari creiamo un'unica banda
EN <- stack(listafinale)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl)

----------------------------------------------------------------------------------------------------------------

# R code for analysing NO2 data from ESA (Pratica del 12/05/20)
 
setwd("C:/LAB/esa_NO2") 
require(raster)

rlist <- list.files(pattern=".png") 
rlist 

listafinale <- lapply(rlist, raster)
# brick
 
EN <- stack(listafinale)
# stack
 
difEN <- EN$EN_0013 - EN$EN_0001
differenza tra il valore del pixel della 13ma immagine e il valore della 1a
 
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)
 
cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl) 
plot dell'intero set

boxplot(EN)
boxplot(EN,horizontal=T)
boxplot(EN,horizontal=T,axes=T)
boxplot(EN,horizontal=T,axes=T,outline=F)
boxplot(EN,horizontal=T,axes=F,outline=F)

