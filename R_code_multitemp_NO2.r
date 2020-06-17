# MULTITEMPORAL ANALYSIS: R CODE FOR ANALYSING NO2 DATA FROM ESA - JANUARY TO MARCH 2020 
# ANALISI MULTITEMPORALE DI DATI CONTINUI (1) (05/05/20)

setwd("C:/LAB/")

library(raster)

# FUNZIONE "raster": SI USA PER IMPORTARE SINGOLE IMMAGINI SATELLITARI
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
#  [1] "after"              "before"             "cl"                
#  [4] "cover"              "d1c"                "d2c"               
#  [7] "defor1"             "defor2"             "EN01"              
# [10] "EN02"               "EN03"               "EN04"              
# [13] "EN05"               "EN06"               "EN07"              
# [16] "EN08"               "EN09"               "EN10"              
# [19] "EN11"               "EN12"               "EN13"              
# [22] "grafico1"           "grafico2"           "meuse"             
# [25] "output"             "p1"                 "panel.correlations"
# [28] "panel.smoothing"    "percent1"           "percent2"          
# [31] "totd1"              "totd2"    

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN01, col=cl)
plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)


# MULTITEMPORAL ANALYSIS
# DIFFERENCE: LAST IMAGE - FIRST IMAGE

difno2 <- EN13 - EN01

cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

# BLUE: DIFFERENZE MINORI NO2 
# YELLOW: DIFFERENZE MAGGIORI NO2

# Exercise: plot all the images
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
# N.B. LE IMMAGINI SONO 13, QUINDI CON "par" DOBBIAMO SELEZIONARE 4 RIGHE E 4 COLONNE!

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI DATI CONTINUI (2) (06/05/20)

setwd("C:/LAB/")

library(raster)

load("multitemp.NO2") 
# OPPURE DOWNLOAD DA IOL: load("EN.RData")

ls()
#  [1] "after"              "before"             "cl"                
#  [4] "cldif"              "cover"              "d1c"               
#  [7] "d2c"                "defor1"             "defor2"            
# [10] "difno2"             "EN01"               "EN02"              
# [13] "EN03"               "EN04"               "EN05"              
# [16] "EN06"               "EN07"               "EN08"              
# [19] "EN09"               "EN10"               "EN11"              
# [22] "EN12"               "EN13"               "grafico1"          
# [25] "grafico2"           "meuse"              "output"            
# [28] "p1"                 "panel.correlations" "panel.smoothing"   
# [31] "percent1"           "percent2"           "totd1"             
# [34] "totd2"             

# FUNZIONE "lapply": NON VIENE APPLICATA AD UN SINGOLO FILE MA FORMULA UN "CICLO"; 
# IN ALTRE PAROLE UNA QUALSIASI FUNZIONE VIENE APPLICATA AD UNA LISTA

setwd("C:/LAB/esa_NO2")
# CREARE UNA NUOVA CARTELLA ALL'INTERNO DI "LAB" ("esa_NO2") ED IMPORTARVI TUTTI I 13 PNG

# FUNZIONE "list.files": SERVE PER FARE UNA LISTA DI FILES CON ATTRIBUTI SIMILI (ES. PNG)
rlist <- list.files(pattern=".png")

rlist                                                     
#  [1] "EN_0001.png" "EN_0002.png" "EN_0003.png" "EN_0004.png" "EN_0005.png"
#  [6] "EN_0006.png" "EN_0007.png" "EN_0008.png" "EN_0009.png" "EN_0010.png"
# [11] "EN_0011.png" "EN_0012.png" "EN_0013.png"

# VISUALIZZIAMO SOLO I PNG


# A "lapply" ASSOCIAMO "raster" (-> SINGOLE IMMAGINI)
listafinale <- lapply(rlist, raster)

listafinale


# FUNZIONE "stack": DALLE IMMAGINI SATELLITARI CREA UN'UNICA BANDA
EN <- stack(listafinale)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl)

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI DATI CONTINUI (3) (12/05/20)
 
setwd("C:/LAB/esa_NO2") 

require(raster)

rlist <- list.files(pattern=".png") 
rlist 

listafinale <- lapply(rlist, raster)
# brick
 
EN <- stack(listafinale)
# stack
 
difEN <- EN$EN_0013 - EN$EN_0001
# DIFFERENZA TRA IL VALORE DEL PIXEL DELLA 13ma IMMAGINE E IL VALORE DELLA 1a
 
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)


cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl) 
# PLOT INTERO SET

boxplot(EN)
boxplot(EN,horizontal=T)
boxplot(EN,horizontal=T,axes=T)
boxplot(EN,horizontal=T,axes=T,outline=F)
boxplot(EN,horizontal=T,axes=F,outline=F)
