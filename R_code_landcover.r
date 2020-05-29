# R CODE FOR LAND COVER: ANALISI DELLA COPERTURA DEL SUOLO (21/04/20)

setwd("C:/LAB")

library(raster)
library(RStoolbox)

p224r63_2011 <- brick("p224r63_2011_masked.grd")


# PLOT RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# unsuperClass()
# con questo comando, esclusivo di RStoolbox, devo specificare immagine e n° classi (in Maiuscolo!)
# i pixel vengono accorpati nelle varie classi o cluster
               
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c
# in questo modo vengono elaborati i dati dell'immagine

plot(p224r63_2011c$map)
# faccio un plot della mappa col "$"

clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
# Legenda: ad ogni classe, 4 in tutto, viene associato un colore
# i vari colori si notano nella scala sulla destra dell'immagine
                          
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2) 
# stesso processo con 2 classi
  

# CONSIDERAZIONI FINALI

# RIPORTANDO POTENZIALMENTE CLASSI DIFFERENTI                      
# L'INCERTEZZA DELL'ALGORITMO AUTOMATICO DI CLASSIFICAZIONE AUMENTA IN FUNZIONE DEL N° CLASSI: 
# PER QUESTO SI PARLA DI SENSITIVITA' DELL'ALGORITMO
