# R CODE LAND COVER (pratica del 21/04/20)

setwd("C:/LAB")

library(raster)
library(RStoolbox) (già installata)

p224r63_2011 <- brick("p224r63_2011_masked.grd")

#rgb
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

unsuperClass() (con questo comando specifico immagine e n° classi) N.B. Classe in maiuscolo!
                i pixel vengono accorpati nelle varie classi o cluster
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c (con questo comando mi escono i dati dell'immagine)

plot(p224r63_2011c$map) (per plottare la mappa)

clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
                         (per stabilire una legenda: ad ogni classe, 4, viene associato un colore
                          i colori delle classi si notano nella scala sulla destra dell'immagine)
                          
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2) (stesso processo con 2 classi)
                         SENSITIVITA' dell'algoritmo all'aumentare del numero delle classi, all'aumentare dell'INCERTEZZA
                         
# IN FUNZIONE DEL N° CLASSI AUMENTA L'INCERTEZZA DELL'ALGORITMO AUTOMATICO DI CLASSIFICAZIONE
# RIPORTANDO POTENZIALMENTE CLASSI LEGGERMENTE DIFFERENTI
                         

