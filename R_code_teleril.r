# Codice R per analisi di immagini satellitari (pratica del 07/04/20)

# packages: raster (richiamiamo la cartella raster)

install.packages("raster")
library(raster)

# setwd("C:/LAB/")

p224r63 <- brick("p224r63_2011_masked.grd")  La funzione "brick" permette di caricare dati dall'esterno e di associarli ad una certa immagine

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)
