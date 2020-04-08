# Codice R per analisi di immagini satellitari (pratica del 07/04/20)

# packages: raster (richiamiamo la cartella raster)

install.packages("raster")
library(raster)

# setwd("C:/LAB/")

p224r63 <- brick("p224r63_2011_masked.grd")  La funzione "brick" permette di caricare dati dall'esterno e di associarli ad una certa immagine

p224r63_2011 <- brick("p224r63_2011_masked.grd")

plot(p224r63_2011)

#Banda 1: blue
#Banda 2: green
#Banda 3: red
#Banda 4: near infrared (NIR)
#Banda 5: medium infrared
#Banda 6: thermal infrared
#Banda 7: medium infrared

# save RData (pratica dell'08/04/20)

# setwd("C:/LAB/")

load("teleril.RData")

ls()

#1 [] ...

library(raster)
plot(p224r63_2011)

cl <- colorRampPalette(c('black','grey','light grey'))(100) #
plot(p224r63_2011,col=cl)

# grey scale low amount of colours
cllow <- colorRampPalette(c('black','grey','light grey'))(5) #
plot(p224r63_2011,col=cllow)

names(p224r63_2011)
#1 [] ...

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_2011$B1_sre, col=clb)
# attch(dataframe) non funziona con il pacchetto raster
# simbolo che lega la colonna (la banda) al dataset (immagine satellitare): 

# Exercise: plottare la banda dell'infrarosso vicino (NIR) con
# colorRampPalette che varia dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(p224r63_2011$B4_sre, col=clnir)



# multiframe (andiamo a plottare bande di colori diversi) (* 'SRE' è il sensore)
par(mfrow=c(2,2)) (i 2 indicano le righe e le colonne)

# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_2011$B1_sre, col=clb)

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100) # 
plot(p224r63_2011$B2_sre, col=clg)

# red
clr <- colorRampPalette(c('dark red','red','pink'))(100) # 
plot(p224r63_2011$B3_sre, col=clr)

# NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(p224r63_2011$B4_sre, col=clnir)

dev.off() (così sparisce l'immagine che abbiamo appena creato)

# natural colours (plottaggi dell'immagine come li vedrebbe l'occhio umano)
# 3 componenti: R G B
# 3 bande: R = banda del rosso, G = b. del verde, B = b. del blu
#
# plotRGB(p224r63_2011, r=3, g=2, b=1) (N.B. NON scrivere plotrgb perchè R è case sensitive e distingue Max e min!)

#B1: blue - 1
#B2: green - 2
#B3: red - 3
#B4: NIR - 4

# immagine a colori naturali
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# nir
# immagine a colori falsati (facciamo scalare i colori di 1, inseriamo la componente IR)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# come salvare un'immagine (in pdf, png...) nella cartella di riferimento
pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()
         
par(mfrow=c(1,2)) (quindi 1 riga e 2 colonne)                     [al contrario: par(mfrow=c(2,1)) (quindi 1 colonna e 2 righe)]
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# nir nella componente red
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Exercise: nir nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
# Exercise: nir nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

