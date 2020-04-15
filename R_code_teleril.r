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

           
-------------------------------------------------------------------------------------------------
 # PRATICA DEL 15/04/20 
 
 library(raster)

# setwd("C:/LAB/")
           
load("teleril.RData2")
        
# facciamo una lista dei dati salvati la settimana scorsa : ls()
   
# analizziamo i dati relativi al 1988 (l'altra volta erano del 2011), nello specifico dobbiamo importare p224r63_1988_masked.grd 
           (p=path, r=row, "grd" sta per greed, griglia)
# p224r63_1988 <- brick("p224r63_1988_masked.grd")
  
plot(p224r63_1988)
# multiframe
par(mfrow=c(2,2))

# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_1988$B1_sre, col=clb)          
           
 # green
clg <- colorRampPalette(c('dark green','green','light green'))(100) # 
plot(p224r63_1988$B2_sre, col=clg)

# red
clr <- colorRampPalette(c('dark red','red','pink'))(100) # 
plot(p224r63_1988$B3_sre, col=clr)

# nir
clnir <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(p224r63_1988$B4_sre, col=clnir)          
           
dev.off()          
           
#B1: blue - 1
#B2: green - 2
#B3: red - 3
#B4: NIR - 4           
           
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin") (plot della scorsa settimana)
           
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
           
# exercise: plot the image using the nir on the "r" component in the RGB space!           
  plotRGB(p224r63_1988, r=4, g=2, b=1, stretch="Lin")
  OPPURE plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
      
# plot delle 2 immagini (confronto tra 1988 e 2011)
par(mfrow=c(2,1))         
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")     *"main" indica il titolo
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")
 dev.off() 
 
           
           
 # spectral indices   
 # dvi1988 = nir1988 - red1988          
           
 dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre        
              (banda nir)            (banda red)
 plot(dvi1988)
 
 # exercise: calculate dvi for 2011 
 dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre           
 plot(dvi2011)         
           
 cldvi <- colorRampPalette(c('light blue','light green','green'))(100) #           
  plot(dvi2011, col=cldvi)
           
 # multitemporal analysis
 difdvi <- dvi2011 - dvi1988 
 plot(difdvi)
  cldifdvi <- colorRampPalette(c('red','white','blue'))(100) #  
   plot(difdvi, col=cldifdvi)

# visualize the output
# multiframe 1988rgb, 2011rgb, difdiv
par(mfrow=c(3,1))         
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")       
plot(difdvi, col=cldifdvi)
           
    col passare del tempo gran parte dell'area è stata destinata a scopi agricoli
           
# changing the grain (cambiamo la risoluzione delle immagini)           
 p224r63_2011lr <- aggregate(p224r63_2011, fact=10)          
 p224r63_2011   (digitando il nome dell'immagine possiamo vederne le caratteristiche e soprattutto come cambia la risoluzione)       
 p224r63_2011lr  (da 30x30 è passata a 300x300) 

par(mfrow=c(2,1))         
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") 
 

# lower resolution
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50) 
p224r63_2011lr50
# original 30m -> resampled 1500m
par(mfrow=c(3,1))         
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")          
 
# dvi2011 low resolution
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre 
plot(dvi2011lr50)

# dvi1988 low resolution
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50) 
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# difdvi low resolution
# difdvilr50 <- dvi2011lr50 - dvi1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) #  
plot(difdvilr50, cl=cldifdvi)

# multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
