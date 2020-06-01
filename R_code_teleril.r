# R CODE FOR SATELLITE IMAGES ANALYSIS:
# ANALISI DI TELERILEVAMENTO (1) (07/04/20)

install.packages("raster")
library(raster)

setwd("C:/LAB/")

p224r63 <- brick("p224r63_2011_masked.grd")  
# funzione "brick": permette di caricare più dati dall'esterno e di associarli ad un'unica immagine
# es. per immagini satellitari (ortofoto)

p224r63_2011 <- brick("p224r63_2011_masked.grd")
# (p=path; r=row; grd=greed, "griglia")

plot(p224r63_2011)

# selezionando "p224r63_2011" e osservando la voce "names" si possono notare 7 bande:
#Banda 1: blue
#Banda 2: green
#Banda 3: red
#Banda 4: near infrared (NIR)
#Banda 5: medium infrared
#Banda 6: thermal infrared
#Banda 7: medium infrared

-----------------------------------------------------------------------------------------------------------------------

# ANALISI DI TELERILEVAMENTO (2) (08/04/20)

setwd("C:/LAB/")

load("teleril.RData")

ls()
# [1] "meuse"              "p224r63"            "p224r63_2011"      
# [4] "panel.correlations" "panel.smoothing"   

library(raster)
plot(p224r63_2011)

# grey scale
cl <- colorRampPalette(c('black','grey','light grey'))(100) #
plot(p224r63_2011,col=cl)

# grey scale, lower amount of colours
cllow <- colorRampPalette(c('black','grey','light grey'))(5) #
plot(p224r63_2011,col=cllow)

names(p224r63_2011)
# [1] "B1_sre" "B2_sre" "B3_sre" "B4_sre" "B5_sre" "B6_bt"  "B7_sre"

clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # 
plot(p224r63_2011$B1_sre, col=clb)
# attach(dataframe) non funziona con il pacchetto "raster"
# con "$" allego la colonna (in questo caso Banda 1) all'immagine satellitare (dataset) 
# "sre" è il sensore

# Exercise: plottare la banda del NIR con colorRampPalette dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(p224r63_2011$B4_sre, col=clnir)


# MULTIFRAME 

# plot di bande di colori diversi
par(mfrow=c(2,2))

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

dev.off()
# fa sparire l'ultimo plot creato


# NATURAL COLOURS MULTIFRAME 

# plot dell'immagine a colori in base a come li vedrebbe l'occhio umano
# 3 componenti: R-G-B
# 3 bande: R = banda del rosso, G = b. del verde, B = b. del blu
# N.B. "RGB" in Maiuscolo, R è case sensitive!
plotRGB(p224r63_2011, r=3, g=2, b=1) 

#B1: blue => 1
#B2: green => 2
#B3: red => 3
#B4: NIR => 4

# immagine a colori naturali
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# NIR
# immagine a colori falsati (scaliamo i colori di 1, inseriamo la Banda NIR)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


# HOW TO SAVE AN IMAGE (PDF, PNG,...)

pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()
         
par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# NIR nella componente red (vedi es. sopra)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# Exercise: NIR nella componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")

# Exercise: NIR nella componente blue
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")

-----------------------------------------------------------------------------------------------------------------------

# ANALISI DI TELERILEVAMENTO (3) (15/04/20)         
 
library(raster)

setwd("C:/LAB/")
           
load("teleril.RData2")

ls()
# [1] "cl"                 "clb"                "clg"               
# [4] "cllow"              "clnir"              "clr"               
# [7] "meuse"              "p224r63"            "p224r63_2011"      
# [10] "panel.correlations" "panel.smoothing"   

# dobbiamo importare "p224r63_1988_masked.grd"    
# analizziamo i dati relativi al 1988 (dati 08/04 relativi al 2011)

p224r63_1988 <- brick("p224r63_1988_masked.grd")
plot(p224r63_1988)


# MULTIFRAME

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

# NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(p224r63_1988$B4_sre, col=clnir)          
           
dev.off()          

#B1: blue => 1
#B2: green => 2
#B3: red => 3
#B4: NIR => 4       
      
           
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")
           
# Exercise: plot the image using NIR on the "r" component in the RGB space (2 possibilità)          
plotRGB(p224r63_1988, r=4, g=2, b=1, stretch="Lin")
# oppure
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")


# COMPARISON BETWEEN THE IMAGES 
# confronto 1988-2011

par(mfrow=c(2,1))         
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin", main="1988")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin", main="2011")

dev.off() 
 
               
# SPECTRAL INDICES 

# Differenza 1988: Banda NIR - Banda red
# dvi1988 = nir1988 - red1988          

dvi1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre        
plot(dvi1988)
 
# Exercise: calculate the same dvi for 2011 
dvi2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre           
plot(dvi2011)         
           
cldvi <- colorRampPalette(c('light blue','light green','green'))(100) #           
plot(dvi2011, col=cldvi)


# MULTITEMPORAL ANALYSIS

difdvi <- dvi2011 - dvi1988 
plot(difdvi)

cldifdvi <- colorRampPalette(c('red','white','blue'))(100) #  
plot(difdvi, col=cldifdvi)


# VISUALIZE THE OUTPUT
# MULTIFRAME: 1988rgb, 2011rgb, difdiv

par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")       
plot(difdvi, col=cldifdvi)
           
# col passare del tempo gran parte dell'area è stata destinata a scopi agricoli


# CHANGING THE GRAIN
# cambio della risoluzione delle immagini

p224r63_2011lr <- aggregate(p224r63_2011, fact=10) 

p224r63_2011   
# digitando il nome dell'immagine possiamo vederne le caratteristiche; come cambia la risoluzione?       
p224r63_2011lr  
# la risoluzione da 30x30m è passata a 300x300m

par(mfrow=c(2,1))         
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") 
 
# LOWER RESOLUTION

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50) 
p224r63_2011lr50
# la risoluzione da 30x30m è passata a 1500x1500m

par(mfrow=c(3,1))  
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")          


# dvi2011 LOW RESOLUTION
dvi2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre 
plot(dvi2011lr50)

# dvi1988 LOW RESOLUTION
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50) 
dvi1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre
plot(dvi1988lr50)

# difdvi LOW RESOLUTION
difdvilr50 <- dvi2011lr50 - dvi1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100) #  
plot(difdvilr50, cl=cldifdvi)

# MULTIFRAME: difdvi, difdvi LR
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
