# R CODE FOR MULTITEMPORAL ANALYSIS:
# ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (1) (29/04/20)

# file deforestazione Foresta Amazzonica, Mato Grosso, BR

setwd("C:/LAB/")

library(raster)

# funzione "brick": dobbiamo caricare dati dall'esterno su più livelli 
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# selezionando "defor1": troviamo al suo interno 3 bande
# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 => NIR
# defor1_.2 => red
# defor1_.3 => green

# plot: associamo le 3 bande alle componenti RGB
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")

# Exercise: plot 2° data
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")

# N.B. confrontando le 2 foto, il fiume risulta di due colori diversi:
# l'acqua pura assorbe completamente IR e diventa nera; questo perché nel fiume ci sono più solidi disciolti 

# plot delle 2 foto
par(mfrow=c(2,1))
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")
 

# CLASSIFICAZIONE NON SUPERVISIONATA (di defor1)

# funzione "unsuperClass": è una funzione del pacchetto RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2)

# dopo aver selezionato "d1c" faccio "d1c$map": la mappa deve essere legata al dataset

plot(d1c$map)

cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c$map, col=cl)
# oppure
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c$map, col=cl)


# esempio significativo del "$"
# mappageologica <- geomap(im_sat, nClasses=...)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)


# CLASSIFICAZIONE NON SUPERVISIONATA (di defor2)

# Exercise: classificare con 2 classi "defor2" 
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) #
plot(d2c$map, col=cl)
  
# plot delle 2 mappe ottenute
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
# oppure
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)


# MAPPA 1 (FREQUENZE E PERCENTUALE)

# funzione "freq": serve per calcolare la frequenza dei valori
freq(d1c$map)

#      value  count
# [1,]     1  33817  aree aperte
# [2,]     2 307475  foresta

totd1 <- 33817 + 307475

totd1
# [1] 341292

percent1 <- freq(d1c$map)  * 100/totd1

percent1
#             value     count
# [1,] 0.0002930042  9.908524  aree aperte
# [2,] 0.0005860085 90.091476  foresta

# aree aperte: 9.9
# foresta: 90.1


# MAPPA 2 (FREQUENZE E PERCENTUALE)

freq(d2c$map)

#      value  count
# [1,]     1 163569 aree aperte
# [2,]     2 179157 foresta

totd2 <- 163569 + 179157

totd2
# [1] 342726

percent2 <- freq(d2c$map)  * 100/totd2

percent2
#             value    count
# [1,] 0.0002917783 47.72588 aree aperte
# [2,] 0.0005835565 52.27412 foresta

# aree aperte: 47.7
# foresta: 52.3


cover <- c("Agriculture","Forest")
before <- c(9.9, 90.1)
after <- c(47.7, 52.3)

output <- data.frame(cover,before,after)
View(output)

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (2) (05/05/20)

setwd("C:/LAB/")

load("defor.RData")        
# vedi appunti prec. oppure download da IOL

ls()

library(raster)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

output
#         cover before after
# 1 Agriculture   10.9  48.2
# 2      Forest   89.1  51.8


library(ggplot2)

ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
# rispettivamente: data, aestetics + type
# vedi rif. appunti "Point Patterns"

# x= copertura (agricoltura + foresta)
# y= % copertura prima della deforestazione
# identity: il riferimento è la colorazione (=copertura)

# Exercise: plot the histograms of the land cover AFTER deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")


install.packages("gridExtra") 
library(gridExtra)
# N.B. funzione "par" con ggplot non funziona!

# grid.arrange(plot1, plot2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

# Exercise: use "grid.arrange" to plot the two graphs
grid.arrange(grafico1, grafico2, nrow = 1)

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (3) (06/05/20)
# REVIEW: vedi rif. 05/05/20

setwd("C:/LAB/")
require(raster)
require(ggplot2)
require(gridExtra)

load("defor.RData")
cover <- c("Agriculture","Forest")
before <- c(9.9, 90.1)
after <- c(47.7, 52.3)

output <- data.frame(cover,before,after)

output
#         cover before after
# 1 Agriculture   10.9  48.2
# 2      Forest   89.1  51.8

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")
  
grid.arrange(grafico1, grafico2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white") + ylim(0, 100)
# in questo modo con "ylim" abbiamo uniformato gli estremi sulle ordinate degli istogrammi

# Exercise: use "grid.arrange" to plot the two graphs 
grid.arrange(grafico1, grafico2, nrow = 1)
