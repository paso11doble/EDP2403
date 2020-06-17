# R CODE FOR MULTITEMPORAL ANALYSIS:
# ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (1) (29/04/20)

# FILE DEFORESTAZIONE FORESTA AMAZZONICA, MATO GROSSO, BR

setwd("C:/LAB/")

library(raster)

# FUNZIONE "brick": DOBBIAMO CARICARE DATI DALL'ESTERNO SU DIVERSI LIVELLI 
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# SELEZIONANDO "defor1": TROVIAMO AL SUO INTERNO 3 BANDE
# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 => NIR
# defor1_.2 => red
# defor1_.3 => green

# PLOT: ASSOCIAMO LE 3 DATE ALLE COMPONENTI "RGB"
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")

# Exercise: plot 2° data
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")

# N.B. CONFRONTANDO LE FOTO, IL FIUME RISULTA DI DUE COLORI DIVERSI:
# L'ACQUA PURA ASSORBE COMPLETAMENTE IR E DIVENTA NERA; QUESTO PERCHE' NEL FIUME CI SONO PIU' SOLIDI DISCIOLTI 

# plot delle 2 foto
par(mfrow=c(2,1))
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")
 

# CLASSIFICAZIONE NON SUPERVISIONATA (di defor1)

# "unsuperClass": E' UNA FUNZIONE DEL PACCHETTO RStoolbox
library(RStoolbox)

d1c <- unsuperClass(defor1, nClasses=2)

# DOPO AVER SELEZIONATO "d1c" SCRIVO "d1c$map": LA MAPPA DEVE ESSERE LEGATA AL DATASET

plot(d1c$map)

cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c$map, col=cl)
# oppure
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c$map, col=cl)


# ESEMPIO SIGNIFICATIVO DEL "$"
# mappageologica <- geomap(im_sat, nClasses=...)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)


# CLASSIFICAZIONE NON SUPERVISIONATA (di defor2)

# Exercise: classificare con 2 classi "defor2" 
d2c <- unsuperClass(defor2, nClasses=2)
plot(d2c$map)
cl <- colorRampPalette(c('black','green'))(100) #
plot(d2c$map, col=cl)
  
# PLOT DELLE 2 MAPPE OTTENUTE
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
# OPPURE
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)


# MAPPA 1 (FREQUENZE E PERCENTUALE)

# "freq": SERVE PER CALCOLARE LA FREQUENZA DEI VALORI
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
# VEDI APPUNTI PREC. OPPURE DOWNLOAD DA IOL

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
# RISPETTIVAMENTE: DATA, AESTETICS + TYPE
# VEDI RIF. APPUNTI "Point Patterns"

# x= COPERTURA (AGRICOLTURA + FORESTA)
# y= % COPERTURA PRE-DEFORESTAZIONE
# identity: RIFERITO ALLA COLORAZIONE (=COPERTURA)

# Exercise: plot the histograms of the land cover AFTER deforestation
ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")


install.packages("gridExtra") 
library(gridExtra)
# N.B. FUNZIONE "par" CON "ggplot" NON FUNZIONA!

# grid.arrange(plot1, plot2, nrow = 1)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + geom_bar(stat="identity", fill="white")

# Exercise: use "grid.arrange" to plot the two graphs
grid.arrange(grafico1, grafico2, nrow = 1)

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (3) (06/05/20)

# REVIEW: VEDI RIF. 05/05/20

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
# IN QUESTO MODO CON "ylim" ABBIAMO UNIFORMATO GLI ESTREMI SULLE COORDINATE DEGLI ISTOGRAMMI

# Exercise: use "grid.arrange" to plot the two graphs 
grid.arrange(grafico1, grafico2, nrow = 1)
