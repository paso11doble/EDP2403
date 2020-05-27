# CODICI PER ANALISI DEI POINT PATTERNS (PATTERN LEGATI AI PUNTI) (31/03/20)

install.packages("ggplot2")    
install.packages("spatstat")
# se il pacchetto fosse già presente: library(ggplot2) oppure require(ggplot2); lo stesso per spatstat

setwd("C:/LAB/")

# IMPORTAZIONE DI DATI DALL'ESTERNO

covid <- read.table("covid_agg.csv", head=T)

head(covid)

plot(covid$country, covid$cases)
# attach(covid)
# plot(country, cases)

# funzione "las": serve per cambiare la disposizione delle etichette
plot(covid$country, covid$cases, las=0)  # parallele agli assi
plot(covid$country, covid$cases, las=1)  # orizzontali
plot(covid$country, covid$cases, las=2)  # perpendicolari
plot(covid$country, covid$cases, las=3)  # verticali

plot(covid$country, covid$cases, las=3)  # vertical labels

# funzione "cex.axis": serve per diminuire la dimensione delle etichette
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5)  # vertical labels


# ggplot2
data(mpg)
head(mpg)

ggplot(mpg, aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()
# 1) data (il file da utilizzare è "mpg")
# 2) aestetics (variabili che compongono l'estetica del grafico)
# 3) type (definisce la geometria, indicata dopo il +)

# ggplot di covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()
# si tratta di una media dati di ogni Paese, di cui si prende come riferimento il centroide


# density
# create dataset for spatstat

attach(covid)

covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

ppp(x.coordinates, y.coordinates, x.range, y.range)

d <- density(covids)

plot(d)

points(covids)
# funzione per inserire i centroidi

-----------------------------------------------------------------------------------------------------------------------

# Save the "RData" (file salvato in "LAB") (01/04/20)

setwd("C:/LAB/")                                                              

load("point_pattern.RData") 

ls()  
# funzione per vedere cosa contiene il file


plot(d)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d, col=cl)
# "Palette" è una gamma di colori scelti per rappresentare una mappa

# Exercise: plot della mappa della densità dal verde al blu
cl <- colorRampPalette(c('green', 'light blue', 'blue')) (100)
plot(d, col=cl)

points(covids)


coastlines <- readOGR("ne_10m_coastline.shp") 
# N.B. devo utilizzare una funzione della libreria "rgdal", per leggere files vettoriali
# quindi prima di dare il comando "readOGR" devo fare: install.packages("rgdal") oppure library(rgdal)

plot(coastlines, add=T)
# in questo modo inseriamo le coastlines e le aggiungiamo al plot precedente

points(covids)
#  mappa è relativa ai dati Covid di febbraio!

# Exercise: plot della mappa della densità con una nuova colorazione e aggiunta delle coastlines
cl <- colorRampPalette(c('blue', 'light blue', 'light green', 'yellow')) (100)
plot(d, col=cl)
plot(coastlines, add=T, col="green")

N.B. posso fare in vari modi cambiando colori e gamma di colori!

cl2 <- colorRampPalette(c('red', 'orange', 'yellow', 'green', 'blue')) (800)
plot(d, col=cl2)
plot(coastlines, add=T)

cl3 <- colorRampPalette(c('blue', 'violet', 'green')) (200)
plot(d, col=cl3)
plot(coastlines, add=T)

cl4 <- colorRampPalette(c('violet', 'yellow', 'green')) (100)
plot(d, col=cl4)
plot(coastlines, add=T)

cl5 <- colorRampPalette(c('darkcyan', 'purple', 'red')) (200)
plot(d, col=cl5)
plot(coastlines, add=T)

cl6 <- colorRampPalette(c('white', 'light blue', 'darkcyan', 'red', 'orange', 'yellow')) (150)
plot(d, col=cl6)
plot(coastlines, add=T)


-----------------------------------------------------------------------------------------------------------------------

(22/04/20)

# Exercise: caricare il workspace point_pattern.RData [load("...")] e creare un grafico di densità

library(spatstat)
library(rgdal) # per coastlines

setwd("C:/LAB/")
load("point_pattern.RData")
ls()
plot(d)

cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(d, col=cl5)
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)



# ANALISI D'INTERPOLAZIONE

head(covid)
con "covid" o "view(covid)" posso vedere tutta la tabella

marks(covids) <- covid$cases

funzione "smooth" serve per creare una mappa "continua" dei casi
s <- Smooth(covids)
plot(s)

# Exercise: plot(s) with points and coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

text(covids)

# MAPPA FINALE (con plot(d) e plot(s) sovrapposti)
par(mfrow=c(2,1))
# densità
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
# interpolazione del numero di casi
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)


-----------------------------------------------------------------------------------------------------------------------

# Esercizio San Marino (28/04/20)
setwd("C:/LAB/")
library(spatstat)

load("Tesi.RData")
ls()
head(Tesi)
attach(Tesi)

summary(Tesi)
# x varia da 12.42 a 12.46
# y varia da 43.91 a 43.94

# per creare il point pattern: x, y, c(xmin, xmax), c(ymin, ymax)
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95)) (abbiamo aumentato un pò i margini)

# per la densità: dT <- density(Tesippp)
plot(dT)
points(Tesippp, col="green")
colors()



setwd("C:/LAB/")
library(spatstat)

load("sanmarino.RData")
ls()

# dT= density map
# Tesi= original dataset
# Tesippp= point pattern
# meuse
# panel.correlations
# panel.smoothing

plot(dT)
points(Tesippp, col="green")
head(Tesi)

marks(Tesippp) <- Tesi$Species_richness
funzione "marks" (vedi sopra): associa i valori della variabile che vogliamo interpolare (es. species richness) al point pattern
(quindi 48 e 43 per Montalbo1 e Montalbo2)

funzione "Smooth": serve per per vedere le eventuali variazioni
interpol <- Smooth(Tesippp)

plot(interpol)

points(Tesippp, col="green")


library(rgdal)
sanmarino <-  readOGR("San_Marino.shp")

plot(sanmarino)
plot(interpol, add=T): aggiungo questa mappa alla precedente

points(Tesippp, col="green") 
plot(sanmarino, add=T): sovrappongo i confini RSM alle mappe precedenti


# Exercise: plot multiframe (2 righe, 1 colonna) di densità e interpolazione (con titolo, indicato con "main")
par(mfrow=c(2,1))
plot(dT, main="Density of points")
points(Tesippp, col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="green")

# Exercise: plot multiframe (2 colonne, 1 riga) di densità e interpolazione
basta invertire i numeri del par:
par(mfrow=c(1,2))
plot(dT, main="Density of points")
points(Tesippp, col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="green")
