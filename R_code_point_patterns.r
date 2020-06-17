# R CODE FOR POINT PATTERN ANALYSIS: 
# PATTERN LEGATI AI PUNTI (1) (31/03/20)

install.packages("ggplot2") / require(ggplot2)
install.packages("spatstat") / require(spatstat)

setwd("C:/LAB/")

# IMPORTAZIONE DI DATI DALL'ESTERNO

covid <- read.table("covid_agg.csv", head=T)

head(covid)

plot(covid$country, covid$cases)
# attach(covid)
# plot(country, cases)

# FUNZIONE "las": SERVE PER CAMBIARE DISPOSIZIONE DELLE ETICHETTE
plot(covid$country, covid$cases, las=0)  # parallele agli assi
plot(covid$country, covid$cases, las=1)  # orizzontali
plot(covid$country, covid$cases, las=2)  # perpendicolari
plot(covid$country, covid$cases, las=3)  # verticali

plot(covid$country, covid$cases, las=3)  # vertical labels

# FUNZIONE "cex.axis": SERVE PER DIMINUIRE DIMENSIONE DELLE ETICHETTE
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5)  # vertical labels


# ggplot2
data(mpg)
head(mpg)

ggplot(mpg, aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()
# 1) DATA (ES. CON FILE "mpg")
# 2) AESTETICS (VARIABILI CHE COMPONGONO L'ESTETICA DEL GRAFICO)
# 3) TYPE (DEFINISCE LA GEOMETRIA, INDICATA SEMPRE DOPO IL +)

# ggplot di covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()
# SI TRATTA DI UNA MEDIA DATI DI OGNI PAESE, DI CUI SI PRENDE COME RIFERIMENTO IL CENTROIDE


# density
# create dataset for spatstat

attach(covid)

covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))
# ppp(x.coordinates, y.coordinates, x.range, y.range)

d <- density(covids)

plot(d)

points(covids)
# FUNZIONE PER INSERIRE I CENTROIDI
# PRIMA PERO' OCCORRE SELEZIONARE "plot(covids)"!
# LA MAPPA E' RELATIVA AI DATI COVID DI FEBBRAIO

-----------------------------------------------------------------------------------------------------------------------

# PATTERN LEGATI AI PUNTI (2) (01/04/20)

# Save the "RData" (IN "LAB")

setwd("C:/LAB/")                                                              

load("point_pattern.RData") 

ls()  
# SPECIFICA COSA CONTIENE IL FILE


plot(d)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d, col=cl)
# "Palette": E' UNA GAMMA DI COLORI SCELTA PER RAPPRESENTARE UNA MAPPA

# Exercise: plot della mappa della densità dal verde al blu
cl <- colorRampPalette(c('green', 'light blue', 'blue')) (100)
plot(d, col=cl)

points(covids)


coastlines <- readOGR("ne_10m_coastline.shp") 
# N.B. DEVO UTILIZZARE UNA FUNZIONE DELLA LIBRERIA "rgdal", PER LEGGERE FILES VETTORIALI
# PRIMA DI DARE IL COMANDO "readOGR" DEVO SELEZIONARE: install.packages("rgdal") / library(rgdal)

plot(coastlines, add=T)
# IN QUESTO MODO INSERIAMO LE COASTLINES E LE AGGIUNGIAMO AL PLOT PRECEDENTE


# Exercise: plot della mappa della densità con una nuova colorazione; aggiungere poi le coastlines
cl <- colorRampPalette(c('blue', 'light blue', 'light green', 'yellow')) (100)
plot(d, col=cl)
plot(coastlines, add=T, col="green")

# OVVIAMENTE POSSO FARE IN VARI MODI SCEGLIENDO TRA VARI COLORI E GAMME (INDICATE NELLA SECONDA PARENTESI)

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

# PATTERN LEGATI AI PUNTI (3) (22/04/20)

# Exercise: caricare il workspace "point_pattern.RData" (con "load") e creare un grafico di densità 

library(spatstat)
library(rgdal)  # per le coastlines

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
# CON "covid" / "view(covid)" POSSO VEDERE TUTTA LA TABELLA

marks(covids) <- covid$cases

s <- Smooth(covids)
plot(s)
# FUNZIONE "Smooth": SERVE PER CREARE UNA MAPPA "CONTINUA", LINEARE DEI CASI

# Exercise: plot(s) con points e coastlines
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

text(covids)


# MAPPA FINALE (con plot(d) e plot(s) sovrapposti)

par(mfrow=c(2,1))

# DENSITA'
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(d, col=cl5, main="density")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# INTERPOLAZIONE N° CASI
cl5 <- colorRampPalette(c('cyan', 'purple', 'red')) (200)
plot(s, col=cl5, main="estimate of cases")
points(covids)
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

-----------------------------------------------------------------------------------------------------------------------

# PATTERN LEGATI AI PUNTI (4) (28/04/20)
# ESERCIZIO SAN MARINO 

setwd("C:/LAB/")
library(spatstat)

load("Tesi.RData")
ls()
head(Tesi)
attach(Tesi)

summary(Tesi)
# x (Longitude): varia da 12.42 a 12.46
# y (Latitude): varia da 43.91 a 43.94

# PER CREARE IL POINT PATTERN: x, y, c(xmin, xmax), c(ymin, ymax)
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95)) 
# abbiamo esteso un pò i margini...

# PER LA DENSITA': USARE LA FUNZIONE "density"
dT <- density(Tesippp)
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

points(Tesippp, col="green")
head(Tesi)

marks(Tesippp) <- Tesi$Species_richness
# FUNZIONE "marks": ASSOCIA I VALORI DELLA VARIABILE CHE VOGLIAMO INTERPOLARE (es. Species richness) AL POINT PATTERN
# (QUINDI 48 E 43 PER MONTALBO1 E MONTALBO2)

# PER VEDERE LE EVENTUALI VARIAZIONI: "Smooth"
interpol <- Smooth(Tesippp)

plot(interpol)

points(Tesippp, col="green")
# PRIMA PERO' OCCORRE SELEZIONARE "plot(Tesippp)"!


library(rgdal)
sanmarino <-  readOGR("San_Marino.shp")

plot(sanmarino)
plot(interpol, add=T)
# AGGIUNGO LA MAPPA "interpol" ALLA PRECEDENTE

points(Tesippp, col="green") 
plot(sanmarino, add=T)
# COSI' SOVRAPPONGO I CONFINI RSM ALLE MAPPE PRECEDENTI


# Exercise: plot multiframe (2 righe, 1 colonna) di densità e interpolazione (con titolo, quindi main="...")
par(mfrow=c(2,1))   # sovrapposti
plot(dT, main="Density of points")
points(Tesippp, col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="green")

# Exercise: plot multiframe (2 colonne, 1 riga) di densità e interpolazione
par(mfrow=c(1,2))   # affiancati
plot(dT, main="Density of points")
points(Tesippp, col="green")
plot(interpol, main="Estimate of species richness")
points(Tesippp, col="green")
