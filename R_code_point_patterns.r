# CODICI PER ANALISI DEI POINT PATTERNS (pattern legati ai punti)

install.packages("ggplot2") oppure library(ggplot2) [comando alternativo a library: require]
install.packages("spatstat")

# setwd("C:/lab/") per Windows

# importare dati
covid <- read.table("covid_agg.csv", head=T)

head(covid)

plot(covid$country, covid$cases)
il dollaro collega una colonna al dataset
# attach(covid)
# plot(country, cases)

plot(covid$country, covid$cases, las=0) #parallele agli assi
plot(covid$country, covid$cases, las=1) #orizzontali
plot(covid$country, covid$cases, las=2) #perpendicolari
plot(covid$country, covid$cases, las=3) #verticali
las serve per cambiare la disposizione delle etichette

plot(covid$country, covid$cases, las=3) #vertical labels

come diminuire la dimensione delle etichette (usare funzione cex.axis)
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5) #vertical labels

# ggplot2
data(mpg)
head(mpg)

# data (il file da utilizzare è mpg)
# aestetics (variabili che compongono l'estetica del grafico)
# type (geometria, indicata dopo il +!)
ggplot(mpg, aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()
media dati di ogni Paese, di cui si prende come riferimento il centroide

# density
# create dataset for spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

> ppp(x.coordinates, y.coordinates, x.range, y.range)

d <- density(covids)

plot(d)
points(covids)

--------------------------------------------------------------------------------------------------------------

# Save the .RData (file salvato nella cartella LAB del disco locale (C:))

setwd("C:/LAB/")
load("point_pattern.RData") (funzione per inserire l'RData di ieri)

ls()

plot(d)
# palette (colori per rappresentare la mappa)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d, col=cl)

# Exercise: plot della mappa della densità dal verde al blu


coastlines <- readOGR("ne_10m_coastline.shp") (devo utilizzare una funzione della libreria RGDAL, per leggere files vettoriali)

quindi prima di dare il comando readOGR devo fare:
install.packages("rgdal")
library(rgdal)

plot(coastlines, add=T) [inseriamo le coastlines e le aggiungiamo al plot precedente]

points(covids) (funzione per inserire i centroidi di ieri)

N.B. la mappa è relativa ai dati Covid di febbraio!

# Exercise: plot della mappa della densità con una nuova colorazione e aggiunta delle coastlines
cl <- colorRampPalette(c('blue', 'light blue', 'light green', 'yellow')) (100)
plot(d, col=cl)
plot(coastlines, add=T, col="green")
