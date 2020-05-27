# R_code_exam.r

### 1. R_code_primocod.r

# prova

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO (18/03/20)

install.packages("sp")
library(sp)
# require(sp): è un comando alternativo per caricare le librerie

data(meuse)
meuse

head(meuse)
names(meuse)

summary(meuse)

pairs(meuse)

pairs(~ cadmium + copper + lead , data = meuse)

# Exercise: cadmium copper lead zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)

"freccia ↑": riprende l'ultimo comando usato
"parentesi [...]": servono per fare un subset
pairs(meuse[,3:6])
pairs(meuse[,3:6], col="green") per cambiare colore
pairs(meuse[,3:6], col="green", pch=19) per selezionare il "point shape"
pairs(meuse[,3:6], col="green", pch=19, cex=3) per aumentare dimensioni punti (*cex=character exageration)
pairs(meuse[,3:6], col="green", pch=19, cex=3, main="Primo pairs") per dare un titolo 
# Exercise: fare lo stesso aggiungendo "elevation" (7° carattere)
pairs(meuse[,3:7], col="green", pch=19, cex=3, main="Primo pairs")
-------------------------------------------------------------------------------------------------
# panels from outside
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))
 
    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}
 
panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}
 
panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col=white", ...)
}
--------------------------------------------------------------------------------------------------
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.istograms)
# Exercise: mettere come lower lo smoothing, come upper le correlations, come diagonal gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.istograms)
# funzione plot
plot(meuse$cadmium, meuse$copper)
"dollaro $": serve per collegare più variabili
attach(meuse)
plot(cadmium, copper)
plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame")


##########################################################################################################################


### 2. R_code_spatial.r

# R spatial: CREAZIONE DI UN CODICE SPAZIALE (24/03/20)

# Review della scorsa settimana: funzione per installazione dei pacchetti dall'esterno: install.packages("sp")
                                                                                     (ricordarsi le virgolette!)
# come richiamare il pacchetto in questione: library(sp)

# come richiamare i dati da usare: data(meuse)

# come visualizzare l'incipit (prime 6 righe) della tabella: head(meuse)


# plot cadmium e lead

# come allegare il dataframe: attach(meuse)

plot(cadmium, lead, col="red", pch=19, cex=2)

# Exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)

# come cambiare le etichette (X lable e Y lable) nel grafico
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco")

# multiframe o multipanel
par(mfrow=c(1,2))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 

# come invertire i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 


# multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6]) (Utilizziamo una funzione all'interno di GGally)
# Spatial!!
head(meuse)
coordinates(meuse)=~x+y
plot(meuse)
spplot(meuse, "zinc")
-----------------------------------------------------------------------------------------------------------
# R spatial: CREAZIONE DI UN CODICE SPAZIALE (2) (25/03/20)
# Funzioni primarie del pacchetto SP
install.packages("sp")
library(sp)
data(meuse)
head(meuse)
# se voglio vedere i nomi delle colonne in alternativa a "head" posso usare: names(meuse)
# come definire le coordinate del dataset: coordinates(meuse)=~x+y 
# come plottare i dati nello spazio (in questo caso dello zinco): spplot(meuse, "zinc")
# Exercise: fare spplot dei dati del rame
spplot(meuse, "copper")
# un tipo di funzione per lavorare sui dati del plot: bubble
bubble(meuse, "zinc")
# Exercise: creare un bubble del rame colorato di rosso
bubble(meuse, "copper", col="red")
                      
# array: funzione che consiste di una serie di numeri                      
# foraminiferi, carbon capture (argomenti di tesi triennale di Sofia e Marco)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)
# i dati sono collegati tra loro? sì sono in correlazione
plot(foram, carbon, col="green", pch=19, cex=2)
# DATI DALL'ESTERNO sul Covid-19

# cartella da creare su Windows: Disco locale(C:)/LAB
# percorso (path) da creare per Windows: setwd("C:/LAB") (*w.d. = working directory)
# funzione per leggere la tabella: covid <- read.table("covid_agg.csv", head=TRUE)


##########################################################################################################################


### 4. R_code_point_patterns.r

# CODICI PER ANALISI DEI POINT PATTERNS (Pattern legati ai punti) (Pratica del 31/03/20)

install.packages("ggplot2")     (se il pacchetto fosse già presente: library(ggplot2) oppure require(ggplot2))
install.packages("spatstat")

# setwd("C:/LAB/")

# importazione di dati dall'esterno
covid <- read.table("covid_agg.csv", head=T)

head(covid)

plot(covid$country, covid$cases)
# attach(covid)
# plot(country, cases)

funzione "las" serve per cambiare la disposizione delle etichette
plot(covid$country, covid$cases, las=0) #parallele agli assi
plot(covid$country, covid$cases, las=1) #orizzontali
plot(covid$country, covid$cases, las=2) #perpendicolari
plot(covid$country, covid$cases, las=3) #verticali

plot(covid$country, covid$cases, las=3) #vertical labels

funzione "cex.axis" serve per diminuire la dimensione delle etichette
plot(covid$country, covid$cases, las=3, cex.lab=0.5, cex.axis=0.5) #vertical labels

# ggplot2
data(mpg)
head(mpg)

# data (il file da utilizzare è "mpg")
# aestetics (variabili che compongono l'estetica del grafico)
# type (definisce la geometria, indicata dopo il "+")
ggplot(mpg, aes(x=displ,y=hwy)) + geom_point()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ,y=hwy)) + geom_polygon()

# ggplot di covid
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()
è una media dati di ogni Paese, di cui si prende come riferimento il centroide

# density
# create dataset for spatstat
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

> ppp(x.coordinates, y.coordinates, x.range, y.range)


d <- density(covids)

plot(d)
points(covids)

--------------------------------------------------------------------------------------------------------------

# Save the .RData (file salvato in LAB nel disco locale (C:)) 

setwd("C:/LAB/")                                                              (Pratica dell'01/04/20)
load("point_pattern.RData") (funzione per inserire l'RData di ieri)

ls()  (funzione per vedere cosa contiene)

plot(d)
# palette (colori per rappresentare la mappa)
cl <- colorRampPalette(c('yellow', 'orange', 'red')) (100)
plot(d, col=cl)

# Exercise: plot della mappa della densità dal verde al blu
cl <- colorRampPalette(c('green', 'light blue', 'blue')) (100)
plot(d, col=cl)

points(covids)



coastlines <- readOGR("ne_10m_coastline.shp") 

(devo utilizzare una funzione della libreria RGDAL, per leggere files vettoriali)
quindi prima di dare il comando  "readOGR" devo fare:
install.packages("rgdal") oppure library(rgdal)

plot(coastlines, add=T) (inseriamo le coastlines e le aggiungiamo al plot precedente)

points(covids) (funzione per inserire i centroidi di ieri)

N.B. la mappa è relativa ai dati Covid di febbraio!

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

(Pratica del 22/04/20)

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


----------------------------------------------------------------------------------------------------------

# Esercizio San Marino (Pratica del 28/04/20)
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



##########################################################################################################################


### 5. R_code_teleril.r

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



##########################################################################################################################


### 7. R_code_teleril.r
