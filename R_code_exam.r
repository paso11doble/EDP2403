#####################                                                                               ######################
### R_code_exam.r ###                                                                               ### MICHELE PASINI ###
#####################                                                                               ######################
                                           ########################################
                                           ### MODULO DI ECOLOGIA DEL PAESAGGIO ###
                                           ########################################

# Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html#Home

# R PLAN:
# 1. R_code_first.r
# 2. R_code_spatial.r
# 3. R_code_spatial2.r
# 4. R_code_point_patterns.r
# 5. R_code_teleril.r
# 6. R_code_landcover.r
# 7. R_code_multitemp.r
# 8. R_code_multitemp_NO2.r
# 9. R_code_snow.r
# 10. R_code_patches.r
# 11. R_code_crop.r - Exam Simulation
# 12. Species Distribution Modelling

# APPUNTI IDENTIFICATI IN MAIUSCOLO

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (1) ## R CODE FIRST: PRIMO CODICE R ECOLOGIA DEL PAESAGGIO (18/03/20)
#########

# prova

install.packages("sp")

library(sp)
# UN COMANDO ALTERNATIVO PER CARICARE LE LIBRERIE: require(sp)

data(meuse)
meuse

head(meuse)
names(meuse)

summary(meuse)

pairs(meuse)

pairs(~ cadmium + copper + lead, data = meuse)

# Exercise: add zinc to cadmium, copper and lead
pairs(~ cadmium + copper + lead + zinc, data = meuse)

# "freccia ↑": RIPRENDE L'ULTIMO COMANDO USATO

# "parentesi [...]": SERVONO PER FARE UN SUBSET
pairs(meuse[,3:6])

pairs(meuse[,3:6], col="green")
# PER CAMBIARE COLORE

pairs(meuse[,3:6], col="green", pch=19) 
# PER SELEZIONARE IL "POINT SHAPE" (O POINT CHARACTER)

pairs(meuse[,3:6], col="green", pch=19, cex=3) 
# PER AUMENTARE LE DIMENSIONI DEI PUNTI (cex=character exageration)

pairs(meuse[,3:6], col="green", pch=19, cex=3, main="Primo pairs") 
# PER DARE UN TITOLO 

# Exercise: add "elevation" character to the previous
pairs(meuse[,3:7], col="green", pch=19, cex=3, main="Primo pairs")

--------------------------------------------------------------------------------------------------

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
# "dollaro $": SERVE PER COLLEGARE PIU' VARIABILI/LAYER AL DATASET
attach(meuse)
plot(cadmium, copper)
plot(cadmium, copper, pch=17, col="green", main="primo plot", xlab="cadmio", ylab="rame")

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (2) ## R SPATIAL: CREAZIONE DI UN CODICE SPAZIALE (24/03/20)
#########

# BREVE REVIEW (ES. CON "meuse"): 
# FUNZIONE PER INSTALLARE PACCHETTI DALL'ESTERNO: install.packages("sp") (ricordarsi le virgolette!)
# COME RICHIAMARE UN PACCHETTO GIA' SALVATO: library(sp) / require(sp)
# COME RICHIAMARE I DATI DA USARE: data(meuse)
# COME VISUALIZZARE L'INCIPIT (PRIME 6 RIGHE) DELLA TABELLA: head(meuse)


# plot cadmium e lead

# COME ALLEGARE IL DATAFRAME: attach(meuse)

plot(cadmium, lead, col="red", pch=19, cex=2)


# Exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)


# COME IMPOSTARE DELLE ETICHETTE/LABLES NEL GRAFICO
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco")


# MULTIFRAME O MULTIPANEL
par(mfrow=c(1,2))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 

# COME INVERTIRE I PLOT RIGA/COLONNA IN COLONNA/RIGA
par(mfrow=c(2,1))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 


# MULTIFRAME AUTOMATICO
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6]) 
# "ggpairs": E' UNA FUNZIONE DEL PACCHETTO GGally


# Spatial!!

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

spplot(meuse, "zinc")

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (3) ## R SPATIAL 2: CREAZIONE DI UN CODICE SPAZIALE (25/03/20)
#########

# FUNZIONI PRIMARIE DEL PACCHETTO "sp" (ES. CON "MEUSE") 
install.packages("sp")
library(sp)
data(meuse)
head(meuse)

# SE VOLESSI VEDERE I NOMI DELLE COLONNE IN ALTERNATIVA A "head" POSSO USARE: names(meuse)

# COME DEFINIRE LE COORDINATE DEL DATASET: coordinates(meuse)=~x+y 
# N.B. USARE LA TILDE

# COME PLOTTARE I DATI NELLO SPAZIO (ES. CON ZINCO): spplot(meuse, "zinc")

# Exercise: fare spplot dei dati del rame
spplot(meuse, "copper")


# "bubble": POSSIBILE FUNZIONE PER LAVORARE SUI DATI DEL PLOT
bubble(meuse, "zinc")

# Exercise: creare un bubble del rame colorato di rosso
bubble(meuse, "copper", col="red")

                      
# "array": FUNZIONE CHE CONSISTE DI UNA SERIE DI NUMERI                     

# FORAMINIFERI E CARBON CAPTURE (ARGOMENTI TESI TREINNALE DI SOFIA E MARCO)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# I DATI SONO COLLEGATI TRA LORO? SI', SONO IN CORRELAZIONE
plot(foram, carbon, col="green", pch=19, cex=2)


# DATI DALL'ESTERNO SUL COVID-19

# CREARE CARTELLA SU WINDOWS: Disco locale(C:)/LAB
# PERCORSO/PATH PER WINDOWS: setwd("C:/LAB") (w.d. = WORKING DIRECTORY)
# FUNZIONE PER LEGGERE LA TABELLA: covid <- read.table("covid_agg.csv", head=TRUE)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (4) ## R CODE FOR POINT PATTERN ANALYSIS: 
######### PATTERN LEGATI AI PUNTI (1) (31/03/20)

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

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (5) ## R CODE FOR SATELLITE IMAGES ANALYSIS:
######### ANALISI DI TELERILEVAMENTO (1) (07/04/20)

install.packages("raster") / library(raster)

setwd("C:/LAB/")

p224r63 <- brick("p224r63_2011_masked.grd")  
# FUNZIONE "brick": PERMETTE DI CARICARE PIU' DATI DALL'ESTERNO E DI ASSOCIARLI AD UN'UNICA IMMAGINE
# AD ES. PER IMMAGINI SATELLITARI

p224r63_2011 <- brick("p224r63_2011_masked.grd")
# (p=path; r=row; grd=greed, "griglia")

plot(p224r63_2011)

# SELEZIONANDO "p224r63_2011" ED OSSERVANDO LA VOCE "names" SI POSSONO NOTARE 7 BANDE:
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
# "attach(dataframe)" NON FUNZIONA COL PACCHETTO "raster"
# CON "$" ALLEGO LA COLONNA (in questo caso Banda 1) ALL'IMMAGINE SATELLITARE (dataset) 
# "sre" E' IL SENSORE

# Exercise: plottare la banda del NIR con colorRampPalette dal rosso, all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100) #
plot(p224r63_2011$B4_sre, col=clnir)


# MULTIFRAME 

# PLOT DI BANDE DI COLORI DIVERSI
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
# FA SPARIRE L'ULTIMO PLOT CREATO


# NATURAL COLOURS MULTIFRAME 

# PLOT DELL'IMMAGINE A COLORI IN BASE A COME LI VEDREBBE L'OCCHIO UMANO
# 3 COMPONENTI/BANDE: R = banda del rosso, G = b. del verde, B = b. del blu
# N.B. "RGB" IN MAIUSCOLO, R E' CASE SENSITIVE!
plotRGB(p224r63_2011, r=3, g=2, b=1) 

#B1: blue => 1
#B2: green => 2
#B3: red => 3
#B4: NIR => 4

# IMM. COLORI NATURALI
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# NIR
# IMM. COLORI FALSATI (SCALIAMO I COLORI DI 1, INSERENDO NIR)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")


# HOW TO SAVE AN IMAGE (PDF, PNG,...)

pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()
         
par(mfrow=c(1,2))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# NIR nella componente red (VEDI SOPRA)
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

# DOBBIAMO IMPORTARE "p224r63_1988_masked.grd"    
# ANALIZZIAMO I DATI RELATIVI AL 1988 (DATI 08/04 RELATIVI AL 2011)

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


# COMPARISON BETWEEN THE IMAGES (1988-2011)

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
           
# COL PASSARE DEL TEMPO GRAN PARTE DELL'AREA E' STATA DESTINATA A SCOPI AGRICOLI


# CHANGING THE GRAIN (CAMBIO RISOL. IMMAGINI)

p224r63_2011lr <- aggregate(p224r63_2011, fact=10) 

p224r63_2011   
# GUARDANDO LE CARATTERISTICHE DELL'IMMAGINE, COME CAMBIA LA RISOLUZIONE?
p224r63_2011lr  
# LA RISOLUZIONE DA 30X30m E' PASSATA A 300x300m

par(mfrow=c(2,1))         
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin") 
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin") 
 
# LOWER RESOLUTION

p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50) 
p224r63_2011lr50
# LA RISOLUZIONE DA 30X30m E' PASSATA A 1500x1500m

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

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (6) ## R CODE FOR LAND COVER: 
######### ANALISI DELLA COPERTURA DEL SUOLO (21/04/20)

setwd("C:/LAB")

library(raster)
library(RStoolbox)

p224r63_2011 <- brick("p224r63_2011_masked.grd")


# PLOT RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# "unsuperClass(...)": CON QUESTO COMANDO, PROPRIO DI RStoolbox, DEVO SPECIFICARE IMMAGINI E N° CLASSI (IN MAIUSCOLO!)
# I PIXEL VENGONO ACCORPATI NELLE VARIE CLASSI/CLUSTER
               
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

p224r63_2011c

plot(p224r63_2011c$map)
# FACCIO UN PLOT DELLA MAPPA LEGATA ALL'IMMAGINE CON "$"

clclass <- colorRampPalette(c('green', 'red', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)
# LEGENDA: AD OGNI CLASSE (4) VIENE ASSOCIATO UN COLORE, VISUALIZZATO NELLA SCALA ALLA DESTRA DELL'IMMAGINE
                          
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2) 
# STESSO PROCESSO CON MENO CLASSI (2)
  

# CONSIDERAZIONI FINALI:
                    
# DATE CLASSI DIFFERENTI, L'INCERTEZZA DELL'ALGORITMO AUTOMATICO DI CLASSIFICAZIONE AUMENTA IN FUNZIONE DEL N° CLASSI: 
# PER QUESTO SI PARLA DI SENSITIVITA' DELL'ALGORITMO

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (7) ## R CODE FOR MULTITEMPORAL ANALYSIS:
######### ANALISI MULTITEMPORALE DI VARIAZIONE DELLA COPERTURA DEL SUOLO (1) (29/04/20)

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

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (8) ## MULTITEMPORAL ANALYSIS: R CODE FOR ANALYSING NO2 DATA FROM ESA - JANUARY TO MARCH 2020 
######### ANALISI MULTITEMPORALE DI DATI CONTINUI (1) (05/05/20)

setwd("C:/LAB/")

library(raster)

# FUNZIONE "raster": SI USA PER IMPORTARE SINGOLE IMMAGINI SATELLITARI
EN01 <- raster("EN_0001.png")
plot(EN01)

# Exercise: import all the other images 
EN02 <- raster("EN_0002.png")
EN03 <- raster("EN_0003.png")
EN04 <- raster("EN_0004.png")
EN05 <- raster("EN_0005.png")
EN06 <- raster("EN_0006.png")
EN07 <- raster("EN_0007.png")
EN08 <- raster("EN_0008.png")
EN09 <- raster("EN_0009.png")
EN10 <- raster("EN_0010.png")
EN11 <- raster("EN_0011.png")
EN12 <- raster("EN_0012.png")
EN13 <- raster("EN_0013.png")

ls()
#  [1] "after"              "before"             "cl"                
#  [4] "cover"              "d1c"                "d2c"               
#  [7] "defor1"             "defor2"             "EN01"              
# [10] "EN02"               "EN03"               "EN04"              
# [13] "EN05"               "EN06"               "EN07"              
# [16] "EN08"               "EN09"               "EN10"              
# [19] "EN11"               "EN12"               "EN13"              
# [22] "grafico1"           "grafico2"           "meuse"             
# [25] "output"             "p1"                 "panel.correlations"
# [28] "panel.smoothing"    "percent1"           "percent2"          
# [31] "totd1"              "totd2"    

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN01, col=cl)
plot(EN13, col=cl)

par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)


# MULTITEMPORAL ANALYSIS
# DIFFERENCE: LAST IMAGE - FIRST IMAGE

difno2 <- EN13 - EN01

cldif <- colorRampPalette(c('blue','black','yellow'))(100) # 
plot(difno2, col=cldif)

# BLUE: DIFFERENZE MINORI NO2 
# YELLOW: DIFFERENZE MAGGIORI NO2

# Exercise: plot all the images
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)
# N.B. LE IMMAGINI SONO 13, QUINDI CON "par" DOBBIAMO SELEZIONARE 4 RIGHE E 4 COLONNE!

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI DATI CONTINUI (2) (06/05/20)

setwd("C:/LAB/")

library(raster)

load("multitemp.NO2") 
# OPPURE DOWNLOAD DA IOL: load("EN.RData")

ls()
#  [1] "after"              "before"             "cl"                
#  [4] "cldif"              "cover"              "d1c"               
#  [7] "d2c"                "defor1"             "defor2"            
# [10] "difno2"             "EN01"               "EN02"              
# [13] "EN03"               "EN04"               "EN05"              
# [16] "EN06"               "EN07"               "EN08"              
# [19] "EN09"               "EN10"               "EN11"              
# [22] "EN12"               "EN13"               "grafico1"          
# [25] "grafico2"           "meuse"              "output"            
# [28] "p1"                 "panel.correlations" "panel.smoothing"   
# [31] "percent1"           "percent2"           "totd1"             
# [34] "totd2"             

# FUNZIONE "lapply": NON VIENE APPLICATA AD UN SINGOLO FILE MA FORMULA UN "CICLO"; 
# IN ALTRE PAROLE UNA QUALSIASI FUNZIONE VIENE APPLICATA AD UNA LISTA

setwd("C:/LAB/esa_NO2")
# CREARE UNA NUOVA CARTELLA ALL'INTERNO DI "LAB" ("esa_NO2") ED IMPORTARVI TUTTI I 13 PNG

# FUNZIONE "list.files": SERVE PER FARE UNA LISTA DI FILES CON ATTRIBUTI SIMILI (ES. PNG)
rlist <- list.files(pattern=".png")

rlist                                                     
#  [1] "EN_0001.png" "EN_0002.png" "EN_0003.png" "EN_0004.png" "EN_0005.png"
#  [6] "EN_0006.png" "EN_0007.png" "EN_0008.png" "EN_0009.png" "EN_0010.png"
# [11] "EN_0011.png" "EN_0012.png" "EN_0013.png"

# VISUALIZZIAMO SOLO I PNG


# A "lapply" ASSOCIAMO "raster" (-> SINGOLE IMMAGINI)
listafinale <- lapply(rlist, raster)

listafinale


# FUNZIONE "stack": DALLE IMMAGINI SATELLITARI CREA UN'UNICA BANDA
EN <- stack(listafinale)

cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl)

-----------------------------------------------------------------------------------------------------------------------

# ANALISI MULTITEMPORALE DI DATI CONTINUI (3) (12/05/20)
 
setwd("C:/LAB/esa_NO2") 

require(raster)

rlist <- list.files(pattern=".png") 
rlist 

listafinale <- lapply(rlist, raster)
# brick
 
EN <- stack(listafinale)
# stack
 
difEN <- EN$EN_0013 - EN$EN_0001
# DIFFERENZA TRA IL VALORE DEL PIXEL DELLA 13ma IMMAGINE E IL VALORE DELLA 1a
 
cld <- colorRampPalette(c('blue','white','red'))(100) # 
plot(difEN, col=cld)


cl <- colorRampPalette(c('red','orange','yellow'))(100) # 
plot(EN, col=cl) 
# PLOT INTERO SET

boxplot(EN)
boxplot(EN,horizontal=T)
boxplot(EN,horizontal=T,axes=T)
boxplot(EN,horizontal=T,axes=T,outline=F)
boxplot(EN,horizontal=T,axes=F,outline=F)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

#########
## (9) ## R CODE SNOW:
######### ANALISI DELLA COPERTURA NEVOSA NEL TEMPO, SU COPERNICUS (20/05/20)

# DATI EUROPEI AGGIORNATI AL 18/05/20

setwd("C:/LAB/")

install.packages("ncdf4")

require(ncdf4)
require(raster)

# VOGLIAMO IMPORTARE E VISUALIZZARE IL FILE CON ESTENSIONE "NC"
# 2 FUNZIONI POSSIBILI: "raster" (->PER SINGOLO LIVELLO) O "brick" (-> PER PIU' LIVELLI O UNO STACK DI DATI)

snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")

cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)

# Exercise: plot snow cover with the cl palette
plot(snowmay, col=cl)

# CI ASPETTIAMO CHE QUESTA COPERTURA DIMINUISCA NEL TEMPO

# create new folder in LAB ("snow")
# import snow data

setwd("C:/LAB/snow")

# PER IMPORTARE TUTTI I FILES: "stack"

rlist <- list.files(pattern=".tif")

rlist
# [1] "predicted.snow.2025.norm.tif" "snow2000r.tif"               
# [3] "snow2005r.tif"                "snow2010r.tif"               
# [5] "snow2015r.tif"                "snow2020r.tif"    

list_rast <- lapply(rlist, raster)

snow.multitemp <- stack(list_rast)

plot(snow.multitemp, col=cl)

# VOGLIAMO FARE UN PLOT DEI "TIF" AGLI ESTREMI (2000 E 2020); CON "$" UNIAMO LE SINGOLE IMMAGINI ALLO STACK

# PLOT
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl)
plot(snow.multitemp$snow2020r,col=cl)

# PLOT WITH LIMITS (zlim)
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r,col=cl,zlim=c(0,250))
plot(snow.multitemp$snow2020r,col=cl,zlim=c(0,250))
# IMPOSTANDO "zlim" LE MAPPE ORA HANNO LA MEDESIMA LEGENDA SULLA DESTRA (PRIMA ERA DIVERSA!)


# DIFFERENCE
diffsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue','white','red'))(100) 
plot(diffsnow, col=cldiff)


# PREDICTION (PREVISIONE PER 2025)
# go to IOL and download "prediction.r" into folder "snow"

source("prediction.r")
# FUNZIONE "source": SERVE PER CARICARE CODICI DALL'ESTERNO

predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

##########
## (10) ## R CODE PATCHES:
########## ANALISI PER PATCHES IN UN PAESAGGIO MISTO AGRICOLTURA-FORESTA (26/05/20)

# setwd("C:/LAB/")
library(raster)

# BREVE REVIEW:
# RASTER CON TANTI LIVELLI -> "brick"
# RASTER SINGOLO -> "raster"

d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
cl <- colorRampPalette(c('green','black'))(100) # 
plot(d1c, col=cl)
plot(d2c, col=cl)

# 1° CASO: AGRICOLTURA E FORESTA
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c, col=cl)
plot(d2c, col=cl)

# agriculture: class 1; forest: class 2 
d1c.for <- reclassify(d1c, cbind(1,NA))
d2c.for <- reclassify(d2c, cbind(1,NA))

# FUNZIONE "cbind": SERVE PER ANNULLARE ALCUNI VALORI (IN QUESTO CASO PER L'AGRICOLTURA -> 1)
# FUNZIONE "reclassify" (PER LA FORESTA -> 2)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c, col=cl)
plot(d1c.for, col=cl)

# 2° CASO: SOLO FORESTA
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)


# FUNZIONE "clump": SERVE PER AGGREGARE LE PATCHES (LEGATA AD "igraph")

# creating patches
install.packages("igraph")
require(igraph) # for patches

d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

d1c.for.patches <- raster("d1c.for.patches")
d2c.for.patches <- raster("d2c.for.patches")

# FUNZIONE PER IMPORTARE DATI IN R -> "raster"
# FUNZIONE PER ESPORTARE DATI DA R -> "writeRaster"

# Exercise: plottare entrambe le mappe una accanto all'altra 
par(mfrow=c(1,2))
plot(d1c.for.patches)
plot(d2c.for.patches)

clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100) # 
par(mfrow=c(1,2))
plot(d1c.for.patches,col=clp)
plot(d2c.for.patches,col=clp)

d1c.for.patches
# class      : RasterLayer 
# dimensions : 478, 714, 341292  (nrow, ncol, ncell)
# resolution : 1, 1  (x, y)
# extent     : 0, 714, 0, 478  (xmin, xmax, ymin, ymax)
# crs        : NA 
# source     : memory
# names      : clumps 
# values     : 1, 301  (min, max)

d2c.for.patches
# class      : RasterLayer 
# dimensions : 478, 717, 342726  (nrow, ncol, ncell)
# resolution : 1, 1  (x, y)
# extent     : 0, 717, 0, 478  (xmin, xmax, ymin, ymax)
# crs        : NA 
# source     : memory
# names      : clumps 
# values     : 1, 1212  (min, max)

# max patches d1c.for.patches = 301
# max patches d2c.for.patches = 1212


# PLOT RESULTS

time <- c("Before deforestation","After deforestation")
npatches <- c(301,1212)

output <- data.frame(time,npatches)
attach(output)

library(ggplot2) # for the final output plot

ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

##########
## (11) ## R CODE CROP: COME EFFETTUARE UN "RITAGLIO" (03/06/20)
##########

# FUNZIONE "crop": SERVE PER FARE UN RITAGLIO SU UNA DETERMINATA ZONA A PARTIRE DA UNA SCALA GLOBALE

setwd("C:/LAB/snow")

require(ncdf4)
require(raster)

# Exercise: upload the whole snow set (from 2000 to 2020, not predicted!) (utilizzare prima lapply poi stack)

rlist <- list.files(pattern="snow")
rlist

# save raster into list
# lapply

list_rast <- lapply(rlist, raster)
snow.multitemp <- stack(list_rast)

clb <- colorRampPalette(c('darkblue','blue','light blue'))(100)

plot(snow.multitemp, col=clb)

# N.B. NON C'E' BISOGNO DI UTILIZZARE "par"!


snow.multitemp
# DAI NOMI SELEZIONIAMO 2010r

plot(snow.multitemp$snow2010r, col=clb)


# FUNZIONE "zoom": PRESENTE NEL PACCHETTO RASTER
zoom(nome immagine, estensione)

# FUNZIONE "ext": SERVE PER DEFINIRE L'ESTENSIONE DI UNA CERTA AREA
extension <- c(xmin, xmax, ymin, ymax)

      
# DOVE SI TROVA L'ITALIA? (fare prima zoom poi crop)
      
extension <- c(6, 18, 40, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

extension <- c(6, 18, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)
      
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)     


plot(snow.multitemp$snow2010r, col=clb)
zoom(snow.multitemp$snow2010r, ext=drawExtent())
# DISEGNARE UN RETTANGOLO DALL'ALTO A SX DELL'AREA DI RIFERIMENTO (con "drawEstent()")

# funzione crop
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

# Exercise: crop the Italy extent on the WHOLE STACK of snow layers (quindi a partire dallo stack SNOW.MULTITEMP)
snow.multitemp.italy <- crop(snow.multitemp, extension)
plot(snow.multitemp.italy, col=clb)

plot(snow.multitemp.italy, col=clb, zlim=c(20,200))
# UNIFORMIAMO I LIMITI (con zlim)


# BOXPLOT DELLA COPERTURA NEVOSA
# vedi rif. "multitemp.NO2"
# COME VARIA IN MEDIA LA COPERTURA NEVOSA NEGLI ANNI?

boxplot(snow.multitemp.italy, horizontal=T, outline=F)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################

##########
## (12) ## SPECIES DISTRIBUTION MODELLING:
########## ANALISI DELLA DISTRIBUZIONE DELLE SPECIE IN UNA DETERMINATA AREA GEOGRAFICA (08/06/20)

install.packages("sdm")
library(sdm)

library(raster)
library(rgdal)

# SPECIES

file <- system.file("external/species.shp", package="sdm") 
# CARICHIAMO LO SHAPEFILE "species" DALL'ESTERNO ALL'INTERNO DEL PACCHETTO "sdm"

species <- shapefile(file)
# FUNZIONE "shapefile": E' DEL PACCHETTO "rgdal" (-> FILES VETTORIALI)
# SERVE PER MAPPARE LA DISTRIBUZIONE DI UNA DETERMINATA SPECIE A TERRA

species
# class       : SpatialPointsDataFrame 
# features    : 200 
# extent      : 110112, 606053, 4013700, 4275600  (xmin, xmax, ymin, ymax)
# crs         : +proj=utm +zone=30 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 
# variables   : 1
# names       : Occurrence 
# min values  :          0 
# max values  :          1 

# N.B. ZONE=FUSO -> SPAGNA (FUSO 30)

species$Occurrence
# "species" E' FORMATO DA DEI PUNTI CON COORDINATE E OGNUNO E' LEGATO AL FATTO CHE LA SPECIE SIA STATA VISTA O MENO (->Occurrence)

plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# ALL'INTERNO DI "species": OCCORRENZE = 1 -> SPECIE PRESENTE (PUNTO BLU)
#                           OCCORRENZE = 0 -> SPECIE ASSENTE (PUNTO ROSSO)


# MODEL

path <- system.file("external", package="sdm") 
# IMPORTIAMO LA CARTELLA "external" IN "sdm"


# PREDICTORS

lst <- list.files(path=path,pattern='asc$',full.names = T) #
preds <- stack(lst) 

lst
# [1] "C:/R-3.6.3/library/sdm/external/elevation.asc"    
# [2] "C:/R-3.6.3/library/sdm/external/precipitation.asc"
# [3] "C:/R-3.6.3/library/sdm/external/temperature.asc"  
# [4] "C:/R-3.6.3/library/sdm/external/vegetation.asc"   

# ABBIAMO 4 LAYER ASCII NELLA LISTA -> NE FACCIAMO UN SINGOLO OGGETTO CON "stack" -> "preds"

# PREDITTORI: VARIABILI PER PREVEDERE LA PROBABILE DISTRIBUZIONE DELLA NOSTRA SPECIE

cl <- colorRampPalette(c('blue','orange', 'red', 'yellow'))(100)
plot(preds, col=cl)

# PRENDIAMO SOLO I PUNTI CON OCCORRENZE = 1 -> SPECIE PRESENTE

plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# LOW ELEVATION: LA SPECIE IN QUESTIONE PREDILIGE PIANURA/COLLINA

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# PREDILIGE AMBIENTI CON TEMPERATURE MEDIO-ALTE

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# PRECIPITAZIONI: SITUAZIONE INTERMEDIA

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,],pch=16)
# LA SPECIE PREFERISCE L'OMBREGGIATURA


# CREIAMO (ATTRAVERSO UN'EQUAZIONE) UN MODELLO LINEARE CHE RACCOLGA TUTTE QUESTE VARIABILI
GLM (Generalized Linear Model)

# MODEL

d <- sdmData(formula=Occurrence~., train=species, predictors=preds)
d
# class                                 : sdmdata 
# =========================================================== 
# number of species                     :  1 
# species names                         :  Occurrence 
# number of features                    :  4 
# feature names                         :  elevation, precipitation, temperature, ... 
# type                                  :  Presence-Absence 
# has independet test data?             :  FALSE 
# number of records                     :  200 
# has Coordinates?                      :  TRUE 

# TRAIN: TUTTI I DATI RACCOLTI A TERRA
# PREDICTORS: VARIABILI (4)
# TILDE "~": serve per indicare "=" NEI MODELLI

# FUNZIONE "sdmData"

m1 <- sdm(Occurrence ~ elevation+precipitation+temperature+vegetation, data=d, methods='glm')

p1 <- predict(m1, newdata=preds)
# FUNZIONE "predict": SERVE PER FARE UNA MAPPA PREVISIONALE DEL MODELLO

plot(p1,col=cl)
points(species[species$Occurrence == 1,], pch=16)

##########################################################################################################################
##########################################################################################################################
##########################################################################################################################
                                _________________________________________________________________________
##########                     |                                                                        |
## EXAM ##                     | SURFACE WATER TEMPERATURE ANALYSIS IN THE GREAT LAKES BASIN (USA, CAN) |
##########                     |________________________________________________________________________|                       


# LAGO SUPERIORE
# LAGO HURON
# LAGO MICHIGAN
# LAGO ERIE
# LAGO ONTARIO


# CREARE NUOVA CARTELLA IN "LAB"
setwd("C:/LAB/GreatLakes")

require(ncdf4)
require(raster)

# DOWNLOAD DA COPERNICUS DI RILEVAMENTI (FILES ".nc") RELATIVI A TEMP. SUP. DELL'ACQUA DEI LAGHI -> "LSWT"
# DATI 11/2016 - 05/2020
# SELEZIONATI A RANDOM, PIU' O MENO OGNI 6 MESI

rlist <- list.files(pattern="GLOBE")
rlist
# [1] "c_gls_LSWT_201611010000_GLOBE_SLSTRA_v1.0.2.nc"
# [2] "c_gls_LSWT_201704210000_GLOBE_SLSTRA_v1.0.2.nc"
# [3] "c_gls_LSWT_201710110000_GLOBE_SLSTRA_v1.0.2.nc"
# [4] "c_gls_LSWT_201804010000_GLOBE_SLSTRA_v1.0.2.nc"
# [5] "c_gls_LSWT_201809210000_GLOBE_SLSTRA_v1.0.2.nc"
# [6] "c_gls_LSWT_201903110000_GLOBE_SLSTRA_v1.0.2.nc"
# [7] "c_gls_LSWT_201909010000_GLOBE_SLSTRA_v1.0.2.nc"
# [8] "c_gls_LSWT_202002210000_GLOBE_SLSTRA_v1.0.2.nc"
# [9] "c_gls_LSWT_202005210000_GLOBE_SLSTRA_v1.0.1.nc"

LT01 <- raster("c_gls_LSWT_201611010000_GLOBE_SLSTRA_v1.0.2.nc")
LT02 <- raster("c_gls_LSWT_201704210000_GLOBE_SLSTRA_v1.0.2.nc")
LT03 <- raster("c_gls_LSWT_201710110000_GLOBE_SLSTRA_v1.0.2.nc")
LT04 <- raster("c_gls_LSWT_201804010000_GLOBE_SLSTRA_v1.0.2.nc")
LT05 <- raster("c_gls_LSWT_201809210000_GLOBE_SLSTRA_v1.0.2.nc")
LT06 <- raster("c_gls_LSWT_201903110000_GLOBE_SLSTRA_v1.0.2.nc")
LT07 <- raster("c_gls_LSWT_201909010000_GLOBE_SLSTRA_v1.0.2.nc")
LT08 <- raster("c_gls_LSWT_202002210000_GLOBE_SLSTRA_v1.0.2.nc")
LT09 <- raster("c_gls_LSWT_202005210000_GLOBE_SLSTRA_v1.0.1.nc")

list_rast <- lapply(rlist, raster)
LSWT <- stack(list_rast)

cl <- colorRampPalette(c('cyan', 'blue', 'dark blue', 'purple', 'red'))(100)


# PLOT GENERALE
plot(LSWT, col=cl)

# PLOT 1° RILEVAMENTO (2016)
plot(LT01, col=cl)


# FOCUS SUI GRANDI LAGHI -> CROP 1° FOTO
extension <- c(xmin, xmax, ymin, ymax)
extension <- c(-93, -74, 41, 50)
zoom(LT01, ext=extension, col=cl)
plot(LT01, col=cl, ext=extension)
zoom(LT01, ext=drawExtent())
Canada01 <- crop(LT01, extension)
plot(Canada01, col=cl)
plot(Canada01, col=cl, main="Great Lakes 01/11/16")

# CROP INTERO STACK
LSWT_Canada <- crop(LSWT, extension)
plot(LSWT_Canada, col=cl)
plot(LSWT_Canada, col=cl, zlim=c(270,300))


# VARIAZIONE TEMP. ACQUA NEGLI ULTIMI 4 ANNI
boxplot(LSWT_Canada, horizontal=T, axes=T, outline=F)


# PLOT VARIAZIONE TEMP. 2019 - 2017
cl <- colorRampPalette(c('cyan', 'blue', 'dark blue', 'purple', 'red'))(100)
Canada03 <- crop(LT03, extension)
plot(Canada03, col=cl, main="Great Lakes 11/10/17", zlim=c(270,300))
Canada07 <- crop(LT07, extension)
plot(Canada07, col=cl, main="Great Lakes 01/09/19", zlim=c(270,300))
difTemp <- Canada07 - Canada03
cldif <- colorRampPalette(c('blue', 'white', 'red'))(100)
plot(difTemp, col=cldif)

# PLOT FINALE
par(mfrow=c(3,1))
plot(Canada07, col=cl,main="Great Lakes 01/09/19", zlim=c(270,300))
plot(Canada03, col=cl,main="Great Lakes 11/10/17", zlim=c(270,300))
plot(difTemp, col=cldif)

# INDICATIVAMENTE QUALI SONO I LAGHI PIU' A RISCHIO A CAUSA DELLA VAR. TEMP.?
# L. MICHIGAN, ERIE E ONTARIO
