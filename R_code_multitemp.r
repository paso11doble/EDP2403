# R code: analisi multitemporale di variazione di land cover (pratica del 29/04/20)
   (utilizzo file deforestazione Foresta Amazzonica, Mato Grosso)

setwd("C:/LAB/")

library(raster)

funzione "brick" per caricare dati dall'esterno
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

selezionando su R "defor1": troviamo al suo interno 3 bande
# names: defor1_.1, defor1_.2, defor1_.3 
# defor1_.1 = nir
# defor1_.2 = red
# defor1_.3 = green

# plot: associamo le 3 bande alle componenti RGB 
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")

# Exercise: plot della 2a data
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")

acqua pura assorbe completamente i.r. e diventa nera: 
in una foto nell'acqua ci sono più solidi disciolti: 
per questo motivo l'acqua risulta di due colori diversi

par(mfrow=c(2,1))
plotRGB(defor1,r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2,r=1, g=2, b=3, stretch="Lin")
 
# classificazione non supervisionata (del defor1)

funzione unsuperClass: per usarla dobbiamo caricare il pacchetto RStoolbox
library(RStoolbox)
d1c <- unsuperClass(defor1, nClasses=2)
dopo aver selezionato "d1c" faccio "d1c$map"

plot(d1c$map): la mappa deve essere legata al modello

da qui ho 2 possibilità:
cl <- colorRampPalette(c('black','green'))(100) #
plot(d1c$map, col=cl)
cl <- colorRampPalette(c('green','black'))(100) #
plot(d1c$map, col=cl)

# esempio significativo del "$"
# mappageologica <- geomap(im_sat, nClasses=...)
# plot(mappageologica$lito)
# plot(mappageologica$lineaments)

# classificazione (del defor2)

# Exercise: classificare con 2 classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
d2c$map
plot(d2c$map)
  cl <- colorRampPalette(c('green','black'))(100) #
  plot(d2c$map, col=cl)
  
le 2 classi sono le foreste e tutto ciò che non è foresta

dev.off()

# plot delle 2 mappe ottenute
par(mfrow=c(2,1))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

funzione "freq()" calcolare la frequenza dei valori
freq(d1c$map)

value  count
# [1,]     1  33817  foresta
# [2,]     2 307475  aree aperte

totd1 <- 33817 + 307475

totd1
# [1] 341292

una volta che abbiamo le frequenze calcoliamo la proporzione

percent1 <- freq(d1c$map)  * 100/totd1

percent1
            value     count
[1,] 0.0002930042  9.908524  foresta
[2,] 0.0005860085 90.091476  aree aperte

facciamo lo stesso per la seconda mappa

 freq(d2c$map)
     value  count
# [1,]     1 163569 foresta
# [2,]     2 179157 aree aperte

totd2 <- 163569 + 179157

totd2
# [1] 342726

percent2 <- freq(d2c$map)  * 100/totd2

percent2
            value    count
[1,] 0.0002917783 47.72588 foresta
[2,] 0.0005835565 52.27412 aree aperte

cover <- c("Agriculture","Forest")
before <- c(90,9.9)
after <- c(52.3,47.7)

output <- data.frame(cover,before,after)
View(output)



