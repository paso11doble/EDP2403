# R spatial: CREAZIONE DI UN CODICE SPAZIALE

# Review della scorsa settimana: funzione per installazione dei pacchetti dall'esterno (ricordarsi le virgolette!): install.packages("sp")

# come richiamare il pacchetto: library(sp)
# come richiamare i dati: data(meuse)

# head(meuse): in questo modo visualizzo le prime 6 righe della tabella in esame

# plot cadmium e lead

# attach(meuse): così alleghiamo il dataframe
plot(cadmium, lead, col="red", pch=19, cex=2)

# exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)

# come cambiare le etichette (X lable e Y lable) nel grafico
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco")

# multiframe o multipanel
par(mfrow=c(1,2))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) (dopo aver copiato su R premere SEMPRE invio!)

# come invertire i grafici riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) (dopo aver copiato su R premere SEMPRE invio!)

# multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6]) (Utilizziamo una funzione all'interno di GGally)

# Spatial!!
head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

# funzione spplot per "plottare" i dati spazialmente
spplot(meuse, "zinc")
