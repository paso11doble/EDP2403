# R SPATIAL: CREAZIONE DI UN CODICE SPAZIALE (24/03/20)

# BREVE REVIEW: 
# funzione per installare pacchetti dall'esterno: install.packages("sp") (ricordarsi le virgolette!)
# come richiamare il pacchetto (se già salvato): library(sp) / require(sp)
# come richiamare i dati da usare: data(meuse)
# come visualizzare l'incipit (prime 6 righe) della tabella: head(meuse)


# plot cadmium e lead

# come allegare il dataframe: attach(meuse)

plot(cadmium, lead, col="red", pch=19, cex=2)


# Exercise: plot di copper e zinc con simbolo triangolo (pch=17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)


# come impostare le etichette ("X lable" e "Y lable") nel grafico
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco")


# multiframe o multipanel
par(mfrow=c(1,2))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 

# come invertire i plot riga/colonna in colonna/riga
par(mfrow=c(2,1))
plot(cadmium, lead, col="red", pch=19, cex=2)
plot(copper, zinc, col="green", pch=17, cex=2) 


# multiframe automatico
install.packages("GGally")
library(GGally)
ggpairs(meuse[,3:6]) 
# ("ggpairs" è una funzione esclusiva di GGally)


# Spatial!!

head(meuse)

coordinates(meuse)=~x+y
plot(meuse)

spplot(meuse, "zinc")
