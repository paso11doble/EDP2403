# R SPATIAL: CREAZIONE DI UN CODICE SPAZIALE (24/03/20)

# BREVE REVIEW (ES. CON "MEUSE"): 
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
