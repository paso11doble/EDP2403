# R SPATIAL: CREAZIONE DI UN CODICE SPAZIALE (25/03/20)

# Funzioni primarie del pacchetto "sp"
install.packages("sp")
library(sp)
data(meuse)
head(meuse)

# se volessi vedere i nomi delle colonne in alternativa a "head" posso usare: names(meuse)

# come definire le coordinate del dataset: coordinates(meuse)=~x+y 


# come plottare i dati nello spazio (in questo caso dello zinco): spplot(meuse, "zinc")

# Exercise: fare spplot dei dati del rame
spplot(meuse, "copper")


# una possibile funzione per lavorare sui dati del plot: bubble
bubble(meuse, "zinc")

# Exercise: creare un bubble del rame colorato di rosso
bubble(meuse, "copper", col="red")

                      
# array: è una funzione che consiste di una serie di numeri                      

# foraminiferi, carbon capture (argomenti di tesi triennale di Sofia e Marco)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# I dati sono collegati tra loro? Sì, sono in correlazione
plot(foram, carbon, col="green", pch=19, cex=2)


# DATI DALL'ESTERNO SUL COVID-19

# cartella da creare su Windows: Disco locale(C:)/LAB
# percorso (path) per Windows: setwd("C:/LAB") (w.d. = working directory)
# funzione per leggere la tabella: covid <- read.table("covid_agg.csv", head=TRUE)
