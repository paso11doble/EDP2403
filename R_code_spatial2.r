# R SPATIAL 2: CREAZIONE DI UN CODICE SPAZIALE (25/03/20)

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
