# R CODE PATCHES:
# ANALISI PER PATCHES IN UN PAESAGGIO MISTO AGRICOLTURA-FORESTA (26/05/20)

# setwd("C:/LAB/")
library(raster)

# BREVE REVIEW:
# RASTER CON TANTI LIVELLI, ES. IMMAGINI SATELLITARI -> "brick"
# RASTER SINGOLO, ES. MAPPA CLASSIFICATA -> "raster"

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


# FUNZIONE "clump": SERVE PER AGGREGARE LE PATCHES

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
