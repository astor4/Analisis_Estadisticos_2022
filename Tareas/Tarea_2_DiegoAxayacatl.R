#Tarea 2


# Importacion de datos ----------------------------------------------------

conjunto <- read.csv("Tareas/CUADRO1.csv", header = T)

help("subset")


# Altura ------------------------------------------------------------------


media.h <- mean(conjunto$Altura)

media.h

H.media <- subset(conjunto$Altura, conjunto$Altura <= media.h)

H.16 <- subset(conjunto$Altura, conjunto$Altura <= 16)

H.16

# Vecinos -----------------------------------------------------------------


Vecinos_3 <- subset(conjunto$Vecinos, conjunto$Vecinos <= 3)

Vecinos_3

Vecinos_4 <- subset(conjunto$Vecinos, conjunto$Vecinos > 4)

Vecinos_4

# Diametro ----------------------------------------------------------------

DBH_mean <- mean(conjunto$Diametro)

DBH_media <- subset(conjunto$Diametro, conjunto$Diametro < DBH_mean)

DBH_media

DBH_16 <- subset(conjunto$Diametro, conjunto$Diametro > 16)

DBH_16

# Especie -----------------------------------------------------------------

Especie_C <- subset(conjunto$Especie, conjunto$Especie == "C")

Especie_C

Especie_HF <- subset(conjunto$Especie, conjunto$Especie == conjunto$Especie[c(1,4)])

Especie_HF


# Determinacion -----------------------------------------------------------

Diametro16.9 <- subset(conjunto$Diametro, conjunto$Diametro <= 16.9)

Diametro16.9

Altura18.5 <- subset(conjunto$Altura, conjunto$Altura > 18.5)

Altura18.5


# Visualizacion de datos --------------------------------------------------

##Histograma Altura

hist(conjunto$Altura, col="blue", main = "Altura de los arboles", xlab = "Alturas", ylab = "Frecuencia")

##Histograma H.media

hist(H.media, col="red", main = "Altura menores o iguales a la media ", xlab = "Alturas", ylab = "Frecuencia")

##Histograma H.16

hist(H.16, col="green", main = "Altura menores o 16.5 m ", xlab = "Alturas", ylab = "Frecuencia")

##Histograma Vecinos

hist(conjunto$Vecinos, col="yellow", main = "Arboles vecinos", xlab = "No. de arboles vecinos", ylab = "Frecuencia")

##Histograma Vecinos_3

hist(Vecinos_3, col="#cc33ff", main = "Arboles vecinos", xlab = "No. de arboles vecinos", ylab = "Frecuencia")

##Histograma Vecinos_4

hist(Vecinos_4, col="#ff8c1a", main = "Arboles vecinos", xlab = "No. de arboles vecinos", ylab = "Frecuencia")

##Histograma Diametro

hist(conjunto$Diametro, col="#33ff77", main = "Diametros de los arboles", xlab = "Diametros", ylab = "Frecuencia", xlim = c(5,26))

##Histograma DBH_media

hist(DBH_media, col="#999966", main = "Diametros de los arboles menores a la media", xlab = "Diametros", ylab = "Frecuencia", xlim = c(6,17))

##Histograma DBH_16

hist(DBH_16, col="#0066ff", main = "Diametros de los arboles mayores a 16 m ", xlab = "Diametros", ylab = "Frecuencia")


# Estadisticas basicas -----------------------------------------------------

##Objetos de Altura

Altura_media <- mean(conjunto$Altura)
Altura_media
Altura_sd <- sd(conjunto$Altura)
Altura_sd

H.media_m <- mean(H.media)
H.media_m
H.media_sd <- sd(H.media)
H.media_sd

H.16_media <- mean(H.16)
H.16_media
H.16_sd <- sd(H.16)
H.16_sd

Vecinos_media <- mean(conjunto$Vecinos)
Vecinos_media
Vecinos_sd <- sd(conjunto$Vecinos)
Vecinos_sd

Vecinos_3_media <- mean(Vecinos_3)
Vecinos_3_media
Vecinos_3_sd <- sd(Vecinos_3)
Vecinos_3_sd

Vecinos_4_media <- mean(Vecinos_4)
Vecinos_4_media
Vecinos_4_sd <- sd(Vecinos_4)
Vecinos_4_sd

Diametro_media <- mean(conjunto$Diametro)
Diametro_media
Diametro_sd <- sd(conjunto$Diametro)
Diametro_sd

DBH_media_m <- mean(DBH_media)
DBH_media_m
DBH_sd <- mean(DBH_media)
DBH_sd

DBH_16_media <- mean(DBH_16)
DBH_16_media
DBH_16_sd <- sd(DBH_16)
DBH_16_sd

