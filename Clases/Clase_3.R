#Clase 3
#Diego Axayacatl
#23/08/22


# Importar CSV ---------------------------------------------------------------------


est <- read.csv("Clases/cumbres.csv", header = T)
head(est) # Revisar los primeros seis datos
tail(est) # Revisar los ultimos sesis datos

viv <- read.csv("Clases/vivero.csv", header = T)

boxplot(viv$IE ~ viv$Tratamiento)

viv$Tratamiento <- as.factor(viv$Tratamiento)

boxplot(viv$IE ~ viv$Tratamiento)

summary(viv)

t