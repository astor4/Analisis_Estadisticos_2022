##Clase 6
##Diego Axayacatl Gonzalez Cuellar
##1610823

## x = variables independiente y = variables dependientes

## Correlacion positiva = a medida que aumenta x aumenta y  +1
## Correlacion negativa = a medida que aumenta y dismunye x -1


## Regression: estima la linea de tendencia central de los datos 

#Correlacion


# Importar datos ----------------------------------------------------------

eba <- read.csv("ebanos.csv", header = T)
eba

plot(eba$diametro, eba$altura, pch=19, col ="red", xlab="Diámetro (cm)",
     ylab="Altura (m)")

#Correlacion de pearson (r)

#Si el valor de p es mayor no hay correlacion si es menor si hay correlacion

cor.eba <- cor.test(eba$diametro, eba$altura)
cor.eba

#si existe una fuerte correlacion entre el diametro y la altura 

#Regresion (r**2)


# Canopy ------------------------------------------------------------------

shapiro.test(canopy$Cnpy)
shapiro.test(canopy$LAI4)
shapiro.test(canopy$GLI)

canopy <-read.csv("canopy_short.csv", header=T)
canopy

plot(canopy$Cnpy, canopy$LAI4,
     xlab = "apertura del dosel (%)", 
     ylab = "IAF",
     pch = 19, col = "orange")

cor.canopy <- cor.test(canopy$Cnpy, canopy$LAI4)
cor.canopy

plot(canopy$Cnpy, canopy$GLI,
     xlab = "apertura del dosel (%)",
     ylab = "GLI",
     pch = 19, col = "red")

cor.glican <- cor.test(canopy$Cnpy, canopy$GLI, method="kendall")
cor.glican

plot(canopy$LAI4, canopy$GLI,
     xlab = "LAI",
     ylab = "GLI",
     pch = 15, col = "dark green")

cor.laigli <- cor.test(canopy$LAI4, canopy$GLI)
cor.laigli

#method = "kendall" para datos no normales


# Regresión ---------------------------------------------------------------

#Regresión entre cnpy vs LAI4

cor.canopy

#Funcion lm (lineal model)

cp.lm <- lm(canopy$LAI4 ~ canopy$Cnpy) ##primero se pone la variable dependiente
cp.lm #intercept es alfa y el segundo valor es beta

## si las variables estan mal acomodadas lo valors del intercepto son anormales
## y la linea de tendencia central no se dibuja en la grafica


plot(canopy$Cnpy, canopy$LAI4,
     xlab = "apertura del dosel (%)", 
     ylab = "IAF",
     pch = 19, col = "orange")
abline(cp.lm, col = "blue") ## esta funcion se utiliza para aplicar la 
                            ## funcion lm a la grafica
text(37, 1.5, "y=2.67-0.04*x", pos =2) ## esta funcion sirve para poner
                                       ## la formula en la grafica

summary(cp.lm)
# Residual standard error: Diferencia que existe entre cada valor de y observado
# y el de la y prima, la sumatoria de todos los residuales debe ser cero
# Los coeficienctes son altamente significativos

#Sumatoria de los residuales

sum(cp.lm$residuals)
canopy$Yprima <- cp.lm$fitted.values
canopy$Yprima
canopy$dif <- canopy$LAI4 - canopy$Yprima
canopy$residual <- cp.lm$residuals

sum(canopy$residual**2)/38 #la variavilidad del IAF es de 44% sumatoria de los
#residuales al cuadrado, si el resultado se divide entre los grados de libertad
#da como resultado la varianza

summary(cp.lm)
