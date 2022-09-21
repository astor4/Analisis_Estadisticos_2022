##Laboratorio 5
##Diego Axayacatl Gonzalez Cuellar
##1610823

##Coeficiente de correlacion


# Ingreso de datos y vectores ---------------------------------------------

x1 <- c(10,8,13,9,11,14,6,4,12,7,5)
x2 <- c(10,8,13,9,11,14,6,4,12,7,5)
x3 <- c(10,8,13,9,11,14,6,4,12,7,5)
x4 <- c(8,8,8,8,8,8,8,19,8,8,8)

y1 <- c(8.04,6.95,7.58,8.81,8.33,9.96,7.24,4.26,10.84,4.82,5.68)
y2 <- c(9.14,8.14,8.74,8.77,9.26,8.10,6.13,3.10,9.13,7.26,4.74)
y3 <- c(7.46,6.77,12.74,7.11,7.81,8.84,6.08,5.39,8.15,6.42,5.73)
y4 <- c(6.58,5.76,7.71,8.84,8.47,7.04,5.25,12.50,5.56,7.91,6.8)


# Coeficiente de correlacion ----------------------------------------------

cor.x1y1 <- cor.test(x1,y1)
cor.x1y1
xy1.lm <- lm(y1 ~ x1)

## para el grupo de x1y1 el valor de r = 0.8164205, df = 9
## el valor de p = 0.00217 rechazando la hipotesis nula y aceptando la alterna
## denotando que si existe una diferencia significativa entre los datos
## aunque la correlacion si sea signifiativa pero los datos se disperesen 
## atipicamente

summary(xy1.lm)

cor.x2y2 <- cor.test(x2,y2)
cor.x2y2
xy2.lm <- lm(y2 ~ x2)

## para el grupo x2y2 el valor de r = 0.8162365, df = 9
## el valor de p = 0.002179 rechazando la hipotesis nula y aceptando la alterna
## demostrando que si existe una diferencia significativa entre los datos
## y cuenta con una correlacion lineal significativa

summary(xy2.lm)

cor.x3y3 <- cor.test(x3,y3)
cor.x3y3
xy3.lm <- lm(y3 ~ x3)

## para el grupo x3y3 el valor de r = 0.8162867, df = 9
## el valor de p = 0.002176 aceptando la hipotesis alterna y rechazando la nula
## marcando que si existe una diferencia significativa entre los datos
## aunque la correlacion si sea signifiativa pero los datos se disperesen 
##atipicamente

summary(xy3.lm)

cor.x4y4 <- cor.test(x4,y4)
cor.x4y4
xy4.lm <- lm(y4 ~ x4)

## para el grupo x4y4 el valor de r = 0.8166967, df = 9
## el valor de p = 0.002156 aceptando la hipotesis alterna y rechazando la nula
## demostrando que si existe una diferencia significativa entre los datos
##aunque la correlacion si sea signifiativa pero los datos se disperesen 
##atipicamente

summary(xy4.lm)

# Grafico de dispersion de puntos ---------------------------------------

op = par(mfrow = c(2, 2), mar = c(4.5, 4, 1, 1))
plot(anscombe$x1, anscombe$y1, pch = 23)
abline(xy1.lm, col = "blue")
plot(anscombe$x2, anscombe$y2, pch = 23)
abline(xy2.lm, col = "blue")
plot(anscombe$x3, anscombe$y3, pch = 23)
abline(xy3.lm, col = "blue")
plot(anscombe$x4, anscombe$y4, pch = 23)
abline(xy4.lm, col = "blue")
par(op)

## Graficamente se puede notar que los datos mantienen correlaciones positivas
## si embargo muestran distribuciones diferentes, el primer grupo xy1 cuentan
## con una distribucion normal y un relacion linear simple
## el grupo xy2 tiene una distribucion anormal aunque el valor de r demuestre
## una relacion entre las variables, el grupo xy3 si cuenta con un relacion
## lineal, sin embargo, la regresion no es concluyente dado por la distribucion
## el grupo xy4, graficamente no cuenta con una correlacion linear, debido a
## los datos atipicos.