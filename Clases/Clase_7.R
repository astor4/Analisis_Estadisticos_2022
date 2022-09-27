##Clase 7
##Diego Axayacatl Gonzalez Cuellar
##1610823
##Analisis de vaianza (ANOVA)

##3 o más tratamientos analisa la variacion interna o entre los tratamientos
##homogeneidad de varianzas
##normalidad de datos

## I = tratamientos, I(J-1) = error, GL = I - 1, total = IJ-1
## J = nuemro de observaciones que tiene cada tratamiento
## SC = suma de cuadrados SCtr =suma de cuadrados de los trat SCerror = suma de
## de cuadrados del error entre los grados de libertad
## CM del tratamiento entre CM del error = F
## CMt = cuadrado medio del tratamiento CMerror= cuadrado medio del error
## F = valor de f pF = probabilidad de F


# Datos -------------------------------------------------------------------

#Ho = No existe diferencia significativa entre la produccion de avena 
#     en los diferentes tipos de suelo

#Ha = Existe una diferencia significativa entre la produccion de avena y al 
#     menos un tipo de suelo

arena <- c(6,10,8,6,14,17,9,11,7,11)
arcilla <- c(17,15,3,11,14,12,12,8,10,13)
limo <- c(13,16,9,12,15,16,17,13,18,14)

prod <- c(arena,arcilla,limo)
suelo <- gl(3,10,30, labels = c("arena","arcilla","limo")) ##gl para generar 
suelo                                                     ##niveles  
prod

avena <- data.frame(suelo, prod)
avena

# Funcion tapply ----------------------------------------------------------

tapply(avena$prod, avena$suelo,length)
tapply(avena$prod, avena$suelo,mean)
tapply(avena$prod, avena$suelo,sd)
tapply(avena$prod, avena$suelo,var)

#homogeneidad de varianzas bartlett test y flinger test

bartlett.test(avena$prod, avena$suelo)
#Ho=No existe diferencia entre las varianzas
#Ha=Existe diferencia entre las varianzas

fligner.test(avena$prod, avena$suelo) 

#revision de los datos de forma grafica

plot(avena)
boxplot(avena$prod ~ avena$suelo)

#SC de los tratamientos
#erro = no atribuible al tratamiento
#si la varianza del error es más grande que la del tratamiento no habra 
#diferencias significativas
#si la varianza del error es más pequeña que la del tratamiento si habra
#diferencias significativas

#Si el valor de CMt es menor que CMerror no el valor de F dira que no hay
#hay diferencia significativa


#Suma de cuadrados total
SST <- sum((avena$prod - mean(avena$prod))^2)

#Suma de cuadrados del error
arena - mean(arena)
arcilla - mean(arcilla)
limo - mean(limo)

#Suma de cuadrados de los tratamientos
arena.sum <- sum((arena-mean(arena))^2)
arcilla.sum <-sum((arcilla-mean(arcilla))^2)
limo.sum <-sum((limo-mean(limo))^2)

SSe <- arena.sum + arcilla.sum + limo.sum

SStr <- SST - SSe

#Cuadrado medio
#Representa la varianza promedio que tiene el experimento

CMerror <- mean(tapply(avena$prod, avena$suelo, var))

CMtr <- SStr/2

Fcal <- CMtr/CMerror
Fcal

Ftab <- qf(0.95,2,27)
Ftab
#Fcal menor que el tabulado no habra diferencias significativas
#Fcal mayor que el tabulado habra diferencias significativas

#FV = suelo, error, totalt
#GL = suelo: 2, error: 27, total:29
#SCT = 414.7 SCerror = 315.5 SCtr = 99.2
#CMT =  CMerror = 11.68 CMtr = 49.6
#Fcal = 4.244691 probF = 0.02495065

probF <- 1-pf(Fcal,2,27) #2 =grados de libertad del tratamiento 27= grados
probF                        # de libertad del error

# ANOVA simplificado

avena.aov <- aov(avena$prod ~ avena$suelo)
summary(avena.aov)
avena.aov

par(mfrow=c(2,2))
plot(aov(avena$prod ~ avena$suelo))
par(mfrow=c(1,1))

TukeyHSD(avena.aov, conf.level = 0.95)

plot(TukeyHSD(avena.aov))
#mientras el tratamiento no toque la linea del 0 si hay diferencias significati

