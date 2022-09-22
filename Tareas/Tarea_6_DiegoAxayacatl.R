##Tarea 6
##Diego Axayacatl Gonzalez Cuellar
##1610823


# Ejercicio 1 -------------------------------------------------------------

erupciones <- read.csv("erupciones.csv", header = T)
erupciones

plot(erupciones$waiting, erupciones$eruptions, pch = 23,
     ylab = "Duracion de las erupciones (min)",
     xlab = "Tiempo de espera entre erupciones (min)")


# Correlacion -------------------------------------------------------------

erup.med <- mean(erupciones$eruptions)
erup.med #media de las erupciones
erup.sd <- sd(erupciones$eruptions)
erup.sd #desviacion estandar de las erupciones
erup.var <- var(erupciones$eruptions)
erup.var #varianza de las erupciones

wait.med <- mean(erupciones$waiting)
wait.med #media del tiempo de espera entre erupciones
wait.sd <- sd(erupciones$waiting)
wait.sd #desviacion estandar del tiempo de espera entre erupciones
wait.var <- var(erupciones$waiting) #varianza del tiempo de espera entre erupciones
wait.var

cor.eruwait <- cor.test(erupciones$eruptions, erupciones$waiting)
cor.eruwait # el valor de r = 0.9008112 y si cuenta con una
            #correlacion significativa


# Regresion lineal --------------------------------------------------------

#H0 = No existe una correlacion significativa entre el tiempo de las erupciones
#y el tiempo de espera
#Ha = Existe una correlacion significativa entre el tiempo de las erupciones y
#el tiempo de espera

erup.lm <- lm(erupciones$eruptions ~ erupciones$waiting)
erup.lm
summary(erup.lm)

#valor de alfa = -1.87402
#valor de beta = 0.07563
#el valor de p = 0.0000000000000000022 por lo tanto si es significativa
#alfa y beta son altamente significativas ***
#la regresion si es significativa respecto a alfa, beta y el p-value

beta <- c(0.075628)
num <- c(1,2,3,4,5)
x <- c(80,40,45,53,61)
esti <- c(-1.87402+(beta*x))
Estim <- data.frame(No = num,
                    X = x,
                    Yestimada = esti)
Estim


