##Tarea 5
##Diego Axayacatl Gonzalez Cuellar
##1610823

## Correlación


# Ejercicio 1 -------------------------------------------------------------

spd <- c(2,3,5,9,14,24,29,34)
abnd <- c(6,3,5,23,16,12,48,43)

efime.lm <- lm(abnd ~ spd)

plot(spd, abnd, pch = 23)
abline(efime.lm, col = "blue")

##Graficamente los datos tienen una correlacion lineal positiva
##aunque su dispersion en el grafico varia mucho en relacion de la regresion 

efime.cor <- cor.test(spd, abnd)
efime.cor

## el valor de r = 0.8441408 df = 6, el valor de p = 0.008393, por lo cual
## se acepta la hipotesis alterna demostrando que si Existe una correlacion
## positiva entre la velocidad del arroyo y la abundancia de efimeras

summary(efime.lm)

# Ejercicio 2 -------------------------------------------------------------

suelo <- read.csv("suelo.csv", header = T)
suelo

cor.test(suelo$pH, suelo$N)
cor.test(suelo$pH, suelo$Dens)
cor.test(suelo$pH, suelo$P)
cor.test(suelo$pH, suelo$Ca)
cor.test(suelo$pH, suelo$Mg)
cor.test(suelo$pH, suelo$K)
cor.test(suelo$pH, suelo$Na)


CuadroSuelo <- matrix(0,7,3)
colnames(CuadroSuelo) <- (c("Conjunto","r","Valor de P"))
rownames(CuadroSuelo)<- (c("1","2","3","4","5","6","7"))

conjunto <- c("pH - N", "pH - Dens", "pH - P", "pH - Ca", "pH - Mg", "pH - K", "pH - Na")
CuadroSuelo [, 1] <- conjunto

r <- c("0.636654", "-0.5890264", "0.5910303", "0.8086293", "-0.3957821", "0.5795727", "-0.693264")
CuadroSuelo [, 2] <- r

valordep <- c("0.00000149", "0.00001062", "0.00000974", "0.000000000003614", "0.005361", "0.00001585", "0.00000004724")
CuadroSuelo [, 3] <- valordep

plot(suelo$pH, suelo$N)
plot(suelo$pH, suelo$Dens)
plot(suelo$pH, suelo$P)
plot(suelo$pH, suelo$Ca)
plot(suelo$pH, suelo$Mg)
plot(suelo$pH, suelo$K)
plot(suelo$pH, suelo$Na)

## H0 = No existe una correlacion entre las caracteristicas fisico-quimicas
## del suelo y el pH
## Ha = Existe una correlacion positiva entre las caracteristicas
## fisico-quimicas del suelo y el pH
## La hipotesis nula es rechazada en los grupos pH-N, pH-P,pH-Ca, pH-K, 
## La hipotesis alterna fue rechazada en los grupos pH-Dens, pH-Mg, pH-Na
