## Comparacion de medias

##Ingreso de datos

costal <- c(87.7, 80.01, 77.28, 78.76, 81.52, 74.2, 80.71, 79.5, 77.87, 81.94,80.7,
            82.32, 75.78, 80.19, 83.91, 79.4, 77.52, 77.62, 81.4, 74.89, 82.95,
            73.59, 77.92, 77.18, 79.83, 81.23, 79.28, 78.44, 79.01, 80.47, 76.23,
            78.89, 77.14, 69.94, 78.54, 79.7, 82.45, 77.29, 75.52, 77.21, 75.99,
            81.94, 80.41, 77.7)
hist(costal)

##Numero de observaciones

n <- length(costal)

##Media

costa.media <- mean(costal)
costa.media

##Desviacion estandar

costa.sd <- sd(costal)
costa.sd

#Necesitamos responder la siguiente pregunta: Dada la desviación estándar (3.056), ¿Cuál es la probabilidad
#de observar una media de la muestra (cuando n = 44) de 78.91 Kg o de menor cantidad si la media verdadera
#es de 80 kg.

#Prueba de T

costa.se <- costa.sd/ sqrt(n)
costa.se

##Valor de T
costa.T <- (costa.media - 80)/costa.se
costa.T

##Valor de p 

pt(costa.T, df= n-1)

###Ejercicio 1

t.test(costal, mu=80)

#1) Valor de p: 0.02264
#2) Grados de libertad: 43
#3) Se acepta la hipotesis alterna
#4) Si existe evidencia, la media como tal es menor y al correr la prueba de T
#   de Student el p-value determina estadisticamente cual hipotesis se acepta en
#   en este caso fue la hipotesis alterna.

###Ejercicio 2

azufre <- c(15.8, 22.7, 26.8, 19.1, 18.5, 14.4, 8.3, 25.9, 26.4, 9.8,
            22.7, 15.2, 23.0, 29.6, 21.9, 10.5, 17.3, 6.2, 18.0, 22.9,
            24.6, 19.4, 12.3, 15.9, 11.2, 14.7, 20.5, 26.6, 20.1, 17.0,
            22.3, 27.5, 23.9, 17.5, 11.0, 20.4, 16.2, 20.8, 13.3, 18.1)

hist(azufre)

t.test(azufre, mu=17.5)

## 1) Valor de p: 0.1893
## 2) Intervalos de confianza: 16.87912 - 20.53588
## 3) Grados de libertad: 39
## 4) Se acepta la hipotesis nula debido a que el p-value es mayor que alfa
## 5) Si existe evidencia que el valor medio promedio de las emisiones es mayor
##    pero estadisticamente no existe un diferencia significativa


###Ejercicio 3

tad <- paste0("https://raw.githubusercontent.com/mgtagle/MCF-202_Agosto_2021/main/TEMPAIRE_DIA.csv")

tempad <- read.csv(tad)

tempad

hist(tempad$temp_media)

tempad$temp_media <- as.factor(tempad$temp_media)

t.test(tempad$temp_media, mu=24)

## 1) Valor de p: 0.03615
## 2) Intervalos de confianza:  23.28216 - 23.97599
## 3) Grados de libertad: 845 (no estoy seguro si deberian ser 199 ya que las
##    el numero de muestras son 200)
## 4) Estadisticamente la hipotesis alterna ya que el p-value es menor que alfa
## 5) No existe evisdncia significativa