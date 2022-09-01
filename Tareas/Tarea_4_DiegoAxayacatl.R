#Tarea 4


# Problema 1 -------------------------------------------------------------

#Histogramas

set.seed(9875)
size <- 1000
x2 <- round(runif(n = size, min = 0, max = 10),2)
x2

hist_mil <- hist(x2, las= 1, col= "green")
hist_mil
hist_mil$breaks


hist_mil1 <- hist(x2, xaxt = "n",
breaks = c(0,1,2,3,4,5,6,7,8,9,10),
col = "#ff0000",
ylab = "Frecuencias",
las = 1,
ylim = c(0,120))
axis(1,hist_mil1$mids)

hist_mil1

hist_mil2 <- hist(x2, xaxt = "n",
                  breaks = c(0,2,4,6,8,10),
                  col = "#0000ff",
                  ylab = "Frecuencias",
                  las = 1,
                  ylim = c(0,250))
axis(1,hist_mil2$mids)


hist_mil3 <- hist(x2, xaxt = "n",
                  breaks = c(0,1,2,4,7,10),
                  col = "#ff0000",
                  ylab = "Frecuencias",
                  las = 1,)
axis(1,hist_mil3$mids)

hist_mil3


# Problema 2 --------------------------------------------------------------


#a)El histograma A
#b)El histograma D
#c)El histograma C
#d)El histograma B
#e)El histograma C


# Problema 3 --------------------------------------------------------------

frec_terr <- table(quakes$mag) ##La funcion table() te ayuda a 
frec_terr                               ##crear una tabla de frecuencias


propor_terr <- frec_terr / sum(frec_terr) 
propor_terr 

porc_terr = 100 * propor_terr 
porc_terr



mags <- hist(quakes$mag, xaxt = "n",
             col = "#e6ac00", xlab="Magnitud de los terremotos",
             ylab= "Frecuencias",
             main = "",
             las = 1,
             cex.names = 0.1,
             ylim = c(0,260),
             labels = paste(levels(quakes$mag),
                            round(porc_terr,2),"%"))
axis(1, mags$mids)

#a)Tiene un sesgo a la derecha
#b)El intervalo de 4.5
#c)El rango es 4:6.4
#d)2.1%
#e)19.8%
#f)58.5%



# Problema 4 --------------------------------------------------------------

summary(quakes$mag)

##61%

##c)75%


# Problema 5 --------------------------------------------------------------

#a) la especie C
#b) la especie F
#C) la especie F
#d) la especoe C
#e) la especie H
#f) la especie F
#g) la especie C
#h) la especie F
#i) la especie H
#j) la especie F


# Problema 6 --------------------------------------------------------------

fires <- c(78, 44, 47, 105, 126, 181, 277, 210, 155)
fires

summary(fires)
quantile(fires)

#1) 44
#2) 277
#3) 44:277
#4) 78
#5) 126
#6) 181
#7) 126
var(fires)
#8) 6069.11
sd(fires)
#9)77.90
#10)
boxplot(fires, horizontal = T, col = "purple", xlab = "Frecuencuencias de los incendios", main = "Incendios forestales en México")
