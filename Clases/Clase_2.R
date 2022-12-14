# Diego Axayacatl
# 16/08/22
# Clase 2

#

# Ingresar datos ----------------------------------------------------------

dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7,
         11.2, 24.1, 14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5,
         7.8, 27.3, 9.7, 6.5, 23.4, 8.2, 28.5, 10.4,11.5,
         14.3, 17.2, 16.8)

length(dbh)

# Medidas de tendencia central
media <- sum(dbh)/length(dbh)
mean(dbh)
median(dbh)

# Media geometrica

exp(mean(log(dbh)))

# Medidas de dispersion

sd(dbh)
var(dbh)
coef.var <- sd(dbh)/mean(dbh)*100
coef.var

fivenum(dbh)
quantile(dbh, 0.15)
quantile(dbh, 0.30)

#Grafica 

###Grafico de caja

boxplot(dbh, horizontal = T, col = "blue", ylab = "Diametro (cm)", xlab = "Sitio 1", main = "Parcela BE")

###Histograma

hist(dbh, col="purple", ylab = "Frecuencia", xlab = "Diametro (cm)", main = "Sitio BE", ylim = c(0, 10))

### Grafico de tallo y hoja

stem(dbh, scale =2)

dbh50 <- sample(dbh, 10, replace = TRUE)
  
set.seed(45)
dbh50 <- rnorm(50, mean = 10, sd= 2)
hist(dbh50)
dbh10 <- rnorm(10, mean = 10, sd= 2)
hist(dbh10)
dbh100 <- rnorm(100, mean = 10, sd= 2)
hist(dbh100)
dbh1000 <- rnorm(1000, mean = 10, sd= 2)
hist(dbh1000)


