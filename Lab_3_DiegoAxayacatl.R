###Laboratorio 3


# Parte 1: Importar datos -------------------------------------------------
getwd()

trees <-read.csv("DBH_1.csv", header = TRUE)

head(trees)


# Insertar datos directamente en la cosnola -------------------------------


dbh <- c(16.5, 25.3, 22.1, 17.2, 16.1, 8.1, 34.3, 5.4, 5.7, 11.2, 24.1,
         14.5, 7.7, 15.6, 15.9, 10, 17.5, 20.5, 7.8, 27.3,
         9.7, 6.5, 23.4, 8.2, 28.5, 10.4, 11.5, 14.3, 17.2, 16.8)


prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url, header= TRUE, fileEncoding = "latin1")
head(profepa)

prof_url_2 <- paste0("http://www.profepa.gob.mx/innovaportal/",
                     "file/7635/1/accionesInspeccionfoanp.csv")
profepa2 <- read.csv(prof_url_2, header= TRUE, fileEncoding = "latin1")
head(profepa2)

# Datos de URL seguras (https) ---------------------------------------------

library(repmis)

conjunto <- source_data("https://www.dropbox.com/s/hmsf07bbayxv6m3/cuadro1.csv?dl=1")

#SHA-1 hash of the downloaded data file is:
#2bdde4663f51aa4198b04a248715d0d93498e7ba

head(conjunto) 

library(readr)
file <- paste0("https://raw.githubusercontent.com/mgtagle/",
               "202_Analisis_Estadistico_2020/master/cuadro1.csv")
inventario <- read_csv(file)
head(inventario)


prof_url <- "http://www.profepa.gob.mx/innovaportal/file/7635/1/accionesInspeccionfoanp.csv"
profepa <- read.csv(prof_url, header= TRUE, fileEncoding = "latin1")
head(profepa)

# Parte 2: Operaciones con la base de datos -----------------------------------------------------------------

dbh_media <- mean(trees$dbh)
dbh_media
dbh_sd <- sd(dbh)
dbh_sd

# Seleccion mediante restricciones ----------------------------------------

sum(trees$dbh <10)
which(trees$dbh <10)

trees.13 <- trees[!(trees$parcela=="2"),]
trees.13

# Seleccion de una submuestra ---------------------------------------------

trees.1 <- subset(trees, dbh <= 10)
head(trees.1)

mean(trees$dbh)

mean(trees.1$dbh)

hist(trees$dbh, ylab = "Frecuencia", xlab = "DBH", main = "Muestra original trees")

hist(trees.1$dbh, ylab = "Frecuencia", xlab = "DBH", main = "dbh <10 cm. trees.1")

# Parte 3: Representacion grafica -----------------------------------------

mamiferos <- read.csv("https://www.openintro.org/data/csv/mammals.csv")
mamiferos

hist(mamiferos$total_sleep, xlim = c(0,20), ylim = c(0,14),
     main = "Total de horas sueño de las 39 especies",
     xlab = "Horas sueño", ylab = "Frecuencia", las = 1,
     col = "#e63900")

# Barplot o grafico de barras ---------------------------------------------

data("chickwts")
head(chickwts [c(1:2,42:43, 62:64), ])

feeds <- table(chickwts$feed)
feeds

barplot(feeds)

barplot(feeds[order(feeds, decreasing = TRUE)], horiz = T, las = 1, col = "#ffff80", xlab = "Numero de Pollos", main = "Frecuencias por tipos de alimentacion" )

