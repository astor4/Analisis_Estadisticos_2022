#Laboratorio 4


# Importar datos ----------------------------------------------------------

esp.url <- paste0("https://raw.githubusercontent.com/mgtagle/",
                  "PrincipiosEstadistica2021/main/cuadro1.csv")
inventario <- read.csv(esp.url)

inventario

str(inventario)     ##Estructura general de los datos

dim(inventario)     ##Dimencion de los datos (variables y obser.)

head(inventario)    ##Muestra de las primeras n filas (6)

tail(inventario)    ##Muestra de las ultimas n filas (6)

names(inventario)   ##Nombre de las columnas 

colnames(inventario)##Igual que names() te muestra el nombre de las columnas

summary(inventario) ##Resumen estadistico de las variables, muestra el dato 
                    ##minimo, el priemer cuantil, mediana, media, tercer cuantil 
                    ##y el numero maximo

# dimensiones (num filas y columnas)

dim(inventario)


# nombre de las primeras cinco columnas

names(inventario[,1:5])


#Resumen estadistico basico de las columnas 3 a 5 

summary(inventario[,3:5])


help("is.factor")

is.factor(inventario$Posicion) ##Esta funcion te ayuda a saber si un vector 
                               ##es un Factor o no.

inventario$Posicion <- factor(inventario$Posicion) ##Esta funcion la utilizamos 
is.factor(inventario$Posicion)          ##para convertir un vector x en factor.


summary(inventario[,3:5])


# Tablas de frecuencia ----------------------------------------------------

frec_position <- table(inventario$Posicion) ##La funcion table() te ayuda a 
frec_position                               ##crear una tabla de frecuencias


propor_position <- frec_position / sum(frec_position) 
propor_position##Para determinar la frecuencia relativa se puede utilizar este 
               ##vector con dos funciones


porc_position = 100 * propor_position 
porc_position ##Si se requiere convertir en porcentaje el vector de frecuencia 
              ##relativa debe multiplcarse por 100

# Graficas barplot y pie --------------------------------------------------

##Graficas de barras(funcion barplot())

barplot(frec_position,    ##Se hara un grafico del vector de frecuencia relativa
        las = 1,          ##Muestra la frecuencia perpendicula al eje y
        border = NA,      ##Elimina el borde negro al rededor de las barras
        cex.names = 0.7)  ##Reduce los tamaños de las etiquetas de categoria 
                          ##(para que quepan en el grafico)

##Grficas de pastel(funcion pie())

library(viridis) ##Libreria de R que provee paletas de colores amigables
pie(frec_position, col = viridis(4)) ##El parentecis indica el numero de colores
                                     ##a utilizar de la paleta seleccionada

pie(porc_position, col = viridis(4),           ##Para mostras los porcentajes en 
    labels = paste(levels(inventario$Posicion),##las etiquetas se puede utilizar
                   round(porc_position,2),"%"))##estas funciones


# Autoestudio -------------------------------------------------------------

inventario$Especie <- factor(inventario$Especie)
is.factor(inventario$Especie)

frec_especie <- table(inventario$Especie)
frec_especie

propor_especie <- frec_especie / sum(frec_especie) 
propor_especie

porc_especie <- 100 * propor_especie
porc_especie

barplot(frec_especie, las = 1, border = NA, cex.names = 0.7)

pie(frec_especie, col = viridis(3))

pie(frec_especie, col = viridis(3),
    labels = paste(levels(inventario$Especie),
                   round(porc_especie,2),"%"))


# Histograma --------------------------------------------------------------

diam_hist <- hist(inventario$Diametro, las= 1, col = "#9999ff")

diam_hist ##breaks: puntos de ruptura (corte) de los intervalos de clase
          ##counts: número de observaciones en cada categoría
          ##density: densidad
          ##mids: punto central del intervalo
          ##xname: nombre del objeto (variable) que se esta graficando
          ##equidist: ¿Los categorías tienen el mismo ancho?
          ##attr: Tipo de clase

diam_hist$breaks ##Muestra los intervalos de clase

hist_diam1 <- hist(inventario$Diametro, xaxt = "n",
               breaks = c(6,8,10,12,14,16,18,20,22,24),
               col = "#00cc00",
               xlab = "Diametros (cm)",
               ylab = "Frecuencias",
               main = "Diametros del inventario",
               las = 1,
               ylim = c(0,14))
               axis(1,diam_hist$mids)

# Autoestudio Altura ------------------------------------------------------

alt_hist <- hist(inventario$Altura, las= 1, col = "#ffff00")

alt_hist

alt_hist$breaks

hist_alt1 <- hist(inventario$Altura, xaxt = "n",
                 breaks = c(8,10,12,14,16,18,20,22),
                 col = "#ff0000",
                 xlab = "Altura (m)",
                 ylab = "Frecuencias",
                 main = "Alturas del inventario",
                 las = 1,
                 ylim = c(0,14))
                 axis(1,alt_hist$mids)


                 
