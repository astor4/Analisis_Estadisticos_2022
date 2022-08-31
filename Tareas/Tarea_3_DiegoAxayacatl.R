#Tarea 3


# Problemas ---------------------------------------------------------------

##Problema 1

xi <- c(6,4,1,3)
yi <- c(1,3,4,2)

sumat_xi <- sum(xi)
sumat_xi

sumat_yi <- sum(yi)
sumat_yi

sumat_yx <- sum(xi * yi)
sumat_yx      

help(prod)

prod_xi <- prod(xi, na.rm = FALSE)
prod_xi

prod_xy <- prod(xi * yi, na.rm = FALSE)
prod_xy

prod_xy2 <- prod(xi^2, yi^0.5, na.rm = FALSE)
prod_xy2


##Problema 2

### a.) el grupo A podria ser el que tenga una altura media mayor, ya que cuenta con un menor grupo de datos y los valores no son menores que algunos del grupo B
### b.) El grupo A tiene la mayor media y coincide con mi respuesta

Grupo_A <- c(80,90,90,100)

mean(Grupo_A)

Grupo_B <- c(60,65,65,70,70,70,75,75,80,80,80,80,80,85,100)

mean(Grupo_B)


##Problema 3

### Mientras saque un 76 sacaria un 80 de promedio en sus 4 examenes

promedio <- c(87,72,85,76)
mean(promedio)

##Problema 4

### El enunciado correcto es el b)

##Problema 5

###a) Se podrian utilizar histogramas

###b) La media de germinacion es de 7 y de las cajas petri es de 2.6

###c) La mediana de germinacion es 7 y la de las cajas petri es 3
germinaciones <- c(5,6,7,8,9)

cajas_petri <- c(1,3,5,3,1)

mean(germinaciones)

mean(cajas_petri)

median(germinaciones)

median(cajas_petri)

##Problema 6

set <- c(2, 2, 3, 6, 10)

###a) moda: 2, mediana: 3 y media: 4.6

getmoda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getmoda(set)

median(set)

mean(set)

###b)Suma 5 - moda:7 ,mediana:8 y media:9.6

set_5 <- c(7,7,8,11,15)

getmoda(set_5)

median(set_5)

mean(set_5)

###c)La moda se queda igual que la suma de la moda anterior y el 5 debido a que el numero de valores que se repiten no cambia.
###c)La mediana quedaria igual que la suma de la mediana anterior más el numero cinco, ya que es el valor más acercado al al centro de los datos.
###c)La media de igual manera solo se ve afectada en aumentar 5 unidades su valor ya que el aumento es uniforme en todos los valores.

###d) Multiplicar por 5 - moda:10 , mediana:15 y media:23

set_x5 <- c(10,10,15,30,50)

getmoda(set_x5)

median(set_x5)

mean(set_x5)

###e) La media, moda y mediana cambian en igual medida al ser multiplicadas por 5 ya que la cantidad de valores es constante.

##Problema 7

###a) 

set_7 <- c(9,7,7,6,6)

set_7.2 <- c(9,7,7,7,5)

median(set_7)
mean(set_7)

median(set_7.2)
mean(set_7.2)

###b)

set_7.3 <- c(7,4,9,7,2) 

set_7.4 <- c(4,7,6,8,9)

median(set_7.3)
mean(set_7.3)

median(set_7.4)
mean(set_7.4)



