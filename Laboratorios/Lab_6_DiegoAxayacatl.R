##Laboratorio 6
##Diego Axayacatl Gonzalez Cuellar
##1610823

##Variables y datos en R

# NBA Datos ---------------------------------------------------------------


NBAd <- matrix(0,15,8)
colnames(NBAd) <- (c("Western Conference","W","L","W/L%","GB","PS/G","PA/G",
                     "SRS"))
rownames(NBAd)<- (c("1","2","3","4","5","6","7","8","9","10","11","12","13",
                    "14","15"))
teams <- c("UJ","PS","DN","LAC","DM","PTB","LAL","MG","GSW","SAS","NOP","SK",
           "MT", "OCT","HR")
NBAd [, 1] <- teams

wins <- c(52,51,47,47,42,42,42,38,39,33,31,31,23,22,17)
NBAd [,2] <- wins

loss <- c(20,21,25,25,30,30,30,34,33,39,41,41,49,50,55)
NBAd [,3] <- loss

wl <- wins / (wins + loss)
NBAd [,4] <- wl

GB <- c(0,1,5,5,10,10,10,14,13,19,21,21,29,30,35) # tambien se puede usar
NBAd [,5] <- GB                                   # "wins[1] - wins"

PSG <- c(116.4,115.3,115.1,114,112.4,116.1,109.5,113.3,113.7,111.1,114.6,113.7,
         112.1,105,108.8)
NBAd [,6] <- PSG

PAG <- c(107.2,109.5,110.1,107.8,110.2,114.3,106.8,112.3,112.7,112.8,114.9,
         117.4,117.7,115.6,116.7)
NBAd [,7] <- PAG

SRS <- c(8.97,5.67,4.82,6.02,2.26,1.81,2.77,1.07,1.10,-1.58,-0.20,-3.45,-5.25,
         -10.13,-7.50)
NBAd [,8] <- SRS


# Manipulacion de vectores: subconjuntos ----------------------------------


# Se utilizan los corchetes para extraer elementos de un vector

#primer elemento de "wins"
wins[1]

#tercer elemento de "loss"
loss[3]

#ultimo nombre en "teams"
teams[15]

#Otras funciones para ver elementos de los vectores son las siguientes:
length(teams) #te dice el largo del vector o la cantidad de datos
sort(wins, decreasing = TRUE) #ordena los valores de forma creciente o
#decreciente
rev(wins) #invierte los valores


# Subconjunto con índices lógicos -----------------------------------------


#victorias de Utah Jazz
wins[teams == 'UJ']

#equipos con victorias > 40
teams[wins > 40]

#nombre de los equipos con derrotas entre 10 y 29
teams[loss >= 10 & loss <= 29]


# Factores y variables cualitativas ---------------------------------------


#vector numerico

num_vector <- c(1, 2, 3, 1, 2, 3, 2)

#crear un factor apartir de num_vector
first_factor <- factor(num_vector)
first_factor

#factor de teams
teams = factor(teams)
teams


# Secuencias --------------------------------------------------------------


#operador dos puntos :
1:5
1:10
-3:7
10:1

#funcion secuencia
seq(from = 1, to = 10)
seq(from = 1, to = 10, by = 1)
seq(from = 1, to = 10, by = 2)
seq(from = -5, to = 5, by = 1)


# Vectores repetidos ------------------------------------------------------


rep(1, times = 5) #repetir 1 cinco veces

rep(c(1, 2), times = 3) #repetir 1 y 2 tres veces

rep(c(1, 2), each = 2) #repetir 1 y 2 dos veces cada uno

rep(c(1, 2), length.out = 5) #repite 1 y 2 hasta 5 veces

rep(c(3, 2, 1), times = 3, each = 2) # repite 3, 2 y 1 cada uno 2 veces, en 3
                                     # ocaciones

# De vectores a estructura tabular (data frame) ---------------------------


dat = data.frame(Teams = teams, #con esta funcion se cre aun data frame
                 Wins = wins,Losses = loss,WLperc = wl) 
dat 

dat$Teams # $ se utiliza para extraer datos de una columna de un dataset

#ademas se pueden utilizar notaciones de corchetes en la columna, como se hace
#con los vectores

dat$Wins[1] #para extraer el primer valor de "Wins"
dat$Wins[5] #para extraer el quinto valor de "Wins"

#tambien se pueden hacer subconjuntos logicos
dat$Wins[dat$Teams == 'UJ'] #extrae las victorias del equipo seleccionado
                            #dentro del data frame
dat$Teams[dat$Wins > 40] #extrae los valores  con victorias mayores a 40

dat$Teams[dat$Losses >= 10 & dat$Losses <= 29] #extrae los equipos entre 10 y 29
                                               #derrotas

# Mi turno ----------------------------------------------------------------

#Los vectores ya se habian creado al principio

wins[1] - wins

posiciones <- data.frame(Teams = teams,
                            Wins = wins,
                            Losses = loss,
                            WLporc = wl,
                        GamesBehind = GB,
                        PointsScored = PSG,
                        PointsAgainst = PAG,
                        Rating = SRS)
posiciones

sort(posiciones$PointsScored, decreasing = FALSE) #creciente
sort(posiciones$PointsScored, decreasing = TRUE) #decreciente

