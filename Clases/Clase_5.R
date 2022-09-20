#Diego Axayacatl
#Clase 5
#06/09/22

#Pruebas de t de student

#T-student: de una muestra, dos muestras (dependientes o independientes)
#Se necesitan minimo 30 observaciones con una distribucion normal, se comparan
#las varianzas.
#Se deben plantear las hipotesis la nula dice que no hay una diferencia y la 
#alterna que si la hay entre los datos comparados.

viv <- read.csv("Clases/vivero.csv", header = T)
viv

summary(viv)

boxplot(viv$IE)

hist(viv$IE)


# Normalidad de los datos -------------------------------------------------

shapiro.test(viv$IE) #P-value menor a 0.05 (hipotesis alterna)

ks.test(viv$IE, "pnorm", mean= mean(viv$IE), sd= sd(viv$IE))


# T-student ---------------------------------------------------------------

t.test(viv$IE, mu=0.77) #mu= media teoretica

#intervalos de confianza n-1


data("chickwts")
summary(chickwts)

hist(chickwts$weight)
shapiro.test(chickwts$weight)
ks.test(chickwts$weight, "pnorm", mean=mean(chickwts$weight),
        sd=sd(chickwts$weight))

t.test(chickwts$weight, mu=250, alternative = "less") ## "less" para determinar
## si la hipotesis alternativa es menor, "greater" para mayor


# T-student de dos muestras -----------------------------------------------

#Dependiente: las observaciones son de la misma poblacion pero en un diferente 
#tiempo
#Independiente: las observaciones son de diferentes poblaciones o grupos 
#muestrales, comparables por un factor o tratamiento comun


# Dos muestras independientes ---------------------------------------------

#Ho : 

boxplot(viv$IE ~ viv$Tratamiento)

shapiro.test(viv$IE)

var.test(viv$IE ~ viv$Tratamiento) #prueba de varianzas

t.test(viv$IE ~ viv$Tratamiento, var.equal = T)

t.test(viv$IE ~ viv$Tratamiento) # cuando no se especifican las varianzas
#cambian los grados de libertad y por ende el p value

#cuando el t calculado es mayor que el t tabulado si hay diferencias signific.
#cuando el t calculado es menor al t tabulado no hay una diferencia signific.

invent <-read.csv("inventario.csv", header = T)

invent$Tratamiento <- as.factor(invent$Tratamiento)

invent$Fecha <- as.factor(invent$Fecha)

boxplot(invent$Diametro ~ invent$Tratamiento)

shapiro.test(invent$Diametro)

var.test(invent$Diametro ~ invent$Tratamiento)

t.test(invent$Diametro ~ invent$Tratamiento, var.equal =T)

#con Dcopa

boxplot(invent$Dcopa ~ invent$Tratamiento)

shapiro.test(invent$Dcopa)

var.test(invent$Dcopa ~ invent$Tratamiento)

t.test(invent$Dcopa ~ invent$Tratamiento, var.equal =T)


# Muestras dependientes ---------------------------------------------------

boxplot(invent$Kilogramo ~ invent$Fecha)

shapiro.test(invent$Kilogramo)

var.test(invent$Kilogramo ~ invent$Fecha)

t.test(invent$Kilogramo ~ invent$Fecha, paired = T) #paired indica que son 
#dependientes










