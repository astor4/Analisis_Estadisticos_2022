#Diego Axayacatl Gonzalez Cuellar
#03/10/22
#Examen Practico MCF

#Carga de datos

TCP <- "https://www.dropbox.com/s/3pi3huovq6qce42/obs.csv?dl=1"

suelo <- read.csv(TCP, header= TRUE, fileEncoding = "latin1")
head(suelo)

suelo$zone <- as.factor(suelo$zone)
suelo$wrb1 <- as.factor(suelo$wrb1)


# Actividad 1 -------------------------------------------------------------

summary(suelo$Clay1)

summary(suelo$Clay2)

summary(suelo$Clay5)

#P1: ¿Cual es la tendencia del contenido de Arcilla (Clay) con respecto a la
#profundidad? La tendencia del contenido de arcilla es aumentar en dependencia
#del aumento en la profundidad de donde se tomaron las muestras.


# Actividad 2 -------------------------------------------------------------

stem(suelo$Clay1)

#P2: ¿Los datos de contenido de arcilla siguen una distribucion simetrica
#o con sesgo? Los datos cuentan con un sesgo a la derecha positivo agrupandose
#la mayoria en el primer cuantil 

# Actividad 3 -------------------------------------------------------------

boxplot(suelo$Clay1, horizontal = T, col = "blue",
        main = "Contenido de Arcilla de 0 a 10 cm")

which(suelo$Clay1 >65)

clay1out <- subset(suelo, Clay1 > 65)

#P3: ¿Existe evidencia de outliers? Si existe evidencia, por lo menos tres datos

clay1out
#P4: ¿Puede identificar cuales observaciones son mediante una simple restriccion
#de datos? Si, las observaciones son 72,71 y 67.

# Actividad 4 -------------------------------------------------------------

mean(suelo$Clay1)

#P5: ¿Estime si el contenido de Arcilla promedio en los suelos tropicales de 30% 
#es significativamente diferente que la media observada en el campo experimental
#Tropenbos Cameroon Programme (TCP)?


# Actividad 5 -------------------------------------------------------------

CorC1C5 <- cor.test(suelo$Clay1, suelo$Clay5)
CorC1C5
#P6: ¿Existe una relación positiva, negativa o para nada relacionados,
#entre los perfiles superior (Clay1 ) e inferior (Clay5 )
#con el contenido de Arcilla? 
#Existe una relacion positiva entre los perfiles del suelo
#P7: ¿La correlación es estadísticamente significativa? Si es una correlacion 
#significativa segun el coeficiente

# Actividad 6 -------------------------------------------------------------

#P8: ¿Es posible determinar una ecuación significativa para predecir el 
#comportamiento del contenido de arcilla en el perfil inferior Clay5 ?
#Si es posible con una regresion y la ecuacion de y=alfa-beta*x

clay.lm <- lm(suelo$Clay5 ~ suelo$Clay1)
clay.lm

#P9: ¿Cuál es la ecuación final para predecir el comportamiento del contenido 
#de arcilla en el perfil más profundo (30-50 cm)?

# "esti.clay5 <- c(18.7586-(0.8289*x))"

summary(clay.lm)

#P10: ¿Son ambos parámetros alfa y beta significativos? Si son significativos

suelo$residual <- clay.lm$residuals

sum(suelo$residual**2)/145

#P11: ¿Cuál es el porcentaje de varianza explicado por el método aplicado? Es
# del 32.34%

# Actividad 7 -------------------------------------------------------------

#P12: ¿Existe una forma de identificar la variación entre las cuatro zonas que
#se encuentran en el estudio? Si mediante un analisis de varianza ANOVA

plot(suelo$Clay5 ~ suelo$zone)

#P13: ¿Existen indicios de que las cuatro zonas son diferentes en cuanto al
#contenido de arcilla en el perfil de 30 a 50 cm.? La zona uno varia en mayor
#medida con la zona 4, la zona 2 también varia entre la zona 3 y 4 y la zona 4
#es la que cuenta con mayor variacion respecto al resto de zonas y el contenido
#de arcilla en el perfil inferior

#P14 ¿Observa alguna tendencia en los datos en las diferentes zonas? 
#Si, la concentracion de arcilla en el perfil inferior decrece en dependencia
#de la zona, con mayores concentraciones en la zona 1 y menores en las zonas
#subsecuentes

by(suelo$Clay5, suelo$zone, summary)

# Actividad 8 -------------------------------------------------------------

suelo.aov <- aov(suelo$Clay5 ~ suelo$zone)
summary(suelo.aov)
suelo.aov

#P15 ¿Existen diferencias significativas entre el contenido de arcilla del
#perfil 30-50 cm y las zonas del estudio? Si existe diferencia significativa
#dado por el valor de p que es menor al valor de 0.05

#P16: En caso de existir diferencias ¿Cuáles zonas son diferentes
#estadísticamente entre si en el contenido de arcilla en el perfil de 30-50 cm?
#Entre la zona 1 y 2 no existen diferencias estadisticas, pero entre las demás
#zonas si.

TukeyHSD(suelo.aov, conf.level = 0.95)

plot(TukeyHSD(suelo.aov))
