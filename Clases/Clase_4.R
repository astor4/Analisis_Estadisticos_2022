# Laboratorio 3

data("chickwts")

head(chickwts)

feeds <- table(chickwts$feed)
feeds

barplot(feeds)

barplot(feeds [order(feeds, decreasing = TRUE)], horiz = TRUE, las =1, col="#b30000")



# Descargar datos abiertos gob mexico -------------------------------------

fert <- "https://www.agricultura.gob.mx/sites/default/files/sagarpa/Publicaciones/datos_abiertos/2021/fertilizantes/6to-listado-fertilizantes-2021v2.csv"

ferti <- read.csv(fert, header= TRUE, fileEncoding = "latin1")

ferti

apoyos <- table(ferti$ESTADO)  ##Tabla de frecuencia de apoyos por estado

apoyos

barplot(apoyos, col="blue", ylim = c(0,8000), ylab= "No. de apoyos", xlab= "Estados")

mun <- table(ferti$MUNICIPIO)
mun

library(dplyr)

EsMun <- ferti %>%
  group_by(ESTADO, MUNICIPIO)%>%
  summarise(n = n())

Gro <- ferti %>%
  filter(ESTADO=="GUERRERO") %>% 
  group_by(MUNICIPIO) %>% 
  summarise(n = n()) %>% 
  filter(n >= 200) %>% 
  mutate(porciento = n/ sum(n)*100)
barplot(Gro$porciento)

write.csv(Gro,"Clases/Guerrero.csv", row.names = F)
