# Laboratorio 1

## Parte I

### Gastos totales

300 + 240+ 1527 + 400 + 1500 + 1833

celular <- 300
transporte <- 240
comestibles <- 1527
gimnasio <- 400
alquiler <- 1500
otros <- 1833

### Gasto mensual total

G.total <-sum(celular, transporte, comestibles, gimnasio,
    alquiler, otros)

G.total

### Gasto total semestral

G.total.sem <- (G.total * 5)

G.total.sem

### Gasto total anual

G.total.anu <- (G.total.sem * 2)

G.total.anu

# Autoevaluacion 

gastos <- c(celular, transporte, comestibles, gimnasio,
            alquiler, otros)
ordenado <- sort(gastos, decreasing = TRUE)

barplot(gastos, main = "Gastos mensuales", ylab = "Pesos", ylim = c(0,2000), col = 3, )

barplot(ordenado, main = "Gastos mensuales", ylab = "Pesos", ylim = c(0,2000), col = rainbow (6),
        names.arg = c("Transporte", "Celular", "Gimnasio", "Alquiler", "Comestibles", "Otros"))

## Parte II

### Problema 1






