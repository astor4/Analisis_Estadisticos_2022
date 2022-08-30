# Tarea 1

## Problema 1:

pinus <- 3140
mezquite <- 1453
encinos <- 450
teka <- 1200
juniperos <- 720

superficie <- c(pinus, mezquite, encinos, teka, juniperos)

barplot(superficie)

creciente <- sort(superficie, decreasing = FALSE)

decreciente <- sort(superficie, decreasing = TRUE)

barplot(decreciente, main = "Reforestación", ylab = "Superficie reforstada por especie (ha)", ylim = c(0,3500), col = 6, )

barplot(decreciente, main = "Reforestación", ylab = "Superficie reforstada por especie (ha)", ylim = c(0,3500),
        col = 6, names.arg = c("Pinus", "Mezquite", "Teka", "Juniperos", "Encinos"))

media_superficie <- mean(superficie)

media_superficie

## Problema 2:

germinacion <- c(4, 1, 6, 2, 4, 2, 4, 2, 4, 6, 3, 5, 3, 2, 5, 4, 0, 5, 4,
2, 4, 5, 3, 5, 3, 5, 4, 3, 6, 2)


media_germinacion <- mean(germinacion)

media_germinacion

help(sd)

sd(germinacion, na.rm = FALSE)

## Problema 3:

altura <- c(38, 14, 44, 11, 9, 21, 39, 28, 41, 4, 35, 24, 36, 12,
            20, 31, 24, 25, 10, 21, 11, 36, 37, 20, 26)

media_altura <- mean(altura)

media_altura

sd (altura)
