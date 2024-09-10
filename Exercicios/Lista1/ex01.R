# (a) (10, 11, 12,..., 30)


vetorA <- c()
contador <- 10
index <- 1

while (contador < 31) {
  vetorA[index] <- contador
  contador <- contador + 1
  index <- index + 1
}


# (b) (30, 29, 28,..., 10)


vetorB <- c() # alocação dinâmica, que em alguns casos pode ser ineficiente
# vetorB <- numeric(21) -> pré alocação. Neste caso seria mais eficiente
contadorB <- 30
indexB <- 1

while (contadorB > 9) {
  vetorB[indexB] = contadorB
  contadorB <- contadorB - 1
  indexB <- indexB + 1
}

# (c) (10, 11, 12,..., 30, 29, 28,..., 10)

vetorC <- c(vetorA, vetorB)


