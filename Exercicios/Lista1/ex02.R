# (a) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,..., 2, 4, 6, 8), em que há dez ocorrências do número 2.


vetor1 <- rep(seq(2, 8, by = 2), times = 10) 


# (b) Crie o vetor (2, 4, 6, 8, 2, 4, 6, 8,..., 2, 4, 6, 8, 2), em que há onze ocorrências do número 2 e dez ocorrências dos números 4, 6 e 8.


vetor2 <- c(vetor1, 2)


