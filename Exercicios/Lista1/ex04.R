


# vetor vazio para guardar os números sorteados
numeros_sorteados <- c()

# sorteando os números de 1 a 100, com tamanho do vetor == 100
numeros_sorteados <- sample(x = 1:100, size = 100, replace = FALSE)



# será guardado o index de cada bola sorteada
posicao_do_resultado <- sample(x = 1:100, size = 40, replace = TRUE)

resultado <- c()
contador <- 40
index <- 1

while (contador > 0) {
  resultado[index] <- numeros_sorteados[posicao_do_resultado[index]]
  contador <- contador - 1
  index <- index + 1
}




cont2 <- 1
conta_pares <- 0
maiores_que_70 <- 0
guarda_pos_impares <- c()
index <- 1
while (cont2 < 41) {
  # (a) Quantas bolas pares foram sorteadas?
  if (resultado[cont2] %% 2 == 0) {
    conta_pares <- conta_pares + 1
  } else {
    # (c) Em quais retiradas (posições) foram sorteadas as bolas ímpares?
    guarda_pos_impares[index] <- cont2
    index <- index + 1
  }
  
  # (b) Quantas bolas maiores do que 70 foram sorteadas?
  if (resultado[cont2] > 70)
    maiores_que_70 <- maiores_que_70 + 1
  
  cont2 <- cont2 + 1
}




