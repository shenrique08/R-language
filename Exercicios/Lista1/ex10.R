# (a) Função para verificar se Luke chegou em casa ou caiu no precipício


retornaPasseio <- function(L) {
  N <- 20
  resultado <- L
  if (L > 0 && L < 20) {
    while (TRUE) {
      if (resultado == 0)  # Luke caiu no precipício
        return(0)
      else if (resultado == 20)  # Luke chegou em casa
        return(1)
      
      # Simulando o lançamento de cara ou coroa
      cara_ou_coroa <- sample(x = 1:2, size = 1, replace = TRUE)
      
      # Se cara (1), ele avança, se coroa (2), ele recua
      if (cara_ou_coroa == 1)
        resultado <- resultado + 1
      else
        resultado <- resultado - 1
    }
  }
}



# (b) Função para replicar o passeio 10 mil vezes

proporcao_chegou_em_casa <- function(L) {
  chegou_em_casa <- 0
  for (i in 1:10000) {
    if (retornaPasseio(L) == 1)
      chegou_em_casa <- chegou_em_casa + 1
  }
  
  # Retorna a proporção de vezes que Luke chegou em casa
  return(chegou_em_casa / 10000)
}



# (c) Gerar proporções para L = 1, 2, ..., 19 e plotar o gráfico
proporcoes <- c()

# Armazena as proporções para cada valor de L
for (L in 1:19) {
  proporcoes[L] <- proporcao_chegou_em_casa(L)
}

# Plotar o gráfico
plot(1:19, proporcoes, type = "b", col = "blue", pch = 19, xlab = "L (Posição Inicial)", ylab = "Proporção de Chegada em Casa", main = "Proporção de Chegada em Casa para Diferentes Valores de L")






