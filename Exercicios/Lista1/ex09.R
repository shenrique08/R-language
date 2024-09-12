# Função para simular o lançamento de dois dados
lancar_dados <- function() {
  dado1 <- sample(1:6, 1)  
  dado2 <- sample(1:6, 1)  
  return(dado1 + dado2)    
}


# Função que simula uma partida de Craps
simular_partida_craps <- function() {
  soma_inicial <- lancar_dados()  # Primeira jogada
  
  # Regras para a primeira jogada
  if (soma_inicial == 7 || soma_inicial == 11) {
    return(1)  # Vitória imediata
  } else if (soma_inicial == 2 || soma_inicial == 3 || soma_inicial == 12) {
    return(0)  # Derrota imediata
  }
  
  # Caso contrário, continua jogando até obter 7 (derrota) ou a soma inicial (vitória)
  while (TRUE) {
    nova_soma <- lancar_dados()
    
    if (nova_soma == 7) {
      return(0)  # Derrota
    } else if (nova_soma == soma_inicial) {
      return(1)  # Vitória
    }
  }
}

# Simulação do experimento 100.000 vezes
num_experimentos <- 100000
vitorias <- 0 

for (i in 1:num_experimentos) {
  vitorias <- vitorias + simular_partida_craps()
}


# Calcula a proporção de vitórias
proporcao_vitorias <- vitorias / num_experimentos
print(proporcao_vitorias)
