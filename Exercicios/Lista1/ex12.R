# Definindo as sequências dos jogadores
steven_seq <- c(0, 1, 0)
garnit_seq <- c(0, 0, 1)

# Função que simula uma única partida
simular_partida <- function() {
  lancamentos <- sample(c(0, 1), 3, replace = TRUE) # Lança a moeda três vezes inicialmente
  
  while (TRUE) {
    # Verifica se Steven ou Garnit ganhou
    if (all(lancamentos[(length(lancamentos) - 2):length(lancamentos)] == steven_seq)) {
      return("steven")
    } else if (all(lancamentos[(length(lancamentos) - 2):length(lancamentos)] == garnit_seq)) {
      return("garnit")
    }
    
    # Lança a moeda mais uma vez
    lancamentos <- c(lancamentos, sample(c(0, 1), 1, replace = TRUE))
  }
}


num_simulacoes <- 10000

# Contar vitórias de Garnit
vitorias_garnit <- sum(replicate(num_simulacoes, simular_partida() == "garnit"))


media_vitorias_garnit <- vitorias_garnit / num_simulacoes

# Mostrar o resultado
cat("Média de vitórias de Garnit em 10.000 partidas:", media_vitorias_garnit, "\n")
