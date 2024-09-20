# (a) Simule um passeio de 8 passos com Link começando na origem. A saída desta simulação deve apresentar o ponto do plano em que Link está após os 8 passos.



passeio <- function(passos) {
  posicao <- c(0, 0) # começa na origem
  
  for (i in 1:passos) {
    
    x <- sample(x = -1:1, size = 1, replace = TRUE)
    y <- sample(x = -1:1, size = 1, replace = TRUE)
    
    # Update the current position
    posicao <- posicao + c(x, y)
  }
  
  
  return(posicao)
}


resultado <- passeio(8)
print(resultado)



# (b) Agora replique dez mil vezes o experimento de (a) e determine a proporção de vezes em que Link retornou para a origem depois de 8 passos. 


# Função para replicar o experimento 10 mil vezes
replica_experimento <- function(passos) {
  voltou_a_origem <- 0  # Contador de quantas vezes Link voltou à origem
  
  
  for (i in 1:10000) {
    # Executa o passeio de 8 passos
    resultado <- passeio(passos)
    
    # Verifica se a posição final é a origem
    if (all(resultado == c(0, 0))) {
      voltou_a_origem <- voltou_a_origem + 1
    }
  }
  
  # Calcula a proporção de vezes que Link voltou à origem
  proporcao_voltou_origem <- voltou_a_origem / 10000
  return(proporcao_voltou_origem)
}

# Executa o experimento com 10 mil replicações e 8 passos
proporcao <- replica_experimento(8)


print(paste("Proporção de vezes que Link voltou à origem após 8 passos:", proporcao))




# A proporção resultante nos diz a frequência com que Link retorna à origem após um passeio aleatório de 8 passos. Esse valor pode fornecer insights sobre o comportamento de caminhadas aleatórias, que tendem a ter uma chance razoável de retornar ao ponto de origem após um pequeno número de passos, dependendo da dimensionalidade e das regras da caminhada.





# (c)



# Função principal onde o usuário entra com N
verifica_passos <- function(N) {
  # Verifica se N é ímpar
  if (N %% 2 != 0) {
    return("Impossível retornar à origem depois de um número ímpar de passos.")
  } else {
    # Se N for par, executa o experimento e calcula a proporção
    proporcao <- replica_experimento(N)
    return(paste("Proporção de vezes que Link retornou à origem depois de", N, "passos:", proporcao))
  }
}


resultado <- verifica_passos(8)  # Tente com um número par
print(resultado)

resultado <- verifica_passos(7)  # Tente com um número ímpar
print(resultado)

