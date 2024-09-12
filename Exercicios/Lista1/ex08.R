# Exercício 8. Michael Scott é gerente regional da empresa Dunder Mufflin. Para as festividades de fim de ano, Michael propôs aos funcionários Dwight Schrute, Jim Halpert, Kevin Malone e Creed Bratton a realização de um amigo oculto entre eles. Consideraremos que o sorteio do amigo oculto deu errado quando uma pessoa sortear ela mesma (Michael tira Michael, por exemplo). Simule o sorteio do amigo oculto. Se ele deu certo, atribua o valor 1; caso contrário, atribua o valor 0 (zero). Em seguida, replique este experimento cem mil vezes e calcule a proporção de vezes que o amigo oculto deu errado.




amigo_secreto <- function(nomes) {
  sorteio <- sample(nomes, length(nomes), replace = FALSE)  # Sorteia todos de uma vez
  return(sorteio)
}

nomesSorteio <- c("Michael Scott", "Dwight Schrute", "Jim Halpert", "Kevin Malone", "Creed Bratton")

qtd_fracasso <- 0  


# Simula o experimento 100.000 vezes
for (i in 1:100000) {
  sorteio <- amigo_secreto(nomesSorteio)
  
  # Verifica se alguém tirou a si mesmo
  if (any(nomesSorteio == sorteio)) {
    qtd_fracasso <- qtd_fracasso + 1
  }
}


# Calcula a proporção de vezes que deu errado
proporcao_fracasso <- qtd_fracasso / 100000
print(proporcao_fracasso)





