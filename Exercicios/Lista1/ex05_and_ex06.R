# Exercício 5. Crie um função no R que irá simular sucessivos lançamentos de um dado até que o número 4
# seja obtido pela segunda vez. A função deverá retornar o número de lançamentos que foram necessários até
# o 4 ser obtido pela segunda vez. Assim, se os sorteios foram 3, 6, 6, 5, 4, 2, 4 a função deverá retornar 7.



retorna_qtd_lancamentos_ate_encontrar_o_numero_7 <- function() {
  qtd_valor4 <- 0
  qtd_lancamentos <- 0
  while (qtd_valor4 < 2) {
    valor_dado <- sample(x = 1:6, size = 1, replace = TRUE)
    qtd_lancamentos <- qtd_lancamentos + 1
    if (valor_dado == 4)
      qtd_valor4 <- qtd_valor4 + 1
  }
  
  return(qtd_lancamentos)
}



quantidade_de_lancamentos <- retorna_qtd_lancamentos_ate_encontrar_o_numero_7()


# Exercício 6. Utilize a função do exercício anterior para replicar o experimento dez mil vezes. Para cada
# replicação, guarde o número de lançamentos num vetor chamado quantidades. Por fim, calcule a média de
# quantidades. Interprete o resultado obtido.

quantidades <- c()

contador <- 0
while (contador <= 10000) {
  quantidades[contador] <- retorna_qtd_lancamento_ate_encontrar_o_numero_7()
  contador <- contador + 1
}


media_quantidades <- mean(quantidades)


# interpretação:
# O resultado da média de 12.097 significa que, em média, são necessários aproximadamente 12 lançamentos de um dado para obter o número 4 duas vezes. Isso está de acordo com o fato de que a probabilidade de obter o número 4 em um único lançamento de um dado é de 1/6.




