# INTEGRANTES:
# SERGIO HENRIQUE RIBEIRO MATOS: 12211BCC038
# MARCEL FERNANDO LOBO DE FÉO: 12211BCC042



# QUESTÃO 1
#     ****       (A)       ****


library(ggplot2)

dados <- read.table("peixe_boi.txt", header = TRUE, sep = ";")

# Criar o gráfico de dispersão com ggplot2
ggplot(dados, aes(x = barcos, y = mortes)) +
  geom_point(color = "blue") +
  labs(
    title = "Relação entre Lanchas Registradas e Mortes de Peixes-Boi",
    x = "Número de Lanchas Registradas (milhares)",
    y = "Número de Mortes de Peixes-Boi"
  ) +
  theme_minimal()


# EXPLICAÇÃO DO GRÁFICO
# Neste gráfico de dispersão, vemos a relação entre o número de lanchas registradas e o número de mortes de peixes-boi. Como o gráfico indica uma tendência ascendente, isso sugere uma correlação positiva entre as variáveis, ou seja, que um aumento no número de lanchas pode estar relacionado a um aumento nas mortes de peixes-boi devido a colisões.




#     ****       (B)       ****




correlacao <- cor(dados$barcos, dados$mortes)
correlacao


# O resultado indica uma forte correlação positiva entre lanchas registradas e mortes de peixe-bois






#     ****       (C)       ****




# Ajustar o modelo de regressão linear
modelo <- lm(mortes ~ barcos, data = dados)

coef(modelo)
# Visualizar o resumo do modelo para análise dos testes de hipóteses
summary(modelo)



# EQUAÇÃO PEDIDA:

# EQUACAO_MORTES = -44.721 + 0.1322 * qtd_barcos



# acrescentando a reta de regressão no gráfico obtido da questão A
ggplot(dados, aes(x = barcos, y = mortes)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Relação entre Lanchas Registradas e Mortes de Peixes-Boi",
    x = "Número de Lanchas Registradas (milhares)",
    y = "Número de Mortes de Peixes-Boi"
  ) +
  theme_minimal()




#     ****       (D)       ****




residuos <- residuals(modelo)

# Realizar o teste de Shapiro-Wilk
shapiro_test <- shapiro.test(residuos)
shapiro_test



# função conferida no google.com para visualizar a normalidade dos resíduos
qqnorm(residuos)
qqline(residuos, col = "red")



# Com o resultado do teste de Shapiro-Wilk:

# Estatística W: 0.98727
# Valor-p: 0.9458

# ******   Interpretação   ********

# valor-p de 0.9458 é significativamente maior que o nível de significância comum de 0,05, indicando que não há evidências suficientes para rejeitar a hipótese nula de que os resíduos seguem uma distribuição Normal.

# Podemos concluir que os resíduos do modelo de regressão podem ser considerados normalmente distribuídos. Esse resultado sugere que os pressupostos de normalidade dos resíduos estão sendo atendidos, o que é favorável para a validade das inferências feitas a partir do modelo.






#     ****       (E)       ****




# Cálculo da previsão
barcos <- 800000 / 1000
mortes_previstas <- -44.72 + 0.132 * barcos
mortes_previstas


# Portanto, a previsão é de aproximadamente 61 mortes de peixes-boi com o limite de 800.000 lanchas.

# Podemos confiar na previsão porque:
  
# Normalidade dos Resíduos: O teste de Shapiro-Wilk confirmou que os resíduos são normalmente distribuídos, validando o modelo.
# Relação Significativa: A correlação entre lanchas e mortes de peixes-boi é forte e positiva.
# Valor Próximo aos Dados: O limite de 800.000 lanchas está dentro do intervalo dos dados analisados, tornando a previsão confiável.

# Esses pontos indicam que o modelo é adequado para essa previsão.






#     ****       (F)       ****





# razões para uma menor confiança na previsão

# 1 -> Extrapolação para Fora do Intervalo: O modelo foi ajustado com base em dados para o número de lanchas entre 447 mil e 978 mil. Prever para 200 mil lanchas é uma extrapolação, o que significa que estamos fazendo uma previsão para uma região onde o modelo não foi validado, aumentando a incerteza.


# 2 -> Mudanças no Comportamento: É possível que a relação entre o número de lanchas e as mortes de peixes-boi mude em valores muito baixos de lanchas (como 200 mil). A relação observada pode não ser linear ou pode não se manter a mesma fora do intervalo dos dados.

# Conclusão: Por esses motivos, a confiança na previsão diminui quando fazemos previsões para valores fora do intervalo dos dados observados.









#    QUESTÃO (2)

musicas <- read.table("musicas.txt", header=TRUE, sep = ";")
str(musicas)
summary(musicas)
set.seed(123)

musicas_padronizado <- scale(musicas[,c(-7,-8)])

modelo_kmeans <- kmeans(musicas_padronizado, centers = 4, nstart = 10)

aglomerados <- modelo_kmeans$cluster
musicas$aglomerado <- aglomerados

ggplot(musicas, aes(x = factor(aglomerado), fill = artista)) +
  geom_bar() +
  theme_minimal() +
  labs(x = "Aglomerado", y = "Número de músicas", fill = "Artista")

# Análise

# O gráfico de barras mostra a distribuição das músicas de cada artista dentro de cada um dos aglomerados criados pelo modelo kmeans.
# Com esse gráfico podemos entender:
# No aglomerado 1 o artista que tem mais músicas é Pato Fu, no aglomerado 2 Racionais MC´s tem mais musicas, no aglomerado 3 Cartola tem mais músicas e no aglomerado 4 quem tem mais músicas é o Pato Fu.
# Também podemos analisar que se um artista aparece muito em algum aglomerado específico, podemos concluir que as músicas daquele artista têm características em comum.
