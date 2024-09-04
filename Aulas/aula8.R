library(ggplot2)
library(palmerpenguins)
library(class)

data(penguins)
pinguins <- penguins
str(pinguins)

pinguins <- pinguins[, -c(2, 7, 8)]
pinguins <- na.omit(pinguins)

n <- round(0.8 * nrow(pinguins))

indices_treino <- sample(1:nrow(pinguins), size = n, replace = FALSE)

treino <- pinguins[indices_treino,]
teste <- pinguins[-indices_treino,]


treino_padronizado <- scale(treino[, -1])
teste_padronizado <scale(teste[, -1])

classe_treino <- treino$species
classe_teste <- teste$species

