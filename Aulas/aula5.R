# passo 1 -> dividir em treino e teste

library(ggplot2)

set.seed(1711)
iris <- iris[sample(nrow(iris)),]

n <- round(0.8 * nrow(iris))

treino <- iris[1:n,]
teste <- iris[-(1:n),]



ggplot(data = treino, mapping = aes(x = Species))+geom_bar()



ggplot(data = treino, mapping = aes(x = Petal.Length))+geom_histogram(bins = 20, fill= "lightblue")+theme_minimal()


ggplot(data = treino, mapping = aes(y = Petal.Length))+geom_boxplot()+facet_wrap(~Species)

ggplot(data = treino, aes(x = Petal.Length, y = Petal.Width, color = Species))+geom_point()


summary(treino$Petal.Length)


resultados <- c()

for (j in 1:nrow(teste)) {
  if (teste$Petal.Length[j] < 2.5) {
    resultados[j] <- "setosa"
  } else {
    if (teste$Petal.Width[j] < 1.75) {
      resultados[j] <- "versicolor"
    } else {
      resultados[j] <- "virginica"
    }
  }
}


teste$Species == resultados
mean(teste$Species == resultados)