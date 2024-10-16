data(iris)

library(ggplot2)

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width))+geom_point()


cor(iris$Petal.Length, iris$Petal.Width)

cor(iris[, -5])


setosa <- iris[iris$Species == "setosa",]
cor(setosa$Sepal.Length, setosa$Petal.Width)


femur <- read.csv("femur.csv")
str(femur)
femur$genero <- as.factor(femur$genero)
femur <- femur[, -1]

mulheres <- femur[femur$genero == "Female",]
homens <- femur[femur$genero == "Male",]

cor(homens$altura, homens$femur)
mean(homens$altura)

cor(mulheres$altura, mulheres$femur)

ggplot(data = homens, aes(x = femur, y = altura))+geom_point()

modelo_linear <- lm(data = homens, formula = altura ~ femur)
modelo_linear

summary(femur)


