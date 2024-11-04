dados <- read.csv("grilo.txt", header = TRUE, sep = ",")
str(dados)

library(ggplot2)
ggplot(data = dados, aes(x = frequencia))+geom_histogram(bins = 10)


cor(dados)

ggplot(data = dados, aes(x = temperatura, y = frequencia))+geom_point()+theme_minimal()+geom_smooth(method = "lm")

modelo_linear <- lm(formula = frequencia ~ temperatura, data = dados)

modelo_linear
summary(dados$temperatura)

w <- data.frame(temperatura = c(21, 23.6, 30.9))

predict(modelo_linear, newdata = w)
library(palmerpenguins)
cor(dados$bill_length_mm, dados$bill_depth_mm)
summary(dados)

dados <- na.omit(dados)
names(dados)
cor(dados[, 3:6])

ggplot(data = dados, aes(x = flipper_length_mm, y = body_mass_g, color = species))+geom_point()+geom_smooth(method = "lm")


















femur <- read.csv("femur.csv")
str(femur)

