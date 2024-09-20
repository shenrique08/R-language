dados <- read.csv("primatas.txt", sep = ":", header = TRUE)

# Visualizar as primeiras linhas dos dados
head(dados)




# (b)

library(ggplot2)

# Gráfico de barras para contar as espécies
ggplot(dados, aes(x = especie)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequência de Espécies (Bonobos vs Chimpanzés)", x = "Espécie", y = "Contagem") +
  theme_minimal()


# Gráfico de barras mostrando a frequência de machos e fêmeas por espécie
ggplot(dados, aes(x = especie, fill = genero)) +
  geom_bar(position = "dodge") +
  labs(title = "Frequência de Machos e Fêmeas por Espécie", x = "Espécie", y = "Contagem") +
  theme_minimal()



# (c)

# Filtrar apenas bonobos
bonobos <- subset(dados, especie == "bonobo")

# Gráfico de comparação de altura e peso entre machos e fêmeas dos bonobos
ggplot(bonobos, aes(x = genero, y = peso, fill = genero)) +
  geom_boxplot() +
  labs(title = "Comparação do Peso entre Machos e Fêmeas dos Bonobos", x = "Gênero", y = "Peso (libras)") +
  theme_minimal()

ggplot(bonobos, aes(x = genero, y = altura, fill = genero)) +
  geom_boxplot() +
  labs(title = "Comparação da Altura entre Machos e Fêmeas dos Bonobos", x = "Gênero", y = "Altura (cm)") +
  theme_minimal()


# Filtrar apenas chimpanzés
chimpanzes <- subset(dados, especie == "chimpanze")

# Gráfico de comparação de altura e peso entre machos e fêmeas dos chimpanzés
ggplot(chimpanzes, aes(x = genero, y = peso, fill = genero)) +
  geom_boxplot() +
  labs(title = "Comparação do Peso entre Machos e Fêmeas dos Chimpanzés", x = "Gênero", y = "Peso (libras)") +
  theme_minimal()

ggplot(chimpanzes, aes(x = genero, y = altura, fill = genero)) +
  geom_boxplot() +
  labs(title = "Comparação da Altura entre Machos e Fêmeas dos Chimpanzés", x = "Gênero", y = "Altura (cm)") +
  theme_minimal()


# (d)


# Filtrar apenas fêmeas
femeas <- subset(dados, genero == "femea")

# Gráfico de comparação de altura e peso entre fêmeas dos bonobos e dos chimpanzés
ggplot(femeas, aes(x = especie, y = peso, fill = especie)) +
  geom_boxplot() +
  labs(title = "Comparação do Peso entre Fêmeas dos Bonobos e Chimpanzés", x = "Espécie", y = "Peso (libras)") +
  theme_minimal()

ggplot(femeas, aes(x = especie, y = altura, fill = especie)) +
  geom_boxplot() +
  labs(title = "Comparação da Altura entre Fêmeas dos Bonobos e Chimpanzés", x = "Espécie", y = "Altura (cm)") +
  theme_minimal()


# Filtrar apenas machos
machos <- subset(dados, genero == "macho")

# Gráfico de comparação de altura e peso entre machos dos bonobos e dos chimpanzés
ggplot(machos, aes(x = especie, y = peso, fill = especie)) +
  geom_boxplot() +
  labs(title = "Comparação do Peso entre Machos dos Bonobos e Chimpanzés", x = "Espécie", y = "Peso (libras)") +
  theme_minimal()

ggplot(machos, aes(x = especie, y = altura, fill = especie)) +
  geom_boxplot() +
  labs(title = "Comparação da Altura entre Machos dos Bonobos e Chimpanzés", x = "Espécie", y = "Altura (cm)") +
  theme_minimal()


# (e) Diferenças entre bonobos e chimpanzés

# Baseado nas análises anteriores:
  
  # A análise dos dados mostra que tanto machos quanto fêmeas dos chimpanzés tendem a ser mais altos e mais pesados que os bonobos. Dentro de cada espécie, machos são geralmente mais altos e pesados que as fêmeas. Essa diferença é mais pronunciada nos chimpanzés, que têm uma sociedade mais hierárquica e dominada por machos. Por outro lado, os bonobos, conhecidos por sua sociedade mais pacífica e matriarcal, apresentam menor dimorfismo sexual em relação ao peso e altura.


# (f)


library(rpart)

# Criar um modelo de árvore de decisão para prever a espécie
modelo <- rpart(especie ~ altura + peso + genero, data = dados, method = "class")

# Mostrar o resumo do modelo
summary(modelo)

# Visualizar a árvore de decisão

library(rpart.plot)
rpart.plot(modelo)

# Previsão e cálculo da acurácia
previsao <- predict(modelo, dados, type = "class")
acuracia <- mean(previsao == dados$especie)
cat("Acurácia do modelo:", acuracia, "\n")

