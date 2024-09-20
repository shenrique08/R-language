# Importar os dados
dados <- read.csv("dados.txt", sep = ";", header = TRUE)
head(dados)


# (a)


library(ggplot2)

# Gráfico de barras para a variável Genero
ggplot(dados, aes(x = Genero)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequência por Gênero", x = "Gênero", y = "Frequência") +
  theme_minimal()


# (b)

# Histograma da idade com 8 bins
ggplot(dados, aes(x = Idade)) +
  geom_histogram(bins = 8, fill = "steelblue", color = "black") +
  labs(title = "Histograma da Idade das Vítimas", x = "Idade", y = "Frequência") +
  theme_minimal()


# Histograma da idade separado por gênero
ggplot(dados, aes(x = Idade, fill = Genero)) +
  geom_histogram(bins = 8, color = "black", position = "dodge") +
  labs(title = "Histograma da Idade das Vítimas por Gênero", x = "Idade", y = "Frequência") +
  theme_minimal()


# (c)

# Boxplot da variável idade
ggplot(dados, aes(x = "", y = Idade)) +
  geom_boxplot(fill = "steelblue") +
  labs(title = "Boxplot da Idade das Vítimas", x = "", y = "Idade") +
  theme_minimal()


# (d)

# Gráfico de barras para o local da morte
ggplot(dados, aes(x = LocalDaMorte)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Frequência por Local da Morte", x = "Local da Morte", y = "Frequência") +
  theme_minimal()



# (e)

# Gráfico de barras para o ano da morte
ggplot(dados, aes(x = as.factor(AnoDaMorte))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribuição das Mortes por Ano", x = "Ano da Morte", y = "Frequência") +
  theme_minimal()



# (f)


# (f) Parágrafo sobre o padrão e o perfil das vítimas.

# Baseado nas análises anteriores:
  
  #A maioria das vítimas eram mulheres idosas, com idades concentradas entre 70 e 90 anos, conforme revelado no histograma e no boxplot. A maioria das mortes ocorreu na própria casa das vítimas, indicando que Harold Shipman provavelmente cometeu muitos dos assassinatos durante visitas domiciliares. A distribuição dos anos de morte mostra que houve um aumento no número de vítimas a partir da década de 1980. O padrão das mortes sugere que Shipman escolheu predominantemente vítimas vulneráveis, como idosos, especialmente mulheres, e utilizou o ambiente doméstico para cometer os assassinatos.
