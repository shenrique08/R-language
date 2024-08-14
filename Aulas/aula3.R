dados <- read.table(file = "titanic.txt", header = TRUE, sep = ",")


dados <- dados[, -1] # apgando os dados da primeira coluna
summary(dados)


# quantidade de sobreviventes
sum(dados[, 1])

taxa_sobreviventes <- sum(dados[, 1]/ length(dados[, 1]))
taxa_sobreviventes


# excluindo a primeira e a 12ª coluna
dados <- dados[, -c(1, 12)]


dados$Survived <- as.factor(dados$Survived)
summary(dados)

dados$Pclass <- as.factor(dados$Pclass)
dados$Sex <- as.factor(dados$Sex)

str(dados)

dados[1, 4]


# criando um conjunto separado de homens para analisar seus dados
homens <- dados[dados$Sex == "male",]
summary(homens)
table(homens$Survived)
barplot(table(homens$Survived))

mulheres <- dados[dados$Sex == "female",]
summary(mulheres)
table(mulheres$Survived)
barplot(table(mulheres$Survived))
