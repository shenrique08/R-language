# 0 Operações básicas
2 + 5 # soma
3 - 9 # subtração
2 * 5 # multiplicação
80/4 # divisão

10 %% 4 # resto da divisão


# 1 atribuição de valores

a <- 3*2
class(a)
9 * 3 -> c
a
c

d <- TRUE
e <- FALSE

# 2 O interpretador considera o TRUE como 1 e o FALSE como 0

f <- c + d + e
f

class(f)

x <- "teste"
class(x)

# 3 Criando Vetores
x1 <- c(3, 10, 78)
lenght(x1)
sum(x1)
mean(x1)
class(x1)


x1[1] # A linguagem R começa com indexação em 1, e não no 0

x1[c(1, 2, 3)] # acessando posições
x2 <- c(x1, 67, 90) # concatenando vetores
x2
x2 < 70
soma <- sum(x2<70)
soma

sum(x2[x2<70])


x2 == 72
! TRUE # negação de TRUE
x2 != 10

TRUE & TRUE #e
TRUE | FALSE #ou


# gerando números aleatórios de um dado, por exemplo

random_dado <- sample(1:6, 10000000, replace = TRUE)
random_dado

count_3 <- sum(random_dado == 3)
chance_de_ser_3 <- count_3 / length(random_dado)
chance_de_ser_3


