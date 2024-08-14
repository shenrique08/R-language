numeros <- c(5, 23, 543, 32, 23)
numeros

numeros[1]
?c

summary(numeros)


nome <- "Henrique"
sobrenome <- "Matos"


library(stringr)
nome_completo <- str_c(nome, " ",  sobrenome)
nome_completo



#--------------------------------------------

a <- c(1, 2, 5)
b <- c(10, 12)
d <- c(a, 6)

3*a
3+a

a+b
d+b


# Laços de repetição

x <- 0
for (i in 1:20) {
  x <- x + 1
}

# ---------------------------------------------------------------------

# sorteando números de 1 a 365, como se fosse o aniversário de alguém
# e verificando se há alguém que fez o aniversário na mesma data

# ---------------------------------------------------------------------


aniversarios <- sample(x = 1:365, size = 10, replace = TRUE)
aniversarios
any(duplicated(aniversarios))



# função que calcula aprobabilidade de haver alguém entre n pessoas que faz
# aniversário no mesmo dia

calcula_probabilidade <- function(n) {
  resultados <- c()
  
  for (j in 1:10000) {
    aniversarios <- sample(x = 1:365, size = n, replace = TRUE)
    resultados[j] <- any(duplicated(aniversarios))
    
  }
  
  return(mean(resultados))
  
}

calcula_probabilidade(n = 366)



# verificando quantos anos levaria para acertar 4 números na mega-sena

bilhete <- c(4, 5, 12, 43, 21, 34)
sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
bilhete %in% sorteio


semanas <- 0
acertos <- 0

while (acertos < 4) {
  sorteio <- sample(x = 1:60, size = 6, replace = FALSE)
  acertos <- sum(bilhete %in% sorteio)
  semanas <- semanas + 1
  
  
}

semanas/52


