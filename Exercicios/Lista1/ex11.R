# (a) Simule um passeio de 8 passos com Link começando na origem. A saída desta simulação deve apresentar o ponto do plano em que Link está após os 8 passos.



passeio <- function(passos) {
  # Starting position at the origin
  posicao <- c(0, 0)
  
  for (i in 1:passos) {
    # Simulating a random step in x and y direction
    x <- sample(x = -1:1, size = 1, replace = TRUE)
    y <- sample(x = -1:1, size = 1, replace = TRUE)
    
    # Update the current position
    posicao <- posicao + c(x, y)
  }
  
  # Return the final position after the steps
  return(posicao)
}

# Simulate a walk of 8 steps
resultado <- passeio(8)
print(resultado)


