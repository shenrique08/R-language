sequencia_fibbonaci <- function(n) {
  
  if (n > 0) {
    fibonnaci <- function(n)
    if (n == 1 || n == 2) {
      return(1)
    } else {
      return(fibonnaci(n - 1) + fibonnaci(n - 2))
    }
    
    sequenci_numerica <- c()
    for (i in 1:n) {
      sequenci_numerica[i] <- fibonnaci(i)
    }
    
    return(sequenci_numerica)
  }
}


