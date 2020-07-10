manhattan.distance <- function(p, q){
  return (sum(abs(p - q)))
}

vectors.are.equal <- function(a, b){
  
  lenght_of_a <- length(a)
  lenght_of_b <- length(b)
  if (lenght_of_a != lenght_of_b) {return (FALSE)}
  
  for (i in 1:lenght_of_a){
    if(a[i] != b[i]) {return (FALSE)}
  }
  
  return (TRUE)
}