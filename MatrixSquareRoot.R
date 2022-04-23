#--------- Find square root of matrix A
# reference from https://stackoverflow.com/questions/28227111/square-root-of-a-singular-matrix-in-r

matrix.sqrt = function(A){
  
  # eigen decomposition
  tmp = eigen(A)
  T = tmp$vectors
  J = diag(tmp$values)
  Tinv = solve(T)
  
  Jsqrt = diag(sqrt(tmp$values))
  Asqrt = T %*% Jsqrt %*% Tinv
  
  return(Asqrt)
}

A = matrix( c( 2, -1, 0, -1, 2, -1, 0, -1, 2 ), nrow=3, byrow=TRUE )
A
Asqrt = matrix.sqrt(A)
# check
Asqrt %*% Asqrt
