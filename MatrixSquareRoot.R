# randomly generate a positive definite matrix via convariance matrix
A <- matrix(runif(3^2)*2-1, ncol=n) 
A <- t(A) %*% A
A


# find square root of matrix A
# reference from https://stackoverflow.com/questions/28227111/square-root-of-a-singular-matrix-in-r
tmp = eigen(A)
T = tmp$vectors

J = diag(tmp$values)
Tinv = solve(T)
Jsqrt = diag(sqrt(tmp$values))
Asqrt = T %*% Jsqrt %*% Tinv

# check
Asqrt %*% Asqrt