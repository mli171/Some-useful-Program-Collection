
K <- 5
xi0 <- c(0.1, 0.2, 0.3, 0.4)
xi <- matrix(0, K-1, K-1)
diag(xi) <- xi0
for(pr in 1:(K-1)){
  ele_clc <- xi0[pr]^(1:(K-1))
  xi[pr,pr:(K-1)] <- ele_clc[1:(K-pr)]
}
xi[lower.tri(xi)] <- t(xi)[lower.tri(xi)]
