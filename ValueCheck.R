
betaClc = seq(from=-1, to=2, length.out=10000)
res = rep(0, length(betaClc))
for(iii in 1:length(betaClc)){
  res[iii] = score.ipcw(t=t.2.star-t.1.star, x=x.m.new, beta=betaClc[iii], w=wt.new)
}
plot(x=betaClc, y=res, type = "p", pch=".")
abline(h=0, lty="dotted", col="red")
abline(v=1, lty="dotted", col="red")
abline(v=beta.hat, lty="dotted", col="blue")
abline(a=0, b=1)