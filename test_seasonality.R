library(TSA)
#-------------------------- Frequency analysis by Spectral Envelope
# <<Time Series Analysis and its Applications with R Examples>>
myfreq <- function(x){
  
  K <- dim(x)[2]
  Var = var(x) # var-cov matrix
  xspec = mvspec(x, spans=c(7,7), plot=FALSE) 
  fxxr = Re(xspec$fxx) # fxxr is real(fxx)
  # compute Q = Var^-1/2
  ev = eigen(Var)
  Q = ev$vectors%*%diag(1/sqrt(ev$values))%*%t(ev$vectors)
  # compute spec envelope and scale vectors
  num = xspec$n.used # sample size used for FFT
  nfreq = length(xspec$freq) # number of freqs used
  specenv = matrix(0,nfreq,1) # initialize the spec envelope 
  beta = matrix(0,nfreq,K) # initialize the scale vectors 
  for (k in 1:nfreq){
    ev = eigen(2*Q%*%fxxr[,,k]%*%Q/num, symmetric=TRUE)
    specenv[k] = ev$values[1] # spec env at freq k/n is max evalue 
    b = Q%*%ev$vectors[,1] # beta at freq k/n
    beta[k,] = b/sqrt(sum(b^2)) 
  } # helps to normalize beta
  
  # output and graphics
  frequency = xspec$freq
  plot(frequency, 100*specenv, ylim=c(0,1), 
       type="l", ylab="Spectral Envelope (%)") # add significance threshold to plot
  m = xspec$kernel$m
  etainv = sqrt(sum(xspec$kernel[-m:m]^2)) 
  thresh=100*(2/num)*exp(qnorm(.9999)*etainv)
  abline(h=thresh, lty=6, col=4)
  
  # output Corresponding period by rank
  data <- cbind(1/frequency, specenv)
  data <- data[order(data[,2], decreasing = T),]
  
  return(data)
}


dat <- read.table(file = "~/Desktop/MTMFinal/CanadaCloudData/hourly_day.txt")
dat$Date <- ISOdate(year = dat$V1, month = dat$V2, day = dat$V3, hour = dat$V4)
dat1 <- dat[dat$V1>=1965 & dat$V1 <=1994,] # 30 years

m <- as.numeric(as.character(dat1$V5))
dat1$V5 <- m
mm <- m
# mm[m==1]  <- 1
# mm[m==2]  <- 1
# mm[m==3]  <- 1
# mm[m==4]  <- 2
# mm[m==5]  <- 2
# mm[m==6]  <- 2
# mm[m==7]  <- 3
# mm[m==8]  <- 3
# mm[m==9]  <- 3
# mm[m==10] <- 4
dat1$V6 <- mm
# Form Data Set
Ti     <- length(mm)
K      <- max(mm) + 1

y_log <- matrix(0, nrow=Ti, ncol=K)
for (tt in 1:Ti) {y_log[tt,mm[tt]+1] <- 1}

# 1. Frequency analysis by periodogram with K = 11
haha <- periodogram(y=m, plot = F)
p <- order(haha$spec, decreasing = T)
# related frequency
(1/haha$freq[p[1:5]])/8
# [1] 11250.00000   362.90323   148.02632    30.90659    69.87578

# 2. Frequency analysis by periodogram with K = 5
haha <- periodogram(y=mm, plot = F)
p <- order(haha$spec, decreasing = T)
# related frequency
(1/haha$freq[p[1:5]])/8
# [1] 11250.00000   362.90323    69.87578   148.02632    30.90659

# 3. Discretized by spectral envelope
tmp <- myfreq(x=y_log)
tmp[1:5, ]
# [1,] 2903.226 0.003610829  # 365*8=2920 (a year)
# [2,] 3000.000 0.003356776
# [3,] 2812.500 0.003295721
# [4,] 3103.448 0.002787562
# [5,] 2727.273 0.002699944


# But plot shows possible 0.125 as another frequency boost