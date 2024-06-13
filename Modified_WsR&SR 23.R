
## A: the modified signed rank test
#x  is an array of data points
mod.wilcox.test<- function(x) {
  n <- length(x)
  m <- mean(x)
  sigma2 <- var(x)
  xc <- x-m
  ## Test statistic
  W <- wilcox.test(x,mu=m)$statistic
  
  ## Tn selection
  r <- quantile(x, c(0.25, 0.75))
  h <- (r[2] - r[1])/1.34
  Tn<-log(n)/( 3 * 1.06 * min(sqrt(var(x)), h)) 
  
  ## Estimation of theta
  S<-function(u) sum( sin(2*pi*(xc[xc!=u]-u)*Tn)/(2*pi*(xc[xc!=u]-u)))+
    sum(sin(2*pi*(xc+u)*Tn)/(2*pi*(xc+u)))
  SV<-Vectorize(S)
  hat_theta <-2*sum(SV(xc))/n^2 + 2*Tn/n
  
  ## Estimation of tau 
  xs <- sort(xc)  # V_(i)
  S1 <- seq(from=1, to=n, by=1) # i
  hat_tau <- sum(xs*S1)/n^2
  
  ## The asymptotic mean and variance 
  E <- n*(n+1)/4
  V <- n*(n+1)*(2*n+1)/24 - n*(n-1)*(n-3) * hat_theta * hat_tau +
    (n-1)*(n-2)*(n-3)*(n-4)*sigma2/(4*n)*(hat_theta)^2
  
  ## The resulting p-value (see Remark 2 in this context)
  pval <- 2*(1-pnorm(abs(E-W)/sqrt(V)))
  return(list("p.value"=pval, "var"=V, "W"=W))
}

## B:  the modified sign test
mod.sign.test <- function(x) {
  m <- mean(x)
  n <- length(x)
  
  ## Test statistic
  S <- sum(1*(x<m))
  
  ## Estimation of w 
  a <- 1 * (x>m-n^(-1/5)) * (x < m+n^(-1/5))
  D <- sum(a)
  A <- max(c(1,D))
  VHW <- (n^(3/10)/A)^2
  hat_w <- sqrt(1/(4*n*VHW))
  
  ## the H0-probability weight moment of x
  CE <- mean((x-m)*(x<m))
  
  ## Asymptotic variance
  V <- 1/4 + var(x)*(hat_w)^2 + 2*hat_w*CE
  
  # The resulting p-value (see Remark 2 in this context)
  pval <- 2*(1-pnorm(abs(S-n/2)/sqrt(n*V))) 
  return(list("p.value"=pval, "var"=n*V, "S"=S))
}

############### mod wilcoxn sign rank test ######################
MC <- 1000 # Number of repetitions
power5 <-rep()
sam <- c(25, 50, 100, 200)
for (d in 1:4) {
  power <-0
  n <- sam[d]
  alpha <- 0.05
  p_values <- numeric(MC)  # Initialize p-values
  for (j in 1:MC) {
    #x <- rnorm(n, 1, 1)
    #x <- rlaplace(n, 1, 1)
    x <- rlogis(n, 1, 1)
    #x<-rmixnorm(n,0,1,0,1,0.5)
    #x<-rchisq(n,35)
    #x<-rexp(n,1)
    xs=sort(x)
    p_values[j]<- mod.wilcox.test(x)$p.value
    if(p_values[j]<alpha){power=power+1}
  }
  power5[d]=power/MC
}
power5

############# mod sign test ###############

MC <- 1000 # Number of repetitions
power5 <-rep()
sam <- c(25, 50, 100, 200)
for (d in 1:4) {
  power <-0
  n <- sam[d]
  alpha <- 0.05
  p_values <- numeric(MC)  # Initialize p-values
  for (j in 1:MC) {
    #x <- rnorm(n, 1, 1)
    #x <- rlaplace(n, 1, 1)
    x <- rlogis(n, 1, 1)
    #x<-rmixnorm(n,0,1,0,1,0.5)
    #x<-rchisq(n,35)
    #x<-rexp(n,1)
    xs=sort(x)
    p_values[j]<- mod.sign.test(x)$p.value
    if(p_values[j]<alpha){power=power+1}
  }
  power5[d]=power/MC
}
power5

########################## B1 #####################
MC <- 1000 # Number of repetitions
power5 <-rep()
sam <- c(25, 50, 100, 200)
for (d in 1:4) {
  power <-0
  n <- sam[d]
  alpha <- 0.05
  p_values <- numeric(MC)  # Initialize p-values
  for (j in 1:MC) {
    #x <- rnorm(n, 1, 1)
    #x <- rlaplace(n, 1, 1)
    #x <- rlogis(n, 0, 1)
    #x<-rmixnorm(n,0,1,0,1,0.5)
    #x<-rchisq(n,35)
    x<-rexp(n,1)
    xs=sort(x)
    p_values[j]<- symmetry_test(x, stat="B1")$p.value
    if(p_values[j]<alpha){power=power+1}
  }
  power5[d]=power/MC
}
power5


