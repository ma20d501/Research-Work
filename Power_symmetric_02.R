#library(spatial)
#install.packages("extraDistr")
#library(extraDistr)
# BHI, CM, MGG, MOI, NAI, SGN, WCX
MC <- 10000 # Number of repetitions
power5 <-rep()
sam <- c(20, 40, 60, 80, 100, 200)
for (d in 1:6) {
  power <-0
  n <- sam[d]
  alpha <- 0.05
  p_values <- numeric(MC)  # Initialize p-values
  for (j in 1:MC) {
    x<-rsl(n, xi=1, omega=1, alpha=0,dp=NULL)
    #x <- rnorm(n, 0)
    #x <- rcauchy(n, 1)
    #x <- rlaplace(n, 1, 1)
    #x <- rlogis(n, 1, 1)
    xs=sort(x)
    tt <- symmetry_test(xs, "BHI", bootstrap = FALSE, k = 3, mu = 0)
    p_values[j] <- tt$p.value
    if(p_values[j]<alpha){power=power+1}
  }
  power5[d]=power/MC
}
power5

