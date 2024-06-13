#############  Data set 01 (E. I. Abdul Sathar-2020) ############
mean <- 3076.88
sd <- 344.362

x <- c(2944, 3206, 2751, 3089, 3406, 3275, 2606, 2723, 2475, 2930, 2530, 2399, 2806, 2827, 2951, 2854, 2930, 2565, 2799, 3102, 3454, 4185, 3095, 3247, 3371, 3302, 3544, 3454, 3468, 3233, 2571, 3268, 2792, 2916, 3006, 3523, 2958, 3123, 2930, 2689, 3075, 3337, 3468, 3254, 3061, 3647, 3295, 2971, 3068, 3371)

################### Histogram  for tensile strength data#######
y <- seq(min(x), max(x), length.out = 100)

# Normal curve
fun <- dnorm(y, mean, sd)
hist(fun)
# Histogram
hist(x, prob = TRUE, col = "white",xlim=c(min(x), max(x)), ylim = c(min(fun), max(fun)), main = "Histogram with normal curve of tensile strength data")
lines(y, fun, lty="dotted", col="darkgreen", lwd = 2)

#######################################
n = length(x)
############Test statistics####################
SCR.Test<-function(x){ # here X is an array
  n = length(x)
  k1=(n+1)/(n*(n-2))
  k2=1/(n*(n-1)*(n-2))
  xs=sort(x)
  s1<-0
  s2<-0
  for(i in 1:n){
    s1<-s1+xs[i]
    s2<-s2+(2*i*((n+1)-i)+2)*xs[i]
  }
  t<- (4*k1*s1)-(6*k2*s2)  
}
t
########################################## Simulated Critical region #####
MC<- 1000
alpha <- 0.05
test_statistics <- numeric(MC)
deltas= rep()
for (i in 1:MC) {
  x <-rnorm(n, mean,sd)
  deltas[i]= SCR.Test(x)
}
deltas=sort(deltas)
c1= quantile(deltas, 0.05) # lower quantile
c2= quantile(deltas, 0.95) # upper quantile
c1
c2


######################################### Simulated critical method data set 2 () ##############
mu=3.61;lambda=1.588
x<- c(0.2, 0.3, 0.5, 0.5, 0.5, 0.5, 0.6, 0.6, 0.7, 0.7, 0.7, 0.8, 0.8, 1.0, 1.0, 1.0, 1.0, 1.1, 1.3, 1.5,1.5, 1.5, 1.5, 2.0, 2.0, 2.2, 2.5, 3.0, 3.0, 3.3, 3.3, 4.0, 4.0, 4.5, 4.7, 5.0, 5.4, 5.4, 7.0, 7.5,8.8, 9.0, 10.3, 22.0, 24.5)
n = length(x)
      ############Test statistics####################
SCR.Test<-function(x){ # here X is an array
  n = length(x)
  k1=(n+1)/(n*(n-2))
  k2=1/(n*(n-1)*(n-2))
  xs=sort(x)
  s1<-0
  s2<-0
  for(i in 1:n){
    s1<-s1+xs[i]
    s2<-s2+(2*i*((n+1)-i)+2)*xs[i]
  }
  t<- (4*k1*s1)-(6*k2*s2)  
}
t
######################## Simulated Critical region #####
n= 45
MC<- 1000
alpha <- 0.1
test_statistics <- numeric(MC)
deltas= rep()
for (i in 1:MC) {
  n=45
  mu=3.61;lambda=1.588
  IGD <- function(x, mu,lambda) {
    exp(-(lambda)*(x - mean)^2 / (2 * x * mu^2)) * (sqrt(lambda /2 * pi * x^3) )
  }
  generate_random_numbers <- function(n,mu,lambda) {
    u <- runif(n)  
    z <- qnorm(u, mean = 0, sd = 1) 
    x <- mu + lambda * z
    return(x)
  }
  x <- generate_random_numbers(n, mu,lambda)
  deltas[i]= SCR.Test(x)
}
deltas=sort(deltas)
c1= quantile(deltas, 0.05) # lower quantile
c2= quantile(deltas, 0.95) # upper quantile

c1
c2

#######################################################################
