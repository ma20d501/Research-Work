##########################################
rm(list=ls())
#library((emplik))
MC <- 1000  # Number of simulations
alpha<- 0.05
power1<-rep()
power2<-rep()
sam=c(20,40,60,80,100,120)
for(d in 1:6){
  n<-sam[d]
 x <- runif(n,-1,1)
 xs=sort(x)
############# RS test statistic  ################
  RS <- function(x, median. = median(x), equal = FALSE, ties.method = "first") {
  RGMD2 <- function(y, n) {
    sum((2 * (1:n) - n - 1) * y) / n / (n - 1)
  }
  xs <- xs - median.
  rx <- rank(abs(xs), ties.method = ties.method)
  xpos <- if (equal) xs >= 0 else xs > 0
  n1 <- sum(xpos)
  xneg <- xs <= 0
  n2 <- sum(xneg)
  rpos <- sort(rx[xpos])
  rneg <- sort(rx[xneg])
  RGMD2(rneg, n2) / RGMD2(rpos, n1)
}
# Function to estimate critical value under the null hypothesis
estimate_critical_value <- function(n, MC, alpha) {
  null_test_statistics <- numeric(MC)
  for (i in 1:MC) {
    x <- runif(n) 
    xs=sort(x)
    test_statistic <- RS(xs)
    if (!is.nan(test_statistic)) {  # Check for NaNs
      null_test_statistics[i] <- test_statistic
    }
  }
  null_test_statistics <- null_test_statistics[!is.nan(null_test_statistics)]  # Remove NaNs
  if (length(null_test_statistics) == 0) {
    stop("Error")
  }
  # Determine critical value based on the distribution of test statistics
  critical_value <- quantile(null_test_statistics, 1 - alpha)
  return(critical_value)
}

# Function to conduct power calculation under the null hypothesis
power_RS <- function(n, alpha, MC) {
  critical_value <- estimate_critical_value(n, MC, alpha)
  rejected_null <- numeric(MC)
  for (i in 1:MC) {
    # Generate data under the null hypothesis (symmetric distribution)
    x <- runif(n)
    xs=sort(x)
    # Calculate test statistic
    test_statistic <- RS(xs)
    # Perform test
    if (abs(test_statistic) > critical_value) {
      rejected_null[i] <- 1
    }
  }
  power <- mean(rejected_null)
  return(power)
}
# Calculate power under the null hypothesis
power1[d] <- power_RS(n, 0.05, MC)
power2[d] <- power_RS(n, 0.01, MC)
}
power1
power2



X<-rnorm(100)
symmetry::B1(X)
X<-rgamma(n,1,1)
symmetry::B1(X)
