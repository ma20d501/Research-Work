# x is a numeric vector of data values
m <- mean(x)
n <- length(x)
W <- wilcox.test(x, mu = m)$statistic
xc <- x - m
# T_n selection
r <- quantile(x, c(0.25, 0.75))
h <- (r[2] - r[1]) / 1.34
Tn <- log(n) / (3 * 1.06 * min(sqrt(var(x)), h))
# Estimation of theta
S <- function(u)
  (sum(sin(2*pi*(xc[xc!=u]-u)*Tn) / (2*pi*(xc[xc!= u]-u)))
   + sum(sin(2*pi*(xc+u)*Tn) / (2*pi*(xc+u))))
SV <- Vectorize(S)
hat_theta <- 2 * sum(SV(xc)) / n^2 + 2*Tn/n
# Estimation of tau
xs <- sort(xc)
S1 <- seq(from = 1, to = n, by = 1)
hat_tau <- sum(xs * S1) / n^2
E <- n * (n + 1) / 4
sigma2 <- var(x)
V <- (n*(n+1)*(2*n+1)/24 - n*(n-1)*(n-3)*hat_theta*hat_tau
      +(n-1)*(n-2)*(n-3)*(n-4)*sigma2/(4*n)*hat_theta^2)
# The resulting p-value
pval <- 2 * (1 - pnorm(abs(E - W) / sqrt(V)))