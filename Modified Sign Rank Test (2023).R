# 2023 Modified Signed Rank Wilcox test for symmetry with unknown center #
library(spatial)
MC <- 1000 # Number of repetitions
power <- numeric()
sam <- c(20,40,60,80,100,200)
for (d in 1:6) {
  n <- sam[d]
  alpha <- 0.05
  p_values <- numeric(MC)  # Initialize p-values
  for (j in 1:MC) {
    #x <- rnorm(n, 0)
    #x <- rlaplace(n, 1, 1)
    x <- rlogis(n, 0, 1)
    #x<-rnorm(n,0,1)
    m <- mean(x)
    n <- length(x)
    # The test statistic
    S <- sum(1 * (x < m))
    # Estimation of w
    a <- 1 * (x > m - n ^ (-1 / 5)) * (x < m + n ^ (-1 / 5))
    D <- sum(a)
    A <- max(c(1, D))
    VHW <- (n ^ (3 / 10) / A) ^ 2
    hat_w <- sqrt(1 / (4 * n * VHW))
    CE <- mean((x - m) * (x < m))
    V <- 1 / 4 + var(x) * hat_w ^ 2 + 2 * hat_w * CE
    # The resulting p-value
    p_values[j] <- 2 * (1 - pnorm(abs(S - n / 2) / sqrt(n * V)))
  }
  power[d] <- mean(p_values < alpha)
}
power

