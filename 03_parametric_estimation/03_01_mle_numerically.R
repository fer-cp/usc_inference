rm(list = ls())
# install.packages("MASS")
set.seed(123456)
  # Only change the RNG seed if you know what you're doing

p <- 4
s <- 2
n <- 10
(x <- round(rgamma(n, shape = p, scale = s), 2))
  # Do not touch this definition, parameters
x
# ?GammaDist


#### Method of moments ####
xbar <- mean(x)
s2 <- (n - 1) * var(x) / n

(a_M <- xbar / s2)
(p_M <- xbar * a_M)

#### Maximum likelihood estimation ####
nlm <- n * log(mean(x))
slx <- sum(log(x))

## Likelihood score for p:
ec <- function(p) n * log(p) - nlm - n * digamma(p) + slx
# ?digamma

## Bisection method:
(upp <- p_M)
ec(upp)
(low <- p_M / 2)
ec(low)
niter <- 20
for (i in 1:niter) {
  mid <- (low + upp) / 2
  cat("iter:", i, " p:", mid, " ec:", ec(mid), "\n")
  if (ec(mid) > 0) {
    low <- mid
  } else {
    upp <- mid
  }
}
(p_MLB <- mid)
(a_MLB <- p_MLB / xbar)


## Newton–Raphson method
sx <- sum(x)
a <- a_M
p <- p_M
niter <- 5
for (i in 1:niter) {
  # Computation of the gradient:
  Gr <- c(n * p / a - sx, n * log(a) - n * digamma(p) + slx)
  #
  # Hessian matrix:
  H11 <- -n * p / a^2
  H12 <- n / a
  H22 <- -n * trigamma(p)
  (H <- cbind(c(H11, H12), c(H12, H22)))

  theta <- c(a, p) - solve(H) %*% Gr
  a <- theta[1]
  p <- theta[2]
  cat("iter:", i, " a:", a, " p:", p, " Gr:", Gr, "\n")
}
(a_MLNR <- a)
(p_MLNR <- p)

## Covariance matrix of the estimators:
I <- -H # Information matrix
(Sigma <- solve(I)) # Cov matrix of the estimators
sqrt(diag(Sigma)) # Typical errors


# Using the fitter to known distributions:
(adjg <- MASS::fitdistr(x, "gamma"))
# Prints estimators with typical errors
# names(adjg) # *$estimate, *$sd
tS <- Sigma
tS[1, 1] <- Sigma[2, 2]
tS[2, 2] <- Sigma[1, 1]
#
c(p_MLB, a_MLB)
c(p_MLNR, a_MLNR)
sqrt(diag(tS))
#
vcov(adjg) # Cov matrix estimators
tS
