rm(list = ls())

#### TEST 1 : X_(1) ####
(c1 <- sqrt(12) * qnorm(1 - .05^(1 / 3)) + 120) # 121.2
set.seed(1)
R <- 1E5
x1 <- replicate(R, min(rnorm(3, 0, sqrt(12))))
beta_phi1 <- function(mi) mean(x1 + mi >= c1)
beta_phi1(120) # 0.05
beta_phi1(115) # 7e-5
beta_phi1(118) # 0.006
beta_phi1(119) # 0.02
beta_phi1(121) # 0.11

gridr <- seq(110, 135, by = .1)
y1 <- sapply(gridr, beta_phi1)
plot(gridr, y1, xlim = c(110, 135), type = "l", xlab = "", ylab = "")


#### TEST 2 : X_(2) ####
p <- function(x) x^2 * (3 - 2 * x)
plot(p, xlim = c(-.5, 1.5))
abline(v = 0:1)

alpha <- .05
## Newton's method
y <- .5 # We want a solution in ]0,1[
niter <- 0
while (abs(p(y) - (1 - alpha)) > 1E-4) {
  y <- y - (p(y) - (1 - alpha)) / (6 * y * (1 - y))
  niter <- niter + 1
  if (niter > 100) break
}
y # 0.86
3 * y^2 - 2 * y^3 # 0.95
(c2 <- sqrt(12) * qnorm(y) + 120) # 123.8
set.seed(1)
x2 <- replicate(R, median(rnorm(3, 0, sqrt(12))))
beta_phi2 <- function(mi) mean(x2 + mi >= c2)
beta_phi2(120) # 0.05
beta_phi2(115) # 4e-5
beta_phi2(118) # 0.007
beta_phi2(119) # 0.02
beta_phi2(121) # 0.11
y2 <- sapply(gridr, beta_phi2)
plot(gridr, y1, xlim = c(110, 135), type = "l", xlab = "", ylab = "")
lines(gridr, y2, type = "l", col = "red")


#### TEST 3: X_(3) ####
(c3 <- sqrt(12) * qnorm(.95^(1 / 3)) + 120) # 127.3
set.seed(1)
x3 <- replicate(R, max(rnorm(3, 0, sqrt(12))))
beta_phi3 <- function(mi) mean(x3 + mi >= c3)
beta_phi3(120) # 0.05
beta_phi3(115) # 0.0005
beta_phi3(118) # 0.01
beta_phi3(119) # 0.02
beta_phi3(121) # 0.10
y3 <- sapply(gridr, beta_phi3)
plot(gridr, y1, xlim = c(110, 135), type = "l", xlab = "", ylab = "")
lines(gridr, y2, type = "l", col = "red")
lines(gridr, y3, type = "l", col = "blue")


#### TEST 4: X_bar ####
(c4 <- 2 * qnorm(.95) + 120) # 123.3
set.seed(1)
x4 <- replicate(R, mean(rnorm(3, 0, sqrt(12))))
beta_phi4 <- function(mi) mean(x4 + mi >= c4)
beta_phi4(120) # 0.05
beta_phi4(115) # 3e-5
beta_phi4(118) # 0.04
beta_phi4(119) # 0.02
beta_phi4(121) # 0.12
y4 <- sapply(gridr, beta_phi4)
plot(gridr, y1, xlim = c(110, 135), type = "l", xlab = "", ylab = "")
lines(gridr, y2, type = "l", col = "red")
lines(gridr, y3, type = "l", col = "blue")
lines(gridr, y4, type = "l", col = "green")
abline(v = 120)
abline(h = beta_phi1(120))


plot(gridr, y1, xlim = c(120, 130), type = "l", xlab = "", ylab = "")
lines(gridr, y2, type = "l", col = "red")
lines(gridr, y3, type = "l", col = "blue")
lines(gridr, y4, type = "l", col = "green")
abline(v = 125)
abline(h = beta_phi1(125))
abline(h = beta_phi2(125), col = "red")
abline(h = beta_phi3(125), col = "blue")
abline(h = beta_phi4(125), col = "green")
beta_phi1(125)
beta_phi2(125)
beta_phi3(125)
beta_phi4(125)