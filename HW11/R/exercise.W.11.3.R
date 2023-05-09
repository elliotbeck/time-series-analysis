#
# R code for problem 3 of homework 11
#

# clear memory and close all graphics
rm(list = ls())
graphics.off()

x.bar <- -0.1049
T <- 100
q <- qt(0.975, T - 1)


# model-based standard error, using estimated coefficients
ma.coef <- c(1, 0.4190, 0.1635)
ma.coef.extend <- c(ma.coef, 0, 0)
s2.z <- 0.7852

gamma <- rep(0, 3)
gamma[1] <- sum(ma.coef^2)
gamma[2] <- sum(ma.coef.extend[1:4] * ma.coef.extend[2:5])
gamma[3] <- sum(ma.coef.extend[1:3] * ma.coef.extend[3:5])
gamma <- s2.z * gamma

s2.inf.1 <- gamma[1] + 2 * (gamma[2] + gamma[3])
se.1 <- sqrt(s2.inf.1 / T)
me <- q * se.1
ci.1 <- c(x.bar - me, x.bar + me)


# model-based standard error, using sample acv.f.
gamma.hat <- c(0.97616, 0.44353, 0.23425)
s2.inf.2 <- gamma.hat[1] + 2 * (gamma.hat[2] + gamma.hat[3])
se.2 <- sqrt(s2.inf.2 / T)
me <- q * se.2
ci.2 <- c(x.bar - me, x.bar + me)


# model-free HAC standard error
gamma.hat <- c(
    0.9762, 0.4435, 0.2343, 0.1421,
    0.0680, 0.0730, 0.0579, 0.0404,
    0.0120, 0.0867, 0.0362, 0.0830,
    0.0112, 0.0085, -0.0001, -0.0469
)
S <- 8
w <- 1 - (1:S) / S
s2.inf.3 <- gamma.hat[1] + 2 * sum(w * gamma.hat[2:(S + 1)])
se.3 <- sqrt(s2.inf.3 / T)
me <- q * se.3
ci.3 <- c(x.bar - me, x.bar + me)