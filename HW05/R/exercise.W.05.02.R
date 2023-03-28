#
# R code for exercise 2 of homework 5
#

library("astsa")

T <- 1000
max.lag <- 20

x.sim <- arima.sim(list(ar = 0.9, ma = -0.9), n = T, innov = rnorm(T, 0, 1))

plot(x.sim, type = "b", col = "blue", main = "Simulated series")

acf2(x.sim, max.lag)

fit <- arima(x.sim, order = c(1, 0, 1))

fit