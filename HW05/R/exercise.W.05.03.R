#
# R code for exercise 3 of homework 5
#

library("astsa")

T <- 120000
max.lag <- 49

x.sim <- sarima.sim(d = 0, ma = 0.5, D = 0, sar = 0.8, S = 12, n = T)

plot(x.sim, type = "b", col = "blue", main = "Simulated series")

acf2(x.sim, max.lag)