#
# R code for problem 6 of homework 13
#

# clear memory and close all graphics
rm(list = ls())

# user input
acf.lag.max <- 4 * 4 # specifiation for ac.f.

# load libraries
library(sandwich) # function: vcovHAC
library(lmtest) # function: coeftest

# load 'class.RDdata'
load("class.RData")


# select time series to analyse
US.FR <- us.data.hill[, 3]
US.BR <- us.data.hill[, 4]

# define lables for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))

# plot time series
pdf("exercise.W.13.06.a - U.S. Interest Rates - time plots.pdf")

par(mfrow = c(2, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = US.FR, type = "b", xlab = "time", ylab = "per cent per year",
    col = "green4"
)
title(main = "U.S. Federal Funds Rate", font.main = 1, cex.main = 1)
plot(
    x = US.BR, type = "b", xlab = "time", ylab = "per cent per year",
    col = "green4"
)
title(main = "U.S. 3-Year Bond Rate", font.main = 1, cex.main = 1)
mtext(
    text = paste("Exercise 13.6.a: U.S. Interest Rates"),
    side = 3, line = 0, outer = T, font = 2, cex = 1
)

# estimate linear regression model
lm.fit <- lm(US.BR ~ US.FR)

# part (a)

# adf test
print(summary(lm.fit))
# ADF test based on residuals:
res.adf.test.aic <- adf.test.aic(
    x = lm.fit$residuals, p.max = 5,
    time.trend = F, acf.plot = F
)

# part (b)
d.US.FR <- diff(US.FR)
d.US.BR <- diff(US.BR)
n <- length(d.US.BR)
ec1 <- US.BR - US.FR
ec2 <- US.BR - lm.fit$coefficients[2] * US.FR

fit1 <- lm(d.US.BR[2:n] ~ d.US.BR[1:(n - 1)] + d.US.FR[1:(n - 1)] + ec1[2:n])
# fitted model (using theta = 1):\n\n")
coeftest(fit1, vcov = vcovHAC(fit1))

fit2 <- lm(d.US.BR[2:n] ~ d.US.BR[1:(n - 1)] + d.US.FR[1:(n - 1)] + ec2[2:n])
# fitted model (using estimated theta):
coeftest(fit2, vcov = vcovHAC(fit2))
