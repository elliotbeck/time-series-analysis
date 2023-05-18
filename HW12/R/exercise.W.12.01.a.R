#
# R code for problem 1 (a) of homework 12
#

# load libraries
library(sandwich) # function: vcovHAC
library(lmtest) # function: coeftest

# load 'class.RData'
load("data/class.RData")


# time series to analyse
infl <- us.data[, "infl"]
unemp <- us.data[, "unemp"]
n <- length(infl)

layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
    x = infl, type = "b", xlab = "time", ylab = "per cent per year",
    col = "green4"
)
title(main = "U.S. inflation rate", font.main = 1, cex.main = 1)
plot(
    x = diff(infl, 1), type = "b", xlab = "time", ylab = "per cent per year",
    col = "green4"
)
title(main = "change in U.S. inflation rate", font.main = 1, cex.main = 1)
plot(x = unemp, type = "b", xlab = "time", ylab = "per cent", col = "green4")
title(main = "U.S. unemployment rate", font.main = 1, cex.main = 1)
plot(
    x = diff(unemp, 1), type = "b", xlab = "time", ylab = "per cent",
    col = "green4"
)
title(main = "change in U.S. unemployment rate", font.main = 1, cex.main = 1)
mtext(
    text = paste("Exercise 12.1(a): U.S. time series"), side = 3, line = 0,
    outer = TRUE, font = 2, cex = 1
)

# fit linear model
model.fit <- lm(formula = diff(unemp[2:n], 1) ~ 1 + diff(infl[1:(n - 1)], 1))


# confidence intervals: standard vs. HAC
conf.int.std <- coef(model.fit)[2] + qnorm(c(0.025, 0.975)) *
    sqrt(vcov(model.fit)[2, 2])
names(conf.int.std) <- c("2.5%", "97.5%")
conf.int.HAC <- coef(model.fit)[2] + qnorm(c(0.025, 0.975)) *
    sqrt(vcovHAC(model.fit)[2, 2])
names(conf.int.HAC) <- c("2.5%", "97.5%")


print(summary(model.fit))
print(coeftest(model.fit, vcov = vcovHAC(model.fit)))

# 95% confidence intervals for slope coefficient
# based on standard OLS output
print(round(x = conf.int.std, digits = 4))
# based on HAC estimator for covariance matrix of parameter estimates
print(round(x = conf.int.HAC, digits = 4))