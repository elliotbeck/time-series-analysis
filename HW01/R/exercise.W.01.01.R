#
# R code for exercise 1 of homework 1
#

# clear memory and close all graphics
rm(list = ls())
# graphics.off()

# user input
RData.file.name <- "data/class.RData"

# load packages
library(sandwich) # function used: vcovHAC
library(lmtest) # function used: coeftest

# load 'class.RData'
load(file = RData.file.name)

# define variables for regression analysis
gnp.v <- as.numeric(us.gnp) # values of us.gnp
gnp.t <- seq(from = 1947.25, to = 1967, by = 0.25) - 1947.25
gnp.t.4cast <- as.data.frame(x = seq(
     from = 1967.25,
     to = 1971, by = 0.25
) - 1947.25)
names(gnp.t.4cast) <- "gnp.t"
gnp.t.all <- seq(from = 1947.25, to = 1971, by = 0.25) - 1947.25

# time plot of U.S. GNP
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
     x = us.gnp, type = "b", xlab = "Time",
     ylab = "U.S. GNP", main = "Time plot of U.S. GNP",
     col = "green4"
)

#### 1a ####

# fit global quadratic trend:
# x(t) = beta0 + beta1*(t-t0) + beta2*(t-t0)^2 + e(t)
gqt.lm <- lm(formula = gnp.v ~ 1 + gnp.t + I(gnp.t^2))

# print output from OLS
gqt.summary <- summary(gqt.lm, correlation = T)
print(gqt.summary)

# fit global cubic trend:
# x(t) = beta0 + beta1*(t-t0) + beta2*(t-t0)^2 + beta3*(t-t0)^3 + e(t)
gct.lm <- lm(formula = gnp.v ~ 1 + gnp.t + I(gnp.t^2) + I(gnp.t^3))

# print output from OLS
gct.summary <- summary(gct.lm, correlation = T)
print(gct.summary)

# f test for cubic coefficient
anova(gqt.lm, gct.lm, test = "F")

#### 1b ####

# fit global multiplicative exponential trend:
# x(t) = beta0*exp(beta1*(t-t0))*e(t), e(t) > 0
# <=> logt(x(t)) = log(beta0) + beta1(t-t0) + log(e(t))
gmet.lm <- lm(formula = log(gnp.v) ~ gnp.t)

# print output from OLS
gmet.summary <- summary(gmet.lm, correlation = T)
print(gmet.summary)

# fit global additive exponential trend:
# x(t) = beta0 + beta1*exp(beta2*(t-t0)) + e(t)
gaet.nls <- nls(
     formula = gnp.v ~ beta0 + beta1 * exp(beta2 * gnp.t),
     start = list(beta0 = mean(gnp.v), beta1 = 0.1, beta2 = 0.1)
)

# print output from NLS
gaet.summary <- summary(gaet.nls, correlation = T)
print(gaet.summary)

# time plot of U.S. GNP and various fitted trends
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))

# global quadratic trend
plot(
     x = 1947.25 + gnp.t, y = gnp.v, type = "b", ylim = c(200, 800),
     xlab = "time", ylab = "U.S. GNP", main = "Global quadratic trend",
     col = "green4"
)
lines(1947.25 + gnp.t, predict(object = gqt.lm), col = "blue")
legend(
     x = "topleft", legend = c("actual values", "estimated global trend"),
     col = c("green4", "blue", "red"), lty = c(1, 1), pch = c(1, -1)
)

# global cubic trend
plot(
     x = 1947.25 + gnp.t, y = gnp.v, type = "b", ylim = c(200, 800),
     xlab = "time", ylab = "U.S. GNP", main = "Global cubic trend",
     col = "green4"
)
lines(1947.25 + gnp.t, predict(object = gct.lm), col = "blue")

# global multiplicative exponential trend
plot(
     x = 1947.25 + gnp.t, y = gnp.v, type = "b", ylim = c(200, 800),
     xlab = "time", ylab = "U.S. GNP",
     main = "Global multiplicative exponential trend",
     col = "green4"
)
lines(1947.25 + gnp.t, exp(predict(object = gmet.lm)), col = "blue")

# global additive exponential trend
plot(
     x = 1947.25 + gnp.t, y = gnp.v, type = "b", ylim = c(200, 800),
     xlab = "time", ylab = "U.S. GNP",
     main = "Global additive exponential trend",
     col = "green4"
)
lines(1947.25 + gnp.t, predict(object = gaet.nls), col = "blue")

mtext(
     text = "Time plots of U.S. GNP and fitted trends",
     side = 3, line = 0, outer = T
)

#### 1c ####

# time plot of residual series
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))

# global quadratic trend
plot(
     x = 1947.25 + gnp.t, y = residuals(gqt.lm), type = "b",
     xlab = "time", ylab = "U.S. GNP", main = "Global quadratic trend",
     col = "blue"
)
lines(
     x = 1947.25 + c(gnp.t[1], gnp.t[length(gnp.t)]),
     y = c(0, 0), col = "black"
)

# global cubic trend
plot(
     x = 1947.25 + gnp.t, y = residuals(gct.lm), type = "b",
     xlab = "time", ylab = "U.S. GNP", main = "Global cubic trend",
     col = "blue"
)
lines(
     x = 1947.25 + c(gnp.t[1], gnp.t[length(gnp.t)]),
     y = c(0, 0), col = "black"
)

# global multiplicative exponential trend
plot(
     x = 1947.25 + gnp.t, y = residuals(gmet.lm), type = "b",
     xlab = "time", ylab = "U.S. GNP",
     main = "Global multiplicative exponential trend", col = "blue"
)
lines(
     x = 1947.25 + c(gnp.t[1], gnp.t[length(gnp.t)]), y = c(0, 0),
     col = "black"
)

# global additive exponential trend
plot(
     x = 1947.25 + gnp.t, y = as.numeric(residuals(gaet.nls)),
     type = "b", xlab = "time", ylab = "U.S. GNP",
     main = "Global additive exponential trend", col = "blue"
)
lines(
     x = 1947.25 + c(gnp.t[1], gnp.t[length(gnp.t)]), y = c(0, 0),
     col = "black"
)

mtext(text = "Time plots of residual series", side = 3, line = 0, outer = T)

# correlogram of residual series
open.graphics.window(width = 11.7, height = 8.3, dpi = 100)
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))

# define axis labels
acf.x.lab <- expression(paste("lag ", tau, sep = ""))
acf.y.lab <- expression(paste("acf(", tau, ")", sep = ""))

# global quadratic trend
acf(
     x = as.ts(residuals(gqt.lm)), xlab = acf.x.lab, ylab = acf.y.lab,
     main = "Global quadratic trend"
)

# global cubic trend
acf(
     x = as.ts(residuals(gct.lm)), xlab = acf.x.lab, ylab = acf.y.lab,
     main = "Global cubic trend"
)

# global multiplicative exponential trend
acf(
     x = as.ts(residuals(gmet.lm)), xlab = acf.x.lab, ylab = acf.y.lab,
     main = "Global multiplicative exponential trend"
)

# global additive exponential trend
acf(
     x = as.ts(as.numeric(residuals(gaet.nls))), xlab = acf.x.lab,
     ylab = acf.y.lab, main = "Global additive exponential trend"
)

mtext(text = "Correlogram of residual series", side = 3, line = 0, outer = T)

#### 1d ####

# time plot of U.S. GNP and forecasts
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))

# global quadratic trend
gnp.gqt.v.f <- predict(object = gqt.lm, newdata = gnp.t.4cast)
plot(
     x = 1947.25 + gnp.t.all, y = c(gnp.v, gnp.gqt.v.f),
     type = "n", ylim = c(200, 1000), xlab = "time", ylab = "U.S. GNP",
     main = "Global quadratic trend"
)
lines(x = 1947.25 + gnp.t, y = gnp.v, type = "b", col = "green4")
lines(x = 1947.25 + gnp.t.4cast[, ], y = gnp.gqt.v.f, type = "b", col = "red")
lines(
     x = 1947.25 + gnp.t, y = predict(object = gqt.lm), type = "l",
     col = "blue"
)
legend(
     x = "topleft", legend = c(
          "actual values", "estimated global trend",
          "forecasts"
     ), col = c("green4", "blue", "red"), lty = c(0, 1, 0),
     pch = c(1, -1, 1)
)

# global cubic trend
gnp.gct.v.f <- predict(object = gct.lm, newdata = gnp.t.4cast)
plot(
     x = 1947.25 + gnp.t.all, y = c(gnp.v, gnp.gct.v.f),
     type = "n", ylim = c(200, 1000), xlab = "time",
     ylab = "U.S. GNP", main = "Global cubic trend"
)
lines(x = 1947.25 + gnp.t, y = gnp.v, type = "b", col = "green4")
lines(
     x = 1947.25 + gnp.t.4cast[, ], y = gnp.gct.v.f, type = "b",
     col = "red"
)
lines(
     x = 1947.25 + gnp.t, y = predict(object = gct.lm),
     type = "l", col = "blue"
)

# global multiplicative exponential trend
gnp.gmet.v.f <- predict(object = gmet.lm, newdata = gnp.t.4cast)
plot(
     x = 1947.25 + gnp.t.all, y = c(
          gnp.v,
          exp(gnp.gmet.v.f)
     ), type = "n", ylim = c(200, 1000),
     xlab = "time", ylab = "U.S. GNP",
     main = "Global multiplicative exponential trend"
)
lines(x = 1947.25 + gnp.t, y = gnp.v, type = "b", col = "green4")
lines(
     x = 1947.25 + gnp.t.4cast[, ], y = exp(gnp.gmet.v.f),
     type = "b", col = "red"
)
lines(
     x = 1947.25 + gnp.t, y = exp(predict(object = gmet.lm)),
     type = "l", col = "blue"
)

# global additive exponential trend
gnp.gaet.v.f <- predict(object = gaet.nls, newdata = gnp.t.4cast)
plot(
     x = 1947.25 + gnp.t.all, y = c(gnp.v, gnp.gaet.v.f),
     type = "n", ylim = c(200, 1000), xlab = "time", ylab = "U.S. GNP",
     main = "Global additive exponential trend"
)
lines(x = 1947.25 + gnp.t, y = gnp.v, type = "b", col = "green4")
lines(x = 1947.25 + gnp.t.4cast[, ], y = gnp.gaet.v.f, type = "b", col = "red")
lines(
     x = 1947.25 + gnp.t, y = predict(object = gaet.nls),
     type = "l", col = "blue"
)

mtext(
     text = "Time plots of U.S. GNP and forecasts",
     side = 3, line = 0, outer = T
)