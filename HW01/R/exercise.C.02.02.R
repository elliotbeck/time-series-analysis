#
# R code for exercise 2.2 in Chatfield & Xing
#

# clear memory and close all graphics
rm(list = ls())

# user in put
data.file.name <- "HW01/R/exercise.C.02.02.dat"

# import data
data.df <- read.table(file = data.file.name, header = F, sep = "")

# create time-series object
data.ts <- ts(data = data.df, start = 1, frequency = 1)

#### a/b ####

# time plot (without mean)
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = data.ts, type = "b", xlab = "Time t",
    ylab = "x(t)", main = "Time plot of stationary time series", col = "green4"
)

# time plot (with mean)
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = data.ts, type = "b", xlab = "Time t",
    ylab = "x(t)", main = "Time plot of stationary time series", col = "green4"
)
lines(
    x = c(1, length(data.ts)), y = rep(mean(data.ts), 2),
    col = "red", lty = 2
)
legend(
    x = "topright", legend = c("x(t)", "mean(x)"),
    col = c("green4", "red"), lty = c(1, 2)
)
#### c ####
# scatterplot of x(t) against x(t+1) (without regression line)
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
text <- "Scatterplot of stationary time series: x(t) against x(t+1)"
plot(
    x = data.ts[-length(data.ts)], y = data.ts[-1],
    type = "p", xlab = "x(t)", ylab = "x(t+1)", main = text, col = "green4"
)

# scatterplot of x(t) against x(t+1) (with regression line)
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
text <- "Scatterplot of x(t) against against x(t+1) (with regression line)"
plot(
    x = data.ts[-length(data.ts)], y = data.ts[-1],
    type = "p", xlab = "x(t)", ylab = "x(t+1)", main = text, col = "green4"
)
abline(lm(formula = data.ts[-length(data.ts)] ~ data.ts[-1]),
    col = "red", lty = 2
)
legend(
    x = "topright", legend = c("(x(t),x(t+1))", "regression line  "),
    col = c("green4", "red"), pch = c(1, -1), lty = c(-1, 2)
)

#### d ####
acf(data.ts)
acf(data.ts)$acf