#
# R code for exercise 2.4 in Chatfield & Xing
#

# clear memory and close all graphics
rm(list = ls())

# construct acf object
x <- c(1, 0.02, 0.05, -0.09, 0.08, -0.02, 0.00, 0.12, 0.06, 0.02, -0.08)
a <- acf(x = rnorm(400), lag.max = 10, type = "correlation", plot = F)
a$acf[, , 1] <- x


# plot the given acf
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = a, xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")),
    col = "green4", main = "Correlogram"
)
legend(
    x = "topright", legend = c("acf", "95% confidence bands "),
    col = c("green4", "blue"), lty = c(1, 2)
)