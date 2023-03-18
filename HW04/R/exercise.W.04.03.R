#
# R code for exercise 3 of homework 4


# user in put
max.lag <- 10 # maximum lag for pac.f.
ar.coef <- c(0.9, -0.3)
series.length <- 100000 # length of simulated series

# load packages
library(stats)

# simulate AR(2) process
AR2.sim <- arima.sim(n = series.length, list(ar = ar.coef, ma = 0))

# compute pac.f.
pac.f.AR2.sim <- pacf(x = AR2.sim, lag.max = max.lag, plot = FALSE)

# plot ac.f.
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = 1:max.lag, y = pac.f.AR2.sim$acf, type = "h", lwd = 2,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("pac.f.(", tau, ")", sep = "")),
    col = "green4", main = ""
)
lines(x = c(-1, max.lag + 1), y = c(0, 0))
lines(
    x = c(-1, max.lag + 1), y = c(-2, -2) / sqrt(series.length),
    lty = 2, col = "blue"
)
lines(
    x = c(-1, max.lag + 1), y = c(2, 2) / sqrt(series.length), lty = 2,
    col = "blue"
)
axis(
    side = 2, at = c(seq(-1, -0.1, 0.1), seq(0.1, 1, 0.1)),
    labels = c(seq(-1, -0.1, 0.1), seq(0.1, 1, 0.1)), tick = T
)
title(
    main = "Sample partial autocorrelation function of simulated AR(2) process",
    font.main = 1, cex.main = 1
)
title.text <- paste("AR(2)-process X(t) = ", ifelse(ar.coef[1] == 0, " ",
    paste(sprintf("%+g", ar.coef[1]), "* X(t-1) ", sep = "")
),
sprintf("%+g", ar.coef[2]), "* X(t-2) + Z(t)",
sep = ""
)
mtext(text = title.text, side = 3, line = 0, outer = T, font = 2, cex = 1)

# construct variables for regression
x <- as.numeric(AR2.sim)
x.lag.0 <- x[3:length(x)]
x.lag.1 <- x[2:(length(x) - 1)]
x.lag.2 <- x[1:(length(x) - 2)]

# regress x(t) on x(t-1)
fit.1 <- lm(x.lag.0 ~ x.lag.1)

# regress x(t) on x(t-1) and x(t-2)
fit.2 <- lm(x.lag.0 ~ x.lag.1 + x.lag.2)

# write output to file
print(summary(fit.1))
print(summary(fit.2))