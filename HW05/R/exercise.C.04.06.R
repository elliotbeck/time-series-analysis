#
# R code for exercise 4.6 in Chatfield
#

# user in put
n <- 60
x.sample.acf <- c(1.00, 0.95, 0.91, 0.87, 0.82, 0.79, 0.74, 0.70, 0.67)
x.sample.pacf <- c(1.00, 0.95, 0.04, -0.05, 0.07, 0.00, 0.07, -0.04, -0.02)
D.x.sample.acf <- c(1.00, 0.02, 0.08, 0.12, 0.05, -0.02, -0.05, -0.01, 0.03)
D.x.sample.pacf <- c(1.00, 0.02, 0.08, 0.06, 0.03, -0.05, -0.06, -0.04, -0.02)


# plot sample ac.f.s and sample parital ac.f.s
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))

# plot sample ac.f. of series
plot(
    x = 0:8, y = x.sample.acf, type = "h", lwd = 2,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("sample ac.f.(", tau, ")", sep = "")),
    ylim = c(0, 1), yaxp = c(0, 1, 10), col = "green4", main = ""
)
lines(x = c(-1, 9), y = c(0, 0))
lines(
    x = c(-1, 9), y = (-1 / n) - 1.96 * c(1, 1) / sqrt(n),
    lty = 2, col = "blue"
)
lines(
    x = c(-1, 9), y = (-1 / n) + 1.96 * c(1, 1) / sqrt(n),
    lty = 2, col = "blue"
)
title(main = "Series: x(t)", font.main = 1, cex.main = 1)

# plot sample ac.f. of differenced series
plot(
    x = 0:8, y = D.x.sample.acf, type = "h", lwd = 2,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("sample ac.f.(", tau, ")", sep = "")),
    ylim = c(0, 1), yaxp = c(0, 1, 10), col = "green4", main = ""
)
lines(x = c(-1, 9), y = c(0, 0))
lines(
    x = c(-1, 9), y = (-1 / n) - 1.96 * c(1, 1) / sqrt(n), lty = 2,
    col = "blue"
)
lines(
    x = c(-1, 9), y = (-1 / n) + 1.96 * c(1, 1) / sqrt(n), lty = 2,
    col = "blue"
)
title(main = "Series: x(t)-x(t-1)", font.main = 1, cex.main = 1)

# plot sample pac.f. of series
plot(
    x = 0:8, y = x.sample.pacf, type = "h", lwd = 2,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("sample pac.f.(", tau, ")", sep = "")),
    ylim = c(0, 1), yaxp = c(0, 1, 10), col = "purple", main = ""
)
lines(x = c(-1, 9), y = c(0, 0))
lines(
    x = c(-1, 9), y = (-1 / n) - 1.96 * c(1, 1) / sqrt(n), lty = 2,
    col = "blue"
)
lines(
    x = c(-1, 9), y = (-1 / n) + 1.96 * c(1, 1) / sqrt(n), lty = 2,
    col = "blue"
)
title(main = "Series: x(t)", font.main = 1, cex.main = 1)

# plot sample pac.f. of differenced series
plot(
    x = 0:8, y = D.x.sample.pacf, type = "h", lwd = 2,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("sample pac.f.(", tau, ")", sep = "")),
    ylim = c(0, 1), yaxp = c(0, 1, 10), col = "purple", main = ""
)
lines(x = c(-1, 9), y = c(0, 0))
lines(
    x = c(-1, 9), y = (-1 / n) - 1.96 * c(1, 1) / sqrt(n),
    lty = 2, col = "blue"
)
lines(
    x = c(-1, 9), y = (-1 / n) + 1.96 * c(1, 1) / sqrt(n),
    lty = 2, col = "blue"
)
title(main = "Series: x(t)-x(t-1)", font.main = 1, cex.main = 1)

mtext(
    text = "Exercise 4.6: Sample ac.f.s and sample pac.f.s", side = 3,
    line = 0, outer = T, font = 2, cex = 1
)