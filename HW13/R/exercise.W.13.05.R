#
# R code for problem 5 of homework 13
#

# user input
acf.lag.max <- 4 * 4 # specifiation for ac.f.

# load 'class.RData'
load("data/class.RData")

# select time series to analyze
US.gdp <- us.data.hill[, 1]

# define labels for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))


# plot time series
par(mfrow = c(2, 1))
par(oma = c(0, 2, 4, 2))
plot(
   x = log(US.gdp), type = "b", xlab = "time",
   ylab = "log USD", col = "green4"
)
title(main = "log U.S. GDP", font.main = 1, cex.main = 1)
plot(
   x = diff(log(US.gdp), 1), type = "b", xlab = "time",
   ylab = "log USD", col = "green4"
)
title(main = "first differences in log U.S. GDP", font.main = 1, cex.main = 1)
mtext(
   text = paste("Exercise 13.5: U.S. GDP - time plot"),
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

# adf test
# variable: log U.S. GDP
# ADF test without time trend
g.adf.test.aic <- adf.test.aic(
   x = log(US.gdp), p.max = 5,
   time.trend = FALSE, acf.plot = FALSE
)
# ADF test with time trend:
gt.adf.test.aic <- adf.test.aic(
   x = log(US.gdp), p.max = 5,
   time.trend = TRUE, acf.plot = FALSE
)

# U.S. GDP without time trend: plot diagrams for diagnostic checks (1)
par(oma = c(0, 2, 4, 2))
par(mfrow = c(2, 2))
plot(g.adf.test.aic$fit,
   which = 1:4, sub.caption = "",
   font.main = 1, cex.main = 1
)
mtext(
   text = "Exercise 13.5: U.S. GDP - ADF test without time trend - diagnostic plots (1)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

#  U.S. GDP without time trend: plot diagrams for diagnostic checks (2)
res <- g.adf.test.aic$fit$resid
layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(
   x = res, type = "b", xlab = "time", ylab = "", main = "residuals",
   col = "green4", font.main = 1, cex.main = 1
)
acf(
   x = res, lag.max = acf.lag.max, type = "correlation",
   xlab = expression(paste("lag ", tau, " (in quarters)", sep = "")),
   ylab = acf.label, main = ""
)
title(main = "sample ac.f. of residuals", font.main = 1, cex.main = 1)
Ljung.Box.p.value <- NULL
for (k in 1:acf.lag.max) {
   Ljung.Box.p.value <- c(
      Ljung.Box.p.value,
      Box.test(x = res, lag = k, type = "Ljung-Box")$p.value
   )
}
plot(
   x = (1:acf.lag.max) / 4, y = Ljung.Box.p.value,
   ylim = c(0, 1), xlab = expression(
      paste("lag ", tau, " (in years)", sep = "")
   ),
   ylab = "p-value", main = "p-values for Ljung-Box statistics",
   col = "green4", font.main = 1, cex.main = 1
)
lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
acf(
   x = res, lag.max = acf.lag.max, type = "partial",
   xlab = expression(paste("lag ", tau, " (in quaraters)", sep = "")),
   ylab = pacf.label, main = ""
)
title(main = "sample pac.f. of residuals", font.main = 1, cex.main = 1)
mtext(
   text = "Exercise 13.5: U.S. GDP - ADF test without time trend - diagnostic plots (2)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

# U.S. GDP with time trend: plot diagrams for diagnostic checks (1)
par(oma = c(0, 2, 4, 2))
par(mfrow = c(2, 2))
plot(gt.adf.test.aic$fit,
   which = 1:4, sub.caption = "", font.main = 1,
   cex.main = 1
)
mtext(
   text = "Exercise 13.5: U.S. GDP - ADF test with time trend - diagnostic plots (1)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

#  U.S. GDP with time trend: plot diagrams for diagnostic checks (2)
res <- gt.adf.test.aic$fit$resid
layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(
   x = res, type = "b", xlab = "time", ylab = "", main = "residuals",
   col = "green4", font.main = 1, cex.main = 1
)
acf(
   x = res, lag.max = acf.lag.max, type = "correlation",
   xlab = expression(paste("lag ", tau, " (in quarters)", sep = "")),
   ylab = acf.label, main = ""
)
title(main = "sample ac.f. of residuals", font.main = 1, cex.main = 1)
Ljung.Box.p.value <- NULL
for (k in 1:acf.lag.max) {
   Ljung.Box.p.value <- c(
      Ljung.Box.p.value,
      Box.test(x = res, lag = k, type = "Ljung-Box")$p.value
   )
}
plot(
   x = (1:acf.lag.max) / 4, y = Ljung.Box.p.value, ylim = c(0, 1),
   xlab = expression(paste("lag ", tau, " (in years)", sep = "")),
   ylab = "p-value", main = "p-values for Ljung-Box statistics",
   col = "green4", font.main = 1, cex.main = 1
)
lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
acf(
   x = res, lag.max = acf.lag.max, type = "partial",
   xlab = expression(paste("lag ", tau, " (in quaraters)", sep = "")),
   ylab = pacf.label, main = ""
)
title(main = "sample pac.f. of residuals", font.main = 1, cex.main = 1)
mtext(
   text = "Exercise 13.5: U.S. GDP - ADF test with time trend - diagnostic plots (2)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)
