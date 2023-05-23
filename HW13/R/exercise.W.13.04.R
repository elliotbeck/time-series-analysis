#
# R code for problem 4 of homework 13
#

# user input
acf.lag.max <- 4 * 4 # specifiation for ac.f.


# load 'class.RData'
load("data/class.RData")


# select time series to analyze
US.gdp <- us.data.hill[, 1]
US.inf <- us.data.hill[, 2]

# define lables for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))


# plot time series
par(oma = c(0, 2, 4, 2))
plot(
   x = diff(log(US.gdp), 1), type = "b", xlab = "time", ylab = "log USD",
   col = "green4"
)
title(main = "first differences in log U.S. GDP", font.main = 1, cex.main = 1)
plot(
   x = diff(US.inf, 1), type = "b", xlab = "time",
   ylab = "percent", col = "green4"
)
title(
   main = "first differences in U.S. inflation rate", font.main = 1,
   cex.main = 1
)
mtext(
   text = paste("Exercise 13.4: U.S.time series - time plots"), side = 3,
   line = 0, outer = TRUE, font = 2, cex = 1
)

# adf test
# variable: first differences in log U.S. GDP
g.adf.test.aic <- adf.test.aic(
   x = diff(log(US.gdp), 1), p.max = 10,
   time.trend = FALSE, acf.plot = FALSE
)
# variable: first differences in log U.S. inflation rates
i.adf.test.aic <- adf.test.aic(
   x = diff(log(US.inf), 1), p.max = 10,
   time.trend = FALSE, acf.plot = FALSE
)

# U.S. GDP: plot diagramms for diagnosic checks (1)
par(oma = c(0, 2, 4, 2))
par(mfrow = c(2, 2))
plot(g.adf.test.aic$fit,
   which = 1:4, sub.caption = "",
   font.main = 1, cex.main = 1
)
mtext(
   text = "Exercise 13.4: U.S. GDP - ADF test - diagnostic plots (1)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

#  U.S. GDP: plot diagramms for diagnosic checks (2)
res <- g.adf.test.aic$fit$resid
layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(
   x = res, type = "b", xlab = "time", ylab = "",
   main = "residuals", col = "green4", font.main = 1, cex.main = 1
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
   text = "Exercise 13.4: U.S. GDP - ADF test - diagnostic plots (2)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

# U.S. inflation rate: plot diagramms for diagnosic checks (1)
par(oma = c(0, 2, 4, 2))
par(mfrow = c(2, 2))
plot(i.adf.test.aic$fit,
   which = 1:4, sub.caption = "",
   font.main = 1, cex.main = 1
)
mtext(
   text = "Exercise 13.4: U.S. inflation rate - ADF test - diagnostic plots (1)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)

#  U.S. inflation rate: plot diagramms for diagnosic checks (2)
res <- i.adf.test.aic$fit$resid
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
   x = (1:acf.lag.max) / 4,
   y = Ljung.Box.p.value, ylim = c(0, 1),
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
   text = "Exercise 13.4: U.S. inflation rate - ADF test - diagnostic plots (2)",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)
