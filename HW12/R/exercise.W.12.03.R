#
# R code for problem 3 of homework 12
#

# load 'class.RData'
load("data/class.RData")
library(urca)


# define lables for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))

# time series to analyse
d.infl <- diff(us.data.hill[, 2], )

# adf test
infl.adf.test.aic <- adf.test.aic(
   x = d.infl, p.max = 10, time.trend = "drift",
   acf.plot = F
)
summary(ur.df(d.infl, type = "drift", lags = 10, selectlags = "AIC"))

# plot diagramms for diagnosic checks (1)
par(oma = c(0, 2, 4, 2))
par(mfrow = c(2, 2))
plot(infl.adf.test.aic$fit,
   which = 1:4, sub.caption = "", font.main = 1,
   cex.main = 1
)
mtext(
   text = "Exercise 12.3: ADF test - diagnostic plots (1)", side = 3,
   line = 0, outer = TRUE, font = 2, cex = 1
)



# plot diagramms for diagnosic checks (2)

res <- infl.adf.test.aic$fit$resid
acf.lag.max <- 6 * 4 # specifiation for ac.f.
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
   text = "Exercise 12.3: ADF test - diagnostic plots (2)", side = 3,
   line = 0, outer = TRUE, font = 2, cex = 1
)
