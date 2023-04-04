#
# R code for exercise 3 (c) of homework 6 (series: U.S. money supply)
#

# choose max lag
max.lag <- 24

# maximum lag for p-values for Ljung-Box statistics
example.str <- "exercise 3(c)"

# load libraries
library(forecast)
library(gdata)

# load 'class.RDdata'
load("data/class.RData")

# time series
ts.data <- us.money.supp
ts.desc <- "U.S. money supply"
# create times series objects
ts.data.raw <- ts.data
ts.data.1stdiff <- diff(ts.data.raw, 1)
ts.data.log <- log(ts.data.raw)
ts.data.log.1stdiff <- diff(log(ts.data.raw), 1)

# plot time series and associated ac.f. and pac.f.
layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 12, 2))
plot(
   x = ts.data.raw, type = "b", xlab = "time",
   ylab = "", main = "raw series", col = "green4",
   font.main = 1, cex.main = 1
)
plot(
   x = ts.data.log, type = "b", xlab = "time",
   ylab = "", main = "log series", col = "green4",
   font.main = 1, cex.main = 1
)
plot(
   x = ts.data.1stdiff, type = "b", xlab = "time",
   ylab = "", main = "first differences of raw series",
   col = "green4", font.main = 1, cex.main = 1
)
plot(
   x = ts.data.log.1stdiff, type = "b", xlab = "time",
   ylab = "", main = "first differences of log series",
   col = "green4", font.main = 1, cex.main = 1
)
mtext(text = paste("Exercise 6.3 (c): ", ts.desc,
   " (raw and log series)",
   sep = ""
), side = 3, line = 6, outer = T, font = 2, cex = 1)

# fit SARIMA model
arima.fit <- arima(
   x = ts.data.log, order = c(0, 1, 0),
   seasonal = list(order = c(0, 0, 0), period = 1),
   include.mean = TRUE, method = "ML"
)

# print estimated model to screen
print(arima.fit)
# compute ac.f. and pac.f. according to estimated model
if (length(coef(arima.fit)) != 0) {
   ar.coef <- coef(arima.fit)[substr(
      x = names(coef(arima.fit)),
      start = 1, stop = 2
   ) == "ar"]
   ma.coef <- coef(arima.fit)[substr(
      x = names(coef(arima.fit)),
      start = 1, stop = 2
   ) == "ma"]
} else {
   ar.coef <- 0
   ma.coef <- 0
}
theo.acf <- ARMAacf(
   ar = ar.coef, ma = ma.coef,
   lag.max = max.lag, pacf = FALSE
)
theo.pacf <- c(1, ARMAacf(
   ar = ar.coef, ma = ma.coef,
   lag.max = max.lag, pacf = TRUE
))

# plot time series and associated ac.f. and pac.f.
layout(matrix(c(1, 1, 2, 3, 4, 4, 5, 6), nr = 2, byrow = T))
par(oma = c(0, 2, 12, 2))
plot(
   x = ts.data.log, type = "b", xlab = "time", ylab = "",
   main = "log series", col = "green4", font.main = 1, cex.main = 1
)
acf(
   x = ts.data.log, type = "correlation", xlab = expression(
      paste("lag ", tau, sep = "")
   ), ylab = expression(paste("ac.f.(", tau, ")", sep = "")),
   main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = ts.data.log, type = "partial",
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("pac.f.(", tau, ")", sep = "")),
   main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
plot(
   x = ts.data.log.1stdiff, type = "b", xlab = "time",
   ylab = "", main = "first differences of log series",
   col = "green4", font.main = 1, cex.main = 1
)
ts.acf <- acf(x = ts.data.log.1stdiff, type = "correlation", plot = FALSE)
plot(ts.acf,
   ylim = c(
      min(c(ts.acf$acf, theo.acf[-1])),
      max(c(ts.acf$acf, theo.acf[-1]))
   ),
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("ac.f.(", tau, ")", sep = "")), main = ""
)
points(
   x = 0:(length(theo.acf) - 1),
   y = theo.acf, type = "p", col = "red"
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
ts.pacf <- acf(x = ts.data.log.1stdiff, type = "partial", plot = FALSE)
plot(ts.pacf,
   ylim = c(
      min(c(ts.pacf$acf, theo.pacf[-1])),
      max(c(ts.pacf$acf, theo.pacf[-1]))
   ), xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("pac.f.(", tau, ")", sep = "")), main = ""
)
points(
   x = 0:(length(theo.pacf) - 1), y = theo.pacf,
   type = "p", col = "red"
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(text = paste("Exercise 6.3 (c): ", ts.desc,
   " (log series)",
   sep = ""
), side = 3, line = 6, outer = T, font = 2, cex = 1)
mtext(paste("model: ", trim(capture.output(print(arima.fit))[2]), sep = ""),
   side = 3, line = 1, outer = T, at = 0.5, col = "red"
)
# save graphics
save.graphics(file = paste(output.dir, "exercise.W.06.03.c [1] - ",
   trim(capture.output(print(arima.fit))[2]),
   " model - time plots and (p)ac.f..", graph.type,
   sep = ""
), type = graph.type)
# diagnostic plots
layout(matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 12, 2))
plot(
   x = arima.fit$residuals, type = "b", xlab = "time",
   ylab = "", main = "standardized residuals",
   col = "green4", font.main = 1, cex.main = 1
)
acf(
   x = arima.fit$residuals, lag.max = max.lag,
   type = "correlation",
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("ac.f.(", tau, ")", sep = "")), main = ""
)
title(
   main = "sample ac.f. of standardized residuals",
   font.main = 1, cex.main = 1
)
acf(
   x = arima.fit$residuals, lag.max = max.lag, type = "partial",
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("pac.f.(", tau, ")", sep = "")), main = ""
)
title(
   main = "sample pac.f. of standardized residuals",
   font.main = 1, cex.main = 1
)
Ljung.Box.p.value <- NULL
for (k in 1:max.lag) {
   Ljung.Box.p.value <- c(
      Ljung.Box.p.value,
      Box.test(x = arima.fit$residuals, lag = k, type = "Ljung-Box")$p.value
   )
}
plot(
   x = 1:max.lag, y = Ljung.Box.p.value, ylim = c(0, 1),
   xlab = expression(paste("lag ", tau, sep = "")), ylab = "p-value",
   main = "p-values for Ljung-Box statistic", col = "green4",
   font.main = 1, cex.main = 1
)
lines(x = c(-5, max.lag + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
mtext(text = paste("Exercise 6.3 (c): ", ts.desc,
   " (log series) - diagnostic plots",
   sep = ""
), side = 3, line = 6, outer = TRUE, font = 2, cex = 1)
mtext(paste("model: ", trim(capture.output(print(arima.fit))[2]), sep = ""),
   side = 3, line = 1, outer = T, at = 0.5
)