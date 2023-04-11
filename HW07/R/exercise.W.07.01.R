#
# R code for problem 1 of homework 7
#

# user in put
acf.lag.max <- 36 # max lag for ac.f./pac.f.
raw.SARIMA.spec <- list(
   p = 1, d = 1, q = 1, P = 2, D = 1,
   Q = 1, freq = 12
) # SARIMA model for raw series
log.SARIMA.spec <- list(p = 1, d = 1, q = 1, P = 2, D = 1, Q = 1, freq = 12) # SARIMA model for log series
forecast.horizon <- 12 # specifications for forecasting: horizon
forecast.level <- 95 # specifications for forecasting: confidence level for prediction intervals


# load 'class.RData'
load("data/class.RData")

# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80), sep = "", collapse = ""), "\n", sep = "")
delimiter.hyphen <- paste(paste(rep("-", 80), sep = "", collapse = ""), "\n", sep = "")

# load libraries
library(forecast) # function: auto.arima
library(gdata) # function: trim


# define time series
ts.data.raw <- fed.prod
ts.data.1stdiff <- diff(ts.data.raw)
ts.data.log <- log(ts.data.raw)
ts.data.log.1stdiff <- diff(ts.data.log, 1)



# plot time series and associated ac.f and pac.f and (raw series)
layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(x = ts.data.raw, type = "b", xlab = "time", ylab = "", main = "raw series", col = "green4", font.main = 1, cex.main = 1)
plot(x = ts.data.1stdiff, type = "b", xlab = "time", ylab = "", main = "first differences of raw series", col = "green4", font.main = 1, cex.main = 1)
acf(x = ts.data.raw, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.raw, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "pac.f.", main = "")
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.1stdiff, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.1stdiff, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "pac.f.", main = "")
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(text = "Exercise 7.1: U.S. Production Index (raw series)", side = 3, line = 0, outer = T, font = 2, cex = 1)


# plot time series and associated ac.f and pac.f and (log series)
layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6), nr = 2, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(x = ts.data.log, type = "b", xlab = "time", ylab = "", main = "log series", col = "green4", font.main = 1, cex.main = 1)
plot(x = ts.data.log.1stdiff, type = "b", xlab = "time", ylab = "", main = "first differences of log series", col = "green4", font.main = 1, cex.main = 1)
acf(x = ts.data.log, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.log, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "pac.f.", main = "")
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.log.1stdiff, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(x = ts.data.log.1stdiff, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "pac.f.", main = "")
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(text = "Exercise 7.1: U.S. Production Index (log series)", side = 3, line = 0, outer = T, font = 2, cex = 1)


# estimate SARIMA model (raw series)
ts.data.raw.fit <- arima(x = ts.data.raw, order = c(raw.SARIMA.spec$p, raw.SARIMA.spec$d, raw.SARIMA.spec$q), seasonal = list(order = c(raw.SARIMA.spec$P, raw.SARIMA.spec$D, raw.SARIMA.spec$Q), period = raw.SARIMA.spec$freq))


# diagnostic plots (raw series)
layout(mat = matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = TRUE), heights = c(1, 1))
par(oma = c(0, 2, 12, 2))
plot(x = ts.data.raw.fit$residuals, type = "b", xlab = "time", ylab = "", main = "standardized residuals", col = "green4", font.main = 1, cex.main = 1)
acf(x = ts.data.raw.fit$residuals, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f. of standardized residuals", font.main = 1, cex.main = 1)
acf(x = ts.data.raw.fit$residuals, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample pac.f. of standardized residuals", font.main = 1, cex.main = 1)
Ljung.Box.p.value <- NULL
for (k in 1:acf.lag.max) {
   Ljung.Box.p.value <- c(Ljung.Box.p.value, Box.test(x = ts.data.raw.fit$residuals, lag = k, type = "Ljung-Box")$p.value)
}
plot(x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1), xlab = "lag", ylab = "p-value", main = "p-values for Ljung-Box statistics", col = "green4", font.main = 1, cex.main = 1)
lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
mtext(text = "Exercise 7.1: U.S. Production Index - raw series - diagnostic plots", side = 3, line = 6, outer = T, font = 2, cex = 1)
mtext(paste("model: ", trim(capture.output(ts.data.raw.fit)[2]), sep = ""), side = 3, line = 1, outer = T, at = 0.5)

# estimate SARIMA model (log series)
ts.data.log.fit <- arima(x = ts.data.log, order = c(log.SARIMA.spec$p, log.SARIMA.spec$d, log.SARIMA.spec$q), seasonal = list(order = c(log.SARIMA.spec$P, log.SARIMA.spec$D, log.SARIMA.spec$Q), period = log.SARIMA.spec$freq))

# diagnostic plots (log series)
layout(mat = matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = TRUE), heights = c(1, 1))
par(oma = c(0, 2, 12, 2))
plot(x = ts.data.log.fit$residuals, type = "b", xlab = "time", ylab = "", main = "standardized residuals", col = "green4", font.main = 1, cex.main = 1)
acf(x = ts.data.log.fit$residuals, lag.max = acf.lag.max, type = "correlation", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample ac.f. of standardized residuals", font.main = 1, cex.main = 1)
acf(x = ts.data.log.fit$residuals, lag.max = acf.lag.max, type = "partial", xlab = "lag", ylab = "ac.f.", main = "")
title(main = "sample pac.f. of standardized residuals", font.main = 1, cex.main = 1)
Ljung.Box.p.value <- NULL
for (k in 1:acf.lag.max) {
   Ljung.Box.p.value <- c(Ljung.Box.p.value, Box.test(x = ts.data.log.fit$residuals, lag = k, type = "Ljung-Box")$p.value)
}
plot(x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1), xlab = "lag", ylab = "p-value", main = "p-values for Ljung-Box statistics", col = "green4", font.main = 1, cex.main = 1)
lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
mtext(text = "Exercise 7.1: U.S. Production Index - log series - diagnostic plots", side = 3, line = 6, outer = T, font = 2, cex = 1)
mtext(paste("model: ", trim(capture.output(ts.data.log.fit)[2]), sep = ""), side = 3, line = 1, outer = T, at = 0.5)


# forecast with exponential smoothing
ts.data.raw.ets <- ets(y = ts.data.raw)
ts.data.log.ets <- ets(y = ts.data.log)
ts.data.raw.ets.pred <- forecast(object = ts.data.raw.ets, h = forecast.horizon, level = forecast.level)
ts.data.log.ets.pred <- forecast(object = ts.data.log.ets, h = forecast.horizon, level = forecast.level)



# forecast with SARIMA
ts.data.raw.sarima.pred <- forecast(object = ts.data.raw.fit, h = forecast.horizon, level = forecast.level)
ts.data.log.sarima.pred <- forecast(object = ts.data.log.fit, h = forecast.horizon, level = forecast.level)



# plot point forecasts and forecast intervals (raw series)
layout(matrix(c(1, 1, 2, 3, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(x = ts.data.raw.ets.pred, include = 12 * 10, shaded = T, shadecols = "pink1", main = "", col = "blue", fcol = "red", type = "b", xlab = "time")
title(main = "Forecast method: SARIMA", font.main = 1, cex.main = 1)
qqnorm(y = ts.data.raw.ets.pred$residuals, main = "", col = "green4")
qqline(y = ts.data.raw.ets.pred$residuals, col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
plot(x = ts.data.raw.sarima.pred, include = 12 * 10, shaded = T, shadecols = "pink1", main = "", col = "blue", fcol = "red", type = "b", xlab = "time")
title(main = "Forecast method: exponential smoothing", font.main = 1, cex.main = 1)
qqnorm(y = ts.data.raw.sarima.pred$residuals, main = "", col = "green4")
qqline(y = ts.data.raw.sarima.pred$residuals, col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
mtext(text = "Exercise 7.1: U.S. Production Index (raw series) - forecasts", side = 3, line = 0, outer = T, font = 2, cex = 1)


# plot point forecasts and forecast intervals (log series)
layout(matrix(c(1, 1, 2, 3, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(x = ts.data.log.ets.pred, include = 12 * 10, shaded = T, shadecols = "pink1", main = "", col = "blue", fcol = "red", type = "b", xlab = "time")
title(main = "Forecast method: SARIMA", font.main = 1, cex.main = 1)
qqnorm(y = ts.data.log.ets.pred$residuals, main = "", col = "green4")
qqline(y = ts.data.log.ets.pred$residuals, col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
plot(x = ts.data.log.sarima.pred, include = 12 * 10, shaded = T, shadecols = "pink1", main = "", col = "blue", fcol = "red", type = "b", xlab = "time")
title(main = "Forecast method: exponential smoothing", font.main = 1, cex.main = 1)
qqnorm(y = ts.data.log.sarima.pred$residuals, main = "", col = "green4")
qqline(y = ts.data.log.sarima.pred$residuals, col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
mtext(text = "Exercise 7.1: U.S. Production Index (log series - forecasts)", side = 3, line = 0, outer = T, font = 2, cex = 1)