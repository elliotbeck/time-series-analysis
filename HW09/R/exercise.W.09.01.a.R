#
# R code for problem 1 (a) of homework 9
#

# load 'class.RDdata'
load("data/class.RData")

# select time series to analyse
ts.data <- us.data[, "unemp"]
mts.data <- us.data

# description of time series
mts.desc <- c("U.S. inflation rate", "U.S. unemployment rate")
mts.units <- c("per cent per year", "per cent")

# specifiation for ac.f.
acf.lag.max <- 4 * 12

# specification for SARIMA estimation
SARIMA.ic <- "BIC" # chose among: AIC, AICc, and BIC
stepwise.model.selection <- TRUE
SARIMA.specs <- list(
   d = NA, D = NA, max.p = 12, max.q = 12, max.P = 12,
   max.Q = 12, max.order = 48, start.p = 00, start.q = 00, start.P = 00,
   start.Q = 00, stationary = FALSE
)

# specification for VAR estimation
VAR.ic <- "SC" # chose among: AIC, HQ, SC, and FPE

# specification for prediction
pred.horizon <- 12
pred.level <- 95

# load libraries
library(forecast)
library(gdata)
library(vars)

# definition of delimiters for output
delimiter.stars <- paste(
   "\n", paste(rep("*", 80), sep = "", collapse = ""),
   "\n",
   sep = ""
)
delimiter.hyphen <- paste(paste(rep("-", 80), sep = "", collapse = ""), "\n",
   sep = ""
)

# define time series
ts.data.1stdiff <- diff(ts.data, 1)

# define lables for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))
lag.in.years.label <- expression(paste("lag ", tau, " ( in years)", sep = ""))


# plot time series and associated ac.f and pac.f
pdf(paste("HW09/figures/exercise.W.09.01.a - ", mts.desc[2], ".pdf"))

layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
   x = ts.data, type = "b", xlab = "time", ylab = mts.units[2],
   main = "rawseries", col = "green4", font.main = 1, cex.main = 1
)
plot(
   x = ts.data.1stdiff, type = "b", xlab = "time", ylab = mts.units[2],
   main = "first differences of raw series", col = "green4", font.main = 1,
   cex.main = 1
)
acf(
   x = ts.data, lag.max = acf.lag.max, type = "correlation",
   xlab = lag.in.years.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = ts.data, lag.max = acf.lag.max, type = "partial",
   xlab = lag.in.years.label, ylab = pacf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
acf(
   x = ts.data.1stdiff, lag.max = acf.lag.max, type = "correlation",
   xlab = lag.in.years.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = ts.data.1stdiff, lag.max = acf.lag.max, type = "partial",
   xlab = lag.in.years.label, ylab = pacf.label, main = ""
)

title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(
   text = paste("Exercise 9.1 (a): ", mts.desc[2], sep = ""), side = 3,
   line = 0, outer = T, font = 2, cex = 1
)

dev.off()


# search for best SARIMA model

# open connection to write output to file
output.file <- paste(
   "HW09/figures/exercise.W.09.01.a - SARIMA model selection - ",
   mts.desc[2], "", ".txt",
   sep = ""
)
sink(file = output.file, append = F, type = "output", split = F)

# write output to file
cat(delimiter.stars)
cat("automatic model selection (selection criterion:", SARIMA.ic, ")\n")
cat(delimiter.hyphen)

# automatic model selection
ts.data.SARIMA.fit <- auto.arima(
   x = ts.data, d = SARIMA.specs$d,
   D = SARIMA.specs$D, max.p = SARIMA.specs$max.p, max.q = SARIMA.specs$max.q,
   max.P = SARIMA.specs$max.P, max.Q = SARIMA.specs$max.Q,
   max.order = SARIMA.specs$max.order, start.p = SARIMA.specs$start.p,
   start.q = SARIMA.specs$start.q, start.P = SARIMA.specs$start.P,
   start.Q = SARIMA.specs$start.Q, stationary = SARIMA.specs$stationary,
   ic = tolower(SARIMA.ic), stepwise = stepwise.model.selection,
   trace = T, approximation = F
)
cat(delimiter.hyphen)
print(ts.data.SARIMA.fit)
cat(delimiter.stars)

# close connection to write output to file and display file
sink()
file.show(output.file,
   title = paste(
      "HW09/figures/exercise.W.09.01.a - SARIMA model selection - ",
      mts.desc[2], "", ".txt",
      sep = ""
   )
)

# diagnostic plots
pdf(paste(
   "HW09/figures/exercise.W.09.01.a - SARIMA model - diagnostic plots - ",
   mts.desc[2], ".pdf"
))

layout(matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 12, 2))
plot(
   x = ts.data.SARIMA.fit$residuals, type = "b", xlab = "time",
   ylab = mts.units[2], main = "residuals", col = "green4",
   font.main = 1, cex.main = 1
)
acf(
   x = ts.data.SARIMA.fit$residuals, lag.max = acf.lag.max,
   type = "correlation", xlab = lag.in.years.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
Ljung.Box.p.value <- NULL
for (k in 1:acf.lag.max) {
   Ljung.Box.p.value <- c(Ljung.Box.p.value, Box.test(
      x = ts.data.SARIMA.fit$residuals, lag = k, type = "Ljung-Box"
   )$p.value)
}
plot(
   x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1),
   xlab = lag.in.years.label, ylab = "p-value",
   main = "p-values for Ljung-Box statistics", col = "green4",
   font.main = 1, cex.main = 1
)
pacf(
   x = ts.data.SARIMA.fit$residuals, lag.max = acf.lag.max,
   xlab = lag.in.years.label, ylab = acf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(text = paste("Exercise 9.1 (a): SARIMA model - diagnostic plots - ",
   mts.desc[2],
   sep = ""
), side = 3, line = 6, outer = T, font = 2, cex = 1)
mtext(paste("model selection criterion: ", SARIMA.ic, "\nbest model: ",
   trim(capture.output(ts.data.SARIMA.fit)[2]),
   sep = ""
),
side = 3, line = 1,
outer = T, at = 0.5
)

dev.off()

# search for best VAR model
mts.data.VAR.ic <- VARselect(
   y = mts.data, lag.max = 24, type = "both", season = NULL
)
mts.data.VAR.fit <- VAR(
   y = mts.data, type = "both", season = NULL, lag.max = 24, ic = VAR.ic
)

# open connection to write output to file
output.file <- "HW09/figures/exercise.W.09.01.a - VAR model - selection and estimation.txt"
sink(file = output.file, append = F, type = "output", split = F)

# write output to file

# estimated model
cat(delimiter.stars)
cat("comparison of information criteria:\n\n")
print(mts.data.VAR.ic)

# estimated model
cat(delimiter.stars)
cat("automatic model selection (selection criterion:", VAR.ic, ")\n")
print(summary(mts.data.VAR.fit))

# Portmanteau test
cat(delimiter.stars)
cat("Portmanteau test:\n")
print(serial.test(x = mts.data.VAR.fit, lags.pt = 24, type = "PT.asymptotic"))

# Ljung-Box-Pierce test
cat(delimiter.stars)
cat("Ljung-Box-Pierce test:\n")
print(serial.test(x = mts.data.VAR.fit, lags.pt = 24, type = "PT.adjusted"))

# close connection to write output to file and display file
sink()
file.show(output.file,
   title = "HW09/figures/exercise.W.09.01.a - VAR model - selection and estimation.txt"
)

# diagnostic plots
for (s in 1:2) {
   pdf(
      paste0(
         "HW09/figures/exercise.W.09.01.a - VAR model - diagnostic plots - equation for ",
         mts.desc[s], ".pdf"
      )
   )

   s <- 2
   res <- ts(
      data = residuals(mts.data.VAR.fit)[, s],
      start = start(mts.data[, s]), frequency = frequency(mts.data[, s])
   )
   layout(matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = T))
   par(oma = c(0, 2, 12, 2))
   plot(
      x = res, type = "b", xlab = "time", ylab = mts.units[s],
      main = "residuals", col = "green4", font.main = 1, cex.main = 1
   )
   acf(
      x = res, lag.max = acf.lag.max, type = "correlation",
      xlab = lag.in.years.label, ylab = acf.label, main = ""
   )
   title(main = "sample ac.f.", font.main = 1, cex.main = 1)
   Ljung.Box.p.value <- NULL
   for (k in 1:acf.lag.max) {
      Ljung.Box.p.value <- c(
         Ljung.Box.p.value,
         Box.test(x = res, lag = k, type = "Ljung-Box")$p.value
      )
   }
   plot(
      x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1),
      xlab = lag.in.years.label, ylab = "p-value",
      main = "p-values for Ljung-Box statistics", col = "green4",
      font.main = 1, cex.main = 1
   )
   pacf(
      x = res, lag.max = acf.lag.max, xlab = lag.in.years.label,
      ylab = acf.label, main = ""
   )
   title(main = "sample pac.f.", font.main = 1, cex.main = 1)
   mtext(text = paste(
      "HW09/figures/Exercise 9.1 (a): VAR model - diagnostic plots - equation for ",
      mts.desc[s],
      sep = ""
   ), side = 3, line = 6, outer = T, font = 2, cex = 1)
   mtext(paste("model selection criterion: ", VAR.ic, "\nbest model: VAR(",
      mts.data.VAR.fit$p, ")",
      sep = ""
   ), side = 3, line = 1, outer = T, at = 0.5)

   dev.off()
}

# predict with SARIMA
ts.data.sarima.pred <- forecast(
   object = ts.data.SARIMA.fit, h = pred.horizon,
   level = pred.level
)


# predict with VAR
mts.data.VAR.pred <- predict(
   object = mts.data.VAR.fit, n.ahead = pred.horizon,
   ci = pred.level / 100
)
# define forecast object
ts.data.VAR.pred <- list(
   method = "VAR",
   model = mts.data.VAR.pred$model,
   level = pred.level,
   mean = ts(
      data = mts.data.VAR.pred$fcst$unemp[, "fcst"],
      start = start(ts.data.sarima.pred$mean),
      frequency = frequency(ts.data.sarima.pred$mean)
   ),
   lower = matrix(mts.data.VAR.pred$fcst$unemp[, "lower"]),
   upper = matrix(mts.data.VAR.pred$fcst$unemp[, "upper"]),
   x = us.data[, "unemp"],
   xname = NULL,
   fitted = ts(
      data = fitted(mts.data.VAR.fit)[, "unemp"],
      start = start(ts.data.sarima.pred$fitted),
      frequency = frequency(ts.data.sarima.pred$fitted)
   ),
   residuals = ts(
      data = residuals(mts.data.VAR.fit)[, "unemp"],
      start = start(ts.data.sarima.pred$residuals),
      frequency = frequency(ts.data.sarima.pred$residuals)
   )
)
class(ts.data.VAR.pred) <- "forecast"



# plot point predictions and prediction intervals
pdf(paste(
   "HW09/figures/exercise.W.09.01.a - ", mts.desc[2],
   " - comparison of predictions and prediction intervals.pdf"
))

layout(matrix(c(1, 1, 2, 3, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
   x = ts.data.sarima.pred, , xlab = "time", ylab = mts.units[2],
   include = 12 * 10, shaded = T, shadecols = "pink1", main = "",
   col = "blue", fcol = "red", type = "b"
)
title(main = "Prediction method: SARIMA model", font.main = 1, cex.main = 1)
qqnorm(y = residuals(ts.data.SARIMA.fit), main = "", col = "green4")
qqline(y = residuals(ts.data.SARIMA.fit), col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
plot(
   x = ts.data.VAR.pred, , xlab = "time", ylab = mts.units[2],
   include = 12 * 10, , shaded = T, shadecols = "pink1", main = "",
   col = "blue", fcol = "red", type = "b"
)
title(main = "Prediction method: VAR model", font.main = 1, cex.main = 1)
qqnorm(y = residuals(mts.data.VAR.fit)[, "unemp"], main = "", col = "green4")
qqline(y = residuals(mts.data.VAR.fit)[, "unemp"], col = "black")
title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
mtext(text = paste("Exercise 9.1 (a): ", mts.desc[2],
   " - predictions and prediction intervals (coverage prob: ", pred.level, "%)",
   sep = ""
), side = 3, line = 0, outer = T, font = 2, cex = 1)

dev.off()


# comparison of predictions and prediction intervals
sarima.pred.ci.length <- ts.data.sarima.pred$upper - ts.data.sarima.pred$lower
dimnames(sarima.pred.ci.length) <- list(
   paste("horizon =", 1:pred.horizon),
   "SARIMA"
)
VAR.pred.ci.length <- ts.data.VAR.pred$upper - ts.data.VAR.pred$lower
dimnames(VAR.pred.ci.length) <- list(paste("horizon =", 1:pred.horizon), "VAR")
ci.length.abs.difference <- as.numeric(sarima.pred.ci.length -
   VAR.pred.ci.length)
ci.length.rel.difference <- ci.length.abs.difference /
   as.numeric(VAR.pred.ci.length)
pred.comp <- data.frame(
   SARIMA = as.numeric(ts.data.sarima.pred$mean),
   VAR = as.numeric(ts.data.VAR.pred$mean)
)
dimnames(pred.comp)[[1]] <- paste("horizon =", 1:pred.horizon)

# open connection to write output to file
output.file <- c(
   "HW09/figures/exercise.W.09.01.a - comparison of predictions and prediction intervals.txt"
)
sink(file = output.file, append = F, type = "output", split = F)

# write output to file
cat(delimiter.stars)
cat("comparison of predictions:\n\n")
print(pred.comp)
cat(delimiter.stars)
cat("comparison of lengths of prediction intervals:\n\n")
print(data.frame(sarima.pred.ci.length, VAR.pred.ci.length,
   abs.Difference = ci.length.abs.difference,
   rel.Difference = ci.length.rel.difference
))
cat(delimiter.stars)

# close connection to write output to file and display file
sink()
file.show(output.file,
   title = c(
      "HW09/figures/exercise.W.09.01.a - comparison of predictions and prediction intervals.txt"
   )
)