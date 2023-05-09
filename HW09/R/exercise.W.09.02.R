#
# R code for problem 2 of homework 9
#

# load libraries
library(forecast)
library(gdata)
library(vars)

# load 'class.RDdata'
load("data/class.RData")

# select time series to analyze
mts.var.names <- c("gdp", "gpdi") # you should also try c("gpdi","gdp") (then don't forget to change mts.desc too) and compare the results for the orthogonal impulse response functions
mts.data <- log(us.macro[, mts.var.names])

# description of time series
mts.desc <- c("log U.S. GDP", "log U.S. gross domestic private investment")
mts.units <- c("log USD", "log USD")

# specifiation for ac.f.
acf.lag.max <- 4 * 4

# specification for VAR estimation
VAR.ic <- "SC" # chose among: AIC, HQ, SC, and FPE

# specification for impulse response function
irf.horizon <- 10 * 4
irf.level <- 95

# specification for prediction
pred.horizon <- 4 * 4
pred.level <- 95

# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80), sep = "", collapse = ""), "\n", sep = "")
delimiter.hyphen <- paste(paste(rep("-", 80), sep = "", collapse = ""), "\n", sep = "")

# define lable for graphs
lag.in.years.label <- expression(paste("lag ", tau, " ( in years)", sep = ""))

# plot time series
pdf("HW09/figures/exercise.W.09.02 - U.S. macro time series.pdf")

layout(matrix(c(1, 2, 3, 4, 5, 6), nr = 3, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
   x = mts.data[, "gdp"], type = "b", xlab = "time",
   ylab = mts.units[1], col = "green4"
)
title(main = mts.desc[1], font.main = 1, cex.main = 1)
plot(
   x = diff(mts.data[, "gdp"], 1), type = "b", xlab = "time",
   ylab = mts.units[1], col = "green4"
)
title(main = paste("change in", mts.desc[1]), font.main = 1, cex.main = 1)
plot(
   x = mts.data[, "gpdi"], type = "b", xlab = "time",
   ylab = mts.units[2], col = "green4"
)
title(main = mts.desc[2], font.main = 1, cex.main = 1)
plot(
   x = diff(mts.data[, "gpdi"], 1), type = "b", xlab = "time",
   ylab = mts.units[2], col = "green4"
)
title(main = paste("change in", mts.desc[2]), font.main = 1, cex.main = 1)
plot(
   x = mts.data[, "gpdi"] - mts.data[, "gdp"], type = "b",
   xlab = "time", ylab = mts.units[1], col = "green4"
)
title(main = paste(mts.desc[2], "-", mts.desc[1]), font.main = 1, cex.main = 1)
mtext(
   text = paste("Exercise 9.2: U.S. macro time series"), side = 3, line = 0,
   outer = T, font = 2, cex = 1
)

dev.off()


# search for best VAR model
mts.data.VAR.ic <- VARselect(
   y = mts.data, lag.max = 4 * 4,
   type = "const", season = NULL
)
mts.data.VAR.fit <- VAR(
   y = mts.data, type = "const", season = NULL,
   lag.max = 4 * 4, ic = VAR.ic
)

# open connection to write output to file
output.file <- paste(
   "HW09/figures/exercise.W.09.02 - VAR model - selection and estimation.txt",
   sep = ""
)
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
print(serial.test(x = mts.data.VAR.fit, lags.pt = 6 * 4, type = "PT.adjusted"))

# close connection to write output to file and display file
sink()
file.show(output.file,
   title = paste("HW09/figures/exercise.W.09.02 - VAR model - selection and estimation.txt",
      sep = ""
   )
)

# diagnostic plots
for (s in 1:2) {
   pdf(paste("HW09/figures/exercise.W.09.02 - VAR model - diagnostic plots - equation for ",
      mts.desc[s], ".pdf",
      sep = ""
   ))
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
      xlab = lag.in.years.label, ylab = "ac.f.", main = ""
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
      x = (1:acf.lag.max) / frequency(mts.data[, s]),
      y = Ljung.Box.p.value, ylim = c(0, 1), xlab = lag.in.years.label,
      ylab = "p-value", main = "p-values for Ljung-Box statistics",
      col = "green4", font.main = 1, cex.main = 1
   )
   lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), lty = 2, col = "red")
   pacf(
      x = res, lag.max = acf.lag.max, xlab = lag.in.years.label,
      ylab = "ac.f.", main = ""
   )
   title(main = "sample pac.f.", font.main = 1, cex.main = 1)
   mtext(text = paste(
      "Exercise 9.2: VAR model - diagnostic plots - equation for ", mts.desc[s],
      sep = ""
   ), side = 3, line = 6, outer = T, font = 2, cex = 1)
   mtext(paste(
      "model selection criterion: ",
      VAR.ic, "\nbest model: VAR(", mts.data.VAR.fit$p, ")",
      sep = ""
   ), side = 3, line = 1, outer = T, at = 0.5)

   dev.off()
}

# plot impulse response functions
pdf("HW09/figures/exercise.W.09.02 - impulse response functions.pdf")

layout(matrix(data = c(1, 3, 2, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
i.r.pairs <- list(c(1, 1), c(1, 2), c(2, 1), c(2, 2))
for (i.r.pair in 1:length(i.r.pairs)) {
   mts.data.VAR.irf <- irf(
      x = mts.data.VAR.fit,
      impulse = mts.var.names[i.r.pairs[[i.r.pair]][1]],
      response = mts.var.names[i.r.pairs[[i.r.pair]][2]],
      n.ahead = irf.horizon, ortho = F, cumulative = F, boot = F,
      ci = irf.level / 100
   )
   irf.ts <- ts(
      data = mts.data.VAR.irf$irf[[1]][, 1], start = 0,
      frequency = frequency(mts.data)
   )
   irf.y.lim <- c(min(irf.ts), max(irf.ts))
   irf.y.factor <- 10 * ceiling(0.1 / (irf.y.lim[2] - irf.y.lim[1]))
   irf.y.lim <- c(
      floor(min(irf.ts) * irf.y.factor) / irf.y.factor,
      ceiling(max(irf.ts) * irf.y.factor) / irf.y.factor
   )
   plot(
      x = irf.ts, type = "b", xlab = "time (in years)", ylab = mts.units[1],
      ylim = irf.y.lim, col = "blue4"
   )
   if (sum(irf.y.lim > 0) > 0) {
      lines(ts(
         data = 0, start = start(irf.ts), end = end(irf.ts),
         frequency = frequency(irf.ts)
      ), type = "l", col = "red")
   }
   title(
      main = paste("impulse = ", mts.desc[i.r.pairs[[i.r.pair]][1]],
         "\nresponse = ", mts.desc[i.r.pairs[[i.r.pair]][2]],
         sep = ""
      ), font.main = 1, cex.main = 1
   )
}
mtext(
   text = paste("Exercise 9.2: Impulse response functions"),
   side = 3, line = 0, outer = T, font = 2, cex = 1
)

dev.off()

# plot orthogonal impulse response functions
pdf("HW09/figures/exercise.W.09.02 - orthogonal impulse response functions.pdf")

layout(matrix(data = c(1, 3, 2, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
i.r.pairs <- list(c(1, 1), c(1, 2), c(2, 1), c(2, 2))
for (i.r.pair in 1:length(i.r.pairs)) {
   mts.data.VAR.irf <- irf(
      x = mts.data.VAR.fit,
      impulse = mts.var.names[i.r.pairs[[i.r.pair]][1]],
      response = mts.var.names[i.r.pairs[[i.r.pair]][2]],
      n.ahead = irf.horizon, ortho = T, cumulative = F,
      boot = F, ci = irf.level / 100
   )
   irf.ts <- ts(
      data = mts.data.VAR.irf$irf[[1]][, 1],
      start = 0, frequency = frequency(mts.data)
   )
   irf.y.lim <- c(min(irf.ts), max(irf.ts))
   irf.y.factor <- 10 * ceiling(0.1 / (irf.y.lim[2] - irf.y.lim[1]))
   irf.y.lim <- c(
      floor(min(irf.ts) * irf.y.factor) / irf.y.factor,
      ceiling(max(irf.ts) * irf.y.factor) / irf.y.factor
   )
   plot(
      x = irf.ts, type = "b", xlab = "time (in years)",
      ylab = mts.units[1], ylim = irf.y.lim, col = "blue4"
   )
   if (sum(irf.y.lim > 0) > 0) {
      lines(ts(
         data = 0, start = start(irf.ts), end = end(irf.ts),
         frequency = frequency(irf.ts)
      ), type = "l", col = "red")
   }
   title(
      main = paste("impulse = ", mts.desc[i.r.pairs[[i.r.pair]][1]],
         "\nresponse = ", mts.desc[i.r.pairs[[i.r.pair]][2]],
         sep = ""
      ), font.main = 1, cex.main = 1
   )
}
mtext(
   text = paste("Exercise 9.2: Orthogonal impulse response functions"),
   side = 3, line = 0, outer = T, font = 2, cex = 1
)

dev.off()


# predict with VAR
mts.data.VAR.pred <- predict(
   object = mts.data.VAR.fit,
   n.ahead = pred.horizon, ci = pred.level / 100
)

# define forecast objects
ts.data.Var.pred.list <- list(rep(NA, 2))
for (k in 1:length(mts.var.names)) {
   ts.data.Var.pred.list[[k]] <- list(
      method = "VAR",
      model = mts.data.VAR.pred$model,
      level = pred.level,
      mean = ts(
         data = mts.data.VAR.pred$fcst[[k]][, "fcst"],
         start = end(mts.data) + c(0, 1), frequency = frequency(mts.data)
      ),
      lower = matrix(mts.data.VAR.pred$fcst[[k]][, "lower"]),
      upper = matrix(mts.data.VAR.pred$fcst[[k]][, "upper"]),
      x = mts.data[, k],
      xname = NULL,
      fitted = ts(
         data = fitted(mts.data.VAR.fit)[, k], start = start(mts.data),
         frequency = frequency(mts.data)
      ),
      residuals = ts(
         data = residuals(mts.data.VAR.fit)[, k],
         start = start(mts.data), frequency = frequency(mts.data)
      )
   )
   class(ts.data.Var.pred.list[[k]]) <- "forecast"
}

# plot predictions and prediction intervals
pdf("HW09/figures/exercise.W.09.02 - predictions and prediction intervals.pdf")

layout(matrix(c(1, 1, 2, 3, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
for (k in 1:length(mts.var.names)) {
   plot(
      x = ts.data.Var.pred.list[[k]], , xlab = "Time", ylab = mts.units[2],
      include = 12 * 10, shaded = T, shadecols = "pink1", main = "", col =
         "blue", fcol = "red", type = "b"
   )
   title(main = mts.desc[k], font.main = 1, cex.main = 1)
   qqnorm(y = residuals(ts.data.Var.pred.list[[k]]), main = "", col = "green4")
   qqline(y = residuals(ts.data.Var.pred.list[[k]]), col = "black")
   title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
}
mtext(text = paste(
   "Exercise 9.2: Predictions and prediction intervals (coverage probabilty: ",
   pred.level, "%)",
   sep = ""
), side = 3, line = 0, outer = T, font = 2, cex = 1)

dev.off()