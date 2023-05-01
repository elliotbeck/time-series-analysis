#
# R code for problem 1 (b) of homework 9
#

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
stepwise.model.selection <- T
SARIMA.specs <- list(
   d = NA, D = NA, max.p = 12, max.q = 12, max.P = 12, max.Q = 12,
   max.order = 48, start.p = 00, start.q = 00, start.P = 00, start.Q = 00,
   stationary = F
)

# specification for VAR estimation
VAR.ic <- "SC" # chose among: AIC, HQ, SC, and FPE

# specification for prediction
pred.horizon <- 1
pred.level <- 95


# load libraries
library(forecast) # function: auto.arima
library(gdata) # function: trim
library(vars) # functions: VARselect, VAR


# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80), sep = "", collapse = ""),
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


# plot time series and associated ac.f.s and pac.f.s
pdf(paste("HW09/figures/exercise.W.09.01.b - ", mts.desc[2], ".pdf"))

layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
   x = ts.data, type = "b", xlab = "time", ylab = mts.units[2],
   main = "raw series", col = "green4", font.main = 1, cex.main = 1
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
   text = paste("Exercise 9.1 (b): ", mts.desc[2], sep = ""),
   side = 3, line = 0, outer = T, font = 2, cex = 1
)

dev.off()

ts.data.subperiod.SARIMA.pred.error <- rep(NA, 12)
ts.data.subperiod.SARIMA.fit.list <- list(rep(NA, 12))
ts.data.subperiod.SARIMA.pred.list <- list(rep(NA, 12))
ts.data.subperiod.VAR.pred.error <- rep(NA, 12)
ts.data.subperiod.VAR.fit.list <- list(rep(NA, 12))
ts.data.subperiod.VAR.pred.list <- list(rep(NA, 12))
mts.data.subperiod.VAR.ic <- list(rep(NA, 12))
mts.data.subperiod.VAR.fit.list <- list(rep(NA, 12))
mts.data.subperiod.VAR.pred.list <- list(rep(NA, 12))


for (w in 12:1) {
   # extract subperiod of time series
   ts.data.subperiod <- window(
      x = ts.data, start = start(ts.data),
      end = time(ts.data)[length(ts.data) - w]
   )
   mts.data.subperiod <- window(
      x = mts.data, start = start(ts.data),
      end = time(ts.data)[length(ts.data) - w]
   )

   # search for best SARIMA model

   # open connection to write output to file
   output.file <- paste(
      "HW09/figures/exercise.W.09.01.b - SARIMA model selection - ",
      mts.desc[2], " - subperiod ", format(13 - w, width = 2), ".txt",
      sep = ""
   )
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file
   cat(delimiter.stars)
   cat("automatic model selection (selection criterion:", SARIMA.ic, ")\n")
   cat(delimiter.hyphen)

   # automatic model selection
   ts.data.subperiod.SARIMA.fit.list[[w]] <- auto.arima(
      x = ts.data.subperiod, d = SARIMA.specs$d, D = SARIMA.specs$D,
      max.p = SARIMA.specs$max.p, max.q = SARIMA.specs$max.q,
      max.P = SARIMA.specs$max.P, max.Q = SARIMA.specs$max.Q,
      max.order = SARIMA.specs$max.order, start.p = SARIMA.specs$start.p,
      start.q = SARIMA.specs$start.q, start.P = SARIMA.specs$start.P,
      start.Q = SARIMA.specs$start.Q, stationary = SARIMA.specs$stationary,
      ic = tolower(SARIMA.ic), stepwise = stepwise.model.selection, trace = T,
      approximation = F
   )
   cat(delimiter.hyphen)
   print(ts.data.subperiod.SARIMA.fit.list[[w]])
   cat(delimiter.stars)

   # close connection to write output to file and display file
   sink()
   file.show(output.file,
      title = paste("HW09/figures/exercise.W.09.01.b - SARIMA model selection - ",
         mts.desc[2], " - subperiod ", format(13 - w, width = 2), ".txt",
         sep = ""
      )
   )

   # search for best VAR model
   mts.data.subperiod.VAR.ic[[w]] <- VARselect(
      y = mts.data.subperiod,
      lag.max = 24, type = "both", season = NULL
   )
   mts.data.subperiod.VAR.fit.list[[w]] <- VAR(
      y = mts.data.subperiod,
      type = "both", season = NULL, lag.max = 24, ic = VAR.ic
   )

   # open connection to write output to file
   output.file <- paste(
      "HW09/figures/exercise.W.09.01.b - VAR model - selection and estimation - subperiod ",
      format(13 - w, width = 2), ".txt",
      sep = ""
   )
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file

   # estimated model
   cat(delimiter.stars)
   cat("comparison of information criteria:\n\n")
   print(mts.data.subperiod.VAR.ic[[w]])

   # estimated model
   cat(delimiter.stars)
   cat("automatic model selection (selection criterion:", VAR.ic, ")\n")
   print(summary(mts.data.subperiod.VAR.fit.list[[w]]))

   # Portmanteau test
   cat(delimiter.stars)
   cat("Portmanteau test:\n")
   print(serial.test(
      x = mts.data.subperiod.VAR.fit.list[[w]],
      lags.pt = 24, type = "PT.asymptotic"
   ))

   # Ljung-Box-Pierce test
   cat(delimiter.stars)
   cat("Ljung-Box-Pierce test:\n")
   print(serial.test(
      x = mts.data.subperiod.VAR.fit.list[[w]],
      lags.pt = 24, type = "PT.adjusted"
   ))

   # close connection to write output to file and display file
   sink()
   file.show(output.file,
      title = paste(
         "HW09/figures/exercise.W.09.01.b - VAR model - selection and estimation - subperiod ",
         format(13 - w, width = 2), ".txt",
         sep = ""
      )
   )


   # predict with SARIMA
   ts.data.subperiod.SARIMA.pred.list[[w]] <- forecast(
      object = ts.data.subperiod.SARIMA.fit.list[[w]],
      h = pred.horizon, level = pred.level
   )

   # compute prediction error
   ts.data.subperiod.SARIMA.pred.error[w] <- ts.data.subperiod[length(ts.data.subperiod)] -
      ts.data.subperiod.SARIMA.pred.list[[w]]$mean

   # predict with VAR
   mts.data.subperiod.VAR.pred.list[[w]] <- predict(
      object = mts.data.subperiod.VAR.fit.list[[w]],
      n.ahead = pred.horizon, ci = pred.level / 100
   )
   # define forecast object
   ts.data.subperiod.VAR.pred.list[[w]] <- list(
      method = "VAR",
      model = mts.data.subperiod.VAR.pred.list[[w]]$model,
      level = pred.level,
      mean = ts(
         data = mts.data.subperiod.VAR.pred.list[[w]]$fcst$unemp[, "fcst"],
         start = start(ts.data.subperiod.SARIMA.pred.list[[w]]$mean),
         frequency = frequency(ts.data.subperiod.SARIMA.pred.list[[w]]$mean)
      ),
      lower = matrix(mts.data.subperiod.VAR.pred.list[[w]]$fcst$unemp[, "lower"]),
      upper = matrix(mts.data.subperiod.VAR.pred.list[[w]]$fcst$unemp[, "upper"]),
      x = us.data[, "unemp"],
      xname = NULL,
      fitted = ts(
         data = fitted(mts.data.subperiod.VAR.fit.list[[w]])[, "unemp"],
         start = start(ts.data.subperiod.SARIMA.pred.list[[w]]$fitted),
         frequency = frequency(ts.data.subperiod.SARIMA.pred.list[[w]]$fitted)
      ),
      residuals = ts(
         data = residuals(mts.data.subperiod.VAR.fit.list[[w]])[, "unemp"],
         start = start(ts.data.subperiod.SARIMA.pred.list[[w]]$residuals),
         frequency = frequency(ts.data.subperiod.SARIMA.pred.list[[w]]$residuals)
      )
   )
   class(ts.data.subperiod.VAR.pred.list[[w]]) <- "forecast"

   # compute prediction error
   ts.data.subperiod.VAR.pred.error[w] <- ts.data.subperiod[length(ts.data.subperiod)] -
      ts.data.subperiod.VAR.pred.list[[w]]$mean
}

# summary of comparison of prediction errors
summary.comparison.pred.error <- data.frame(
   sum.of.squares =
      rbind(
         sum(ts.data.subperiod.SARIMA.pred.error^2),
         sum(ts.data.subperiod.VAR.pred.error^2)
      ),
   sum.of.abs.values =
      rbind(
         sum(abs(ts.data.subperiod.SARIMA.pred.error)),
         sum(abs(ts.data.subperiod.VAR.pred.error))
      )
)
rownames(summary.comparison.pred.error) <- list("SARIMA", "VAR")

# open connection to write output to file
output.file <- paste("HW09/figures/exercise.W.09.01.b - ",
   mts.desc[2], " - comparison of prediction errors", ".txt",
   sep = ""
)
sink(file = output.file, append = F, type = "output", split = F)

# write output to file
cat(delimiter.stars)
cat("summary of comparison of prediction errors:\n\n")
print(summary.comparison.pred.error)
cat(delimiter.stars)

# close connection to write output to file and display file
sink()
file.show(output.file,
   title = paste("HW09/figures/exercise.W.09.01.b - ",
      mts.desc[2], " - comparison of prediction errors", ".txt",
      sep = ""
   )
)