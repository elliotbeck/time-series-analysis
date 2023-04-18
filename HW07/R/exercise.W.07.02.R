#
# R code for problem 2 of homework 7
#

# user in put
acf.lag.max <- 36 # max lag for ac.f./pac.f.
arima.specs.list <- list(
   list(
      arima.order = c(0, 0, 2),
      seasonal = list(order = c(1, 1, 1), period = 12)
   ), # SARIMA model for temperature
   list(
      arima.order = c(0, 0, 1),
      seasonal = list(order = c(1, 0, 1), period = 12)
   )
) # SARIMA model for precipitation
# specifications for forecasting
forecast.horizon <- 12 # specifications for forecasting: horizon
forecast.level <- 95 # specifications for forecasting: confidence level for prediction intervals

# load 'class.RDdata'
load("data/class.RData")


# define time series to analyze

# time series
ts.data.list <- list(zurich.temp, zurich.precip)

# description of time series
ts.desc.list <- list("Zurich temperature", "Zurich precipitation")


# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80),
   sep = "",
   collapse = ""
), "\n", sep = "")


# load libraries
library(forecast) # function: auto.arima
library(gdata) # function: trim



for (s in 1:length(ts.data.list)) {
   # define time series
   ts.data <- ts.data.list[[s]]
   ts.data.1stdiff <- diff(ts.data, 1)


   # plot time series and associated ac.f and pac.f

   pdf(file = paste("HW07/figures/exercise.W.07.02 - ", ts.desc.list[[s]],
      ".", "pdf",
      sep = ""
   ))

   layout(matrix(c(1, 1, 2, 3, 4, 4, 5, 6), nr = 2, byrow = T))
   par(oma = c(0, 2, 4, 2))
   plot(
      x = ts.data, type = "b", xlab = "time", ylab = "",
      main = "raw series", col = "green4", font.main = 1, cex.main = 1
   )
   acf(
      x = ts.data, lag.max = acf.lag.max, type = "correlation",
      xlab = expression(paste("lag ", tau, sep = "")),
      ylab = expression(paste("ac.f.(", tau, ")", sep = "")), main = ""
   )
   title(main = "sample ac.f.", font.main = 1, cex.main = 1)
   acf(
      x = ts.data, lag.max = acf.lag.max, type = "partial", xlab =
         expression(paste("lag ", tau, sep = "")), ylab =
         expression(paste("pc.f.(",
            tau, ")",
            sep = ""
         )), main = ""
   )
   title(main = "sample pac.f.", font.main = 1, cex.main = 1)
   plot(x = ts.data.1stdiff, type = "b", xlab = "time", ylab = "", main = "first
   differences of raw series", col = "green4", font.main = 1, cex.main = 1)
   acf(
      x = ts.data.1stdiff, lag.max = acf.lag.max, type = "correlation", xlab =
         expression(paste("lag ", tau, sep = "")), ylab =
         expression(paste("ac.f.(",
            tau, ")",
            sep = ""
         )), main = ""
   )
   title(main = "sample ac.f.", font.main = 1, cex.main = 1)
   acf(
      x = ts.data.1stdiff, lag.max = acf.lag.max, type = "partial", xlab =
         expression(paste("lag ", tau, sep = "")), ylab =
         expression(paste("pc.f.(",
            tau, ")",
            sep = ""
         )), main = ""
   )
   title(main = "sample pac.f.", font.main = 1, cex.main = 1)
   mtext(
      text = paste("Exercise 7.2: ", ts.desc.list[[s]], sep = ""), side = 3,
      line = 0, outer = T, font = 2, cex = 1
   )

   dev.off()

   # estimate SARIMA model
   ts.data.fit <- arima(
      x = ts.data, order = arima.specs.list[[s]]$arima.order,
      seasonal = arima.specs.list[[s]]$seasonal, include.mean = T, method =
         "CSS-ML"
   )


   # open connection to write output to file
   output.file <- paste("HW07/figures/exercise.W.07.02 - ", ts.desc.list[[s]], "
   - model estimation", ".txt", sep = "")
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file
   print(ts.data.fit)

   # close connection to write output to file and display file
   sink()
   file.show(output.file, title = paste("HW07/figures/exercise.W.07.02 - ",
      ts.desc.list[[s]], " - model estimation", ".txt",
      sep = ""
   ))


   # diagnostic plots
   layout(matrix(c(1, 1, 2, 2, 3, 3, 4, 4), nr = 2, byrow = T))
   par(oma = c(0, 2, 12, 2))
   plot(
      x = ts.data.fit$residuals, type = "b", xlab = "time", ylab = "", main =
         "residuals", col = "green4", font.main = 1, cex.main = 1
   )
   acf(
      x = ts.data.fit$residuals, lag.max = acf.lag.max, type = "correlation",
      xlab = expression(paste("lag ", tau, sep = "")), ylab =
         expression(paste("ac.f.(", tau, ")", sep = "")), main = ""
   )
   title(main = "sample ac.f. of residuals", font.main = 1, cex.main = 1)
   Ljung.Box.p.value <- NULL
   for (k in 1:acf.lag.max) {
      Ljung.Box.p.value <- c(Ljung.Box.p.value, Box.test(
         x =
            ts.data.fit$residuals, lag = k, type = "Ljung-Box"
      )$p.value)
   }
   plot(
      x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1), xlab =
         expression(paste("lag ", tau, sep = "")), ylab = "p-value", main =
         "p-values
   for Ljung-Box statistics", col = "green4", font.main = 1, cex.main = 1
   )
   lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
   acf(
      x = ts.data.fit$residuals, lag.max = acf.lag.max, type = "partial",
      xlab = expression(paste("lag ", tau, sep = "")), ylab =
         expression(paste("pac.f.(", tau, ")", sep = "")), main = ""
   )
   title(main = "sample pac.f. of residuals", font.main = 1, cex.main = 1)
   mtext(text = paste("Exercise 7.2: ", ts.desc.list[[s]], " - diagnostic
   plots", sep = ""), side = 3, line = 6, outer = T, font = 2, cex = 1)
   mtext(paste("model: ", trim(capture.output(ts.data.fit)[2]), sep = ""),
      side = 3, line = 3, outer = T, at = 0.5
   )


   # forecast with exponential smoothing
   ts.data.ets <- ets(y = ts.data)
   ts.data.ets.pred <- forecast(
      object = ts.data.ets, h = forecast.horizon,
      level = forecast.level
   )
   pred.dates <- seq(from = as.Date(paste(end(time(ts.data))[1], "/",
      end(time(ts.data))[2], "/01",
      sep = ""
   )), by = "months", length.out = 1 +
      forecast.horizon)
   ts.data.ets.pred.table <- data.frame(
      date = paste(months(pred.dates[-1]), format(pred.dates[-1], "%Y")),
      point.forecast = ts.data.ets.pred$mean,
      PI.lower.bound = ts.data.ets.pred$lower,
      PI.upper.bound = ts.data.ets.pred$upper,
      PI.length = ts.data.ets.pred$upper - ts.data.ets.pred$lower
   )


   # forecast with SARIMA
   ts.data.sarima.pred <- forecast(
      object = ts.data.fit, h = forecast.horizon,
      level = forecast.level
   )
   ts.data.sarima.pred.table <- data.frame(
      date = paste(months(pred.dates[-1]), format(pred.dates[-1], "%Y")),
      point.forecast = as.numeric(ts.data.sarima.pred$mean),
      PI.lower.bound = as.numeric(ts.data.sarima.pred$lower),
      PI.upper.bound = as.numeric(ts.data.sarima.pred$upper),
      PI.length = as.numeric(ts.data.sarima.pred$upper -
         ts.data.sarima.pred$lower)
   )

   # open connection to write output to file
   output.file <- paste("HW07/figures/exercise.W.07.02 - ", ts.desc.list[[s]], "
   - comparison of forecasts.txt", sep = "")
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file
   cat("Forecast method: SARIMA\n\n")
   print(ts.data.ets.pred.table, row.names = F)
   cat(delimiter.stars)
   cat("\nForecast method: exponential smoothing\n\n")
   print(ts.data.sarima.pred.table, row.names = F)

   # close connection to write output to file and display file
   sink()
   file.show(output.file, title = paste("exercise.W.07.02 - ",
      ts.desc.list[[s]], " - comparison of forecasts.txt",
      sep = ""
   ))


   # plot point forecasts and forecast intervals
   pdf(paste("HW07/figures/exercise.W.07.02 - ", ts.desc.list[[s]], " -
   forecasts", ".", "pdf", sep = ""))

   layout(matrix(c(1, 1, 2, 3, 3, 4), nr = 2, byrow = T))
   par(oma = c(0, 2, 4, 2))
   plot(
      x = ts.data.ets.pred, include = 12 * 10, shaded = T, shadecols =
         "pink1", main = "", col = "blue", fcol = "red", type = "b"
   )
   title(main = "Forecast method: SARIMA", font.main = 1, cex.main = 1)
   qqnorm(y = ts.data.ets.pred$residuals, main = "", col = "green4")
   qqline(y = ts.data.ets.pred$residuals, col = "black")
   title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
   plot(
      x = ts.data.sarima.pred, include = 12 * 10, shaded = T, shadecols =
         "pink1", main = "", col = "blue", fcol = "red", type = "b"
   )
   title(
      main = "Forecast method: exponential smoothing", font.main = 1,
      cex.main = 1
   )
   qqnorm(y = ts.data.sarima.pred$residuals, main = "", col = "green4")
   qqline(y = ts.data.sarima.pred$residuals, col = "black")
   title(main = "Normal Q-Q plot for residuals", font.main = 1, cex.main = 1)
   mtext(text = paste("Exercise 7.2: ", ts.desc.list[[s]], " - forecasts",
      sep =
         ""
   ), side = 3, line = 0, outer = T, font = 2, cex = 1)

   dev.off()
}