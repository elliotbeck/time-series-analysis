#
# R code for problem 1 of homework 10
#

# user input
acf.lag.max <- 4 * 4 # specifiation for ac.f.

# load 'class.RDdata'
load("data/class.RData")


# description of time series
mts.desc <- c(
   "German disposable income", "German consumption",
   "impulse-dummy variable", "step-dummy variable"
)
mts.units <- c("DM", "DM", "DM", "DM")


# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80), sep = "", collapse = ""),
   "\n",
   sep = ""
)
delimiter.hyphen <- paste(paste(rep("-", 80), sep = "", collapse = ""),
   "\n",
   sep = ""
)


# define time series variables
disp.income <- german.data[, "disp"]
impulse.dummy <- german.data[, "imp"]
step.dummy <- german.data[, "step"]
time.trend <- ts(
   data = 1:length(disp.income), start = start(disp.income),
   end = end(disp.income), frequency = frequency(disp.income)
)


# plot time series
pdf("HW10/figures/exercise.W.10.01 - German time series.pdf")

layout(matrix(c(1, 2, 3), nr = 3, byrow = TRUE))
par(oma = c(0, 2, 4, 2))
plot(
   x = disp.income, type = "b", xlab = "time", ylab = mts.units[1],
   col = "green4"
)
title(main = mts.desc[1], font.main = 1, cex.main = 1)
plot(
   x = impulse.dummy, type = "b", xlab = "time", ylab = mts.units[3],
   col = "green4"
)
title(main = mts.desc[3], font.main = 1, cex.main = 1)
plot(
   x = step.dummy, , type = "b", xlab = "time", ylab = mts.units[4],
   col = "green4"
)
title(main = mts.desc[4], font.main = 1, cex.main = 1)
mtext(
   text = paste("Exercise 10.1: Time series"), side = 3, line = 0,
   outer = TRUE, font = 2, cex = 1
)

dev.off()

# estimate ARIMA model without dummy variable
arima.nod.fit <- arima(
   x = disp.income, order = c(1, 1, 0),
   seasonal = list(order = c(0, 0, 0), period = NA), xreg = time.trend
)

# estimate ARIMA model with impact dummy variable
arima.imp.fit <- arima(
   x = disp.income, order = c(1, 1, 0),
   seasonal = list(order = c(0, 0, 0), period = NA),
   xreg = cbind(time.trend, impulse.dummy)
)

# estimate ARIMA model with step dummy variable
arima.step.fit <- arima(
   x = disp.income, order = c(1, 1, 0),
   seasonal = list(order = c(0, 0, 0), period = NA),
   xreg = cbind(time.trend, step.dummy)
)


# open connection to write output to file
output.file <- "HW10/figures/exercise.W.10.01 - ARIMA - estimation.txt"
sink(file = output.file, append = FALSE, type = "output", split = FALSE)

# write output to file

# ARIMA model without dummy variable
cat(delimiter.stars)
cat("ARIMA model without dummy variable:\n\n")
print(arima.nod.fit)

# Box-Pierce test
cat(delimiter.hyphen)
cat("Box-Pierce test:\n")
print(Box.test(
   x = residuals(arima.nod.fit), lag = acf.lag.max,
   type = "Box-Pierce"
))

# Ljung-Box test
cat(delimiter.hyphen)
cat("Ljung-Box test:\n")
print(Box.test(
   x = residuals(arima.nod.fit),
   lag = acf.lag.max, type = "Ljung-Box"
))


# ARIMA model with impulse dummy variable
cat(delimiter.stars)
cat("ARIMA model with impulse dummy variable:\n\n")
print(arima.imp.fit)

# Box-Pierce test
cat(delimiter.hyphen)
cat("Box-Pierce test:\n")
print(Box.test(
   x = residuals(arima.imp.fit),
   lag = acf.lag.max, type = "Box-Pierce"
))

# Ljung-Box test
cat(delimiter.hyphen)
cat("Ljung-Box test:\n")
print(Box.test(
   x = residuals(arima.imp.fit),
   lag = acf.lag.max, type = "Ljung-Box"
))


# ARIMA model with step dummy variable
cat(delimiter.stars)
cat("ARIMA model with step dummy variable:\n\n")
print(arima.step.fit)

# Box-Pierce test
cat(delimiter.hyphen)
cat("Box-Pierce test:\n")
print(Box.test(
   x = residuals(arima.step.fit),
   lag = acf.lag.max, type = "Box-Pierce"
))

# Ljung-Box test
cat(delimiter.hyphen)
cat("Ljung-Box test:\n")
print(Box.test(
   x = residuals(arima.step.fit),
   lag = acf.lag.max, type = "Ljung-Box"
))

# close connection to write output to file and display file
sink()
file.show(
   output.file,
   title = "HW10/figures/exercise.W.10.01 - ARIMA - estimation.txt"
)


# diagnostic plots
arima.fit.list <- list(arima.nod.fit, arima.imp.fit, arima.step.fit)
arima.fit.desc.list <- list("without", "with impulse", "with step")

for (s in 1:length(arima.fit.list)) {
   pdf(paste0(
      "HW10/figures/exercise.W.10.01 - ARIMA model ",
      arima.fit.desc.list[[s]], " dummy variable - diagnostic plots.pdf"
   ))
   res <- ts(
      data = residuals(arima.fit.list[[s]]),
      start = start(german.data[, 1]),
      frequency = frequency(german.data[, 1])
   )
   layout(matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = TRUE))
   par(oma = c(0, 2, 4, 2))
   plot(
      x = res, type = "b", xlab = "time", ylab = mts.units[s],
      main = "residuals", col = "green4", font.main = 1, cex.main = 1
   )
   acf(
      x = res, lag.max = acf.lag.max, type = "correlation",
      xlab = "lag (in years)", ylab = "ac.f.", main = ""
   )
   title(main = "sample ac.f.", font.main = 1, cex.main = 1)
   Ljung.Box.p.value <- NULL
   for (k in 1:acf.lag.max) {
      Ljung.Box.p.value <- c(
         Ljung.Box.p.value,
         Box.test(
            x = res,
            lag = k, type = "Ljung-Box"
         )$p.value
      )
   }
   plot(
      x = (1:acf.lag.max) / frequency(german.data[, 1]),
      y = Ljung.Box.p.value, ylim = c(0, 1), xlab = "lag (in years)",
      ylab = "p-value", main = "p-values for Ljung-Box statistics",
      col = "green4", font.main = 1, cex.main = 1
   )
   lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red")
   pacf(
      x = res, lag.max = acf.lag.max, xlab = "lag (in years)",
      ylab = "ac.f.", main = ""
   )
   title(main = "sample pac.f.", font.main = 1, cex.main = 1)
   mtext(text = paste0(
      "Exercise 10.1: ARIMA model ", arima.fit.desc.list[[s]],
      " dummy variable - diagnostic plots"
   ), side = 3, line = 0, outer = TRUE, font = 2, cex = 1)

   # save time plot
   dev.off()
}