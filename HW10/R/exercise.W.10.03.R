#
# R code for problem 3 of homework 10
#

# user input
acf.lag.max <- 4 * 4 # specifiation for ac.f.


# load 'class.RDdata'
load("data/class.RData")


# time series to analyse
mts.var.names <- c("disp", "cons")
mts.data <- german.data[, mts.var.names]
impulse.dummy <- diff(as.matrix(german.data[, "step"]))
colnames(impulse.dummy) <- "impulse.dummy"


# description of time series
mts.desc <- c(
   "German disposable income",
   "German consumption",
   "impulse-dummy variable"
)
mts.units <- c("DM", "DM", "DM")


# specification for VAR estimation
VAR.ic <- "SC" # chose among: AIC, HQ, SC, and FPE

# specification for prediction
irf.horizon <- 10 * 4
irf.level <- 95

# specification for prediction
pred.horizon <- 4 * 4
pred.level <- 95


# load libraries
library(forecast)
library(gdata)
library(vars)

# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80),
   sep = "",
   collapse = ""
), "\n", sep = "")
delimiter.hyphen <- paste(paste(rep("-", 80),
   sep = "",
   collapse = ""
), "\n", sep = "")


# plot time series
pdf("HW10/figures/exercise.W.10.03 - German macro time series.pdf")

layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = T))
par(oma = c(0, 2, 4, 2))
plot(
   x = mts.data[, mts.var.names[1]], type = "b", xlab = "time",
   ylab = mts.units[1], col = "green4"
)
title(main = mts.desc[1], font.main = 1, cex.main = 1)
plot(
   x = diff(mts.data[, mts.var.names[1]], 1), type = "b", xlab = "time",
   ylab = mts.units[1], col = "green4"
)
title(main = paste("change in", mts.desc[1]), font.main = 1, cex.main = 1)
plot(
   x = mts.data[, mts.var.names[2]], type = "b", xlab = "time",
   ylab = mts.units[2], col = "green4"
)
title(main = mts.desc[2], font.main = 1, cex.main = 1)
plot(
   x = diff(mts.data[, mts.var.names[2]], 1), type = "b", xlab = "time",
   ylab = mts.units[2], col = "green4"
)
title(main = paste("change in", mts.desc[2]), font.main = 1, cex.main = 1)
mtext(
   text = paste("Exercise 10.3: German macro time series"), side = 3,
   line = 0, outer = T, font = 2, cex = 1
)

dev.off()


mts.data.VAR.ic.list <- list(rep(NA, 2))
mts.data.VAR.fit.list <- list(rep(NA, 2))
exogen.var.list <- list(NULL, impulse.dummy)
exogen.desc.list <- list(
   "without dummy variable", "with impulse-dummy variable"
)

for (r in 1:2) {
   # search for best VAR model
   mts.data.VAR.ic.list[[r]] <- VARselect(
      y = diff(mts.data), lag.max = 4 * 4,
      season = NULL, exogen = exogen.var.list[[r]]
   )
   mts.data.VAR.fit.list[[r]] <- VAR(
      y = diff(mts.data), season = NULL,
      exogen = exogen.var.list[[r]], lag.max = 4 * 4, ic = VAR.ic
   )

   # open connection to write output to file
   output.file <- paste(
      "HW10/figures/exercise.W.10.03 - VAR model - ",
      exogen.desc.list[[r]], " - selection and estimation", ".txt",
      sep = ""
   )
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file

   # estimated model
   cat(delimiter.stars)
   cat("comparison of information criteria:\n\n")
   print(mts.data.VAR.ic.list[[r]])

   # estimated model
   cat(delimiter.stars)
   cat("automatic model selection (selection criterion:", VAR.ic, ")\n")
   print(summary(mts.data.VAR.fit.list[[r]]))

   # Portmanteau test
   cat(delimiter.stars)
   cat("Portmanteau test:\n")
   print(serial.test(
      x = mts.data.VAR.fit.list[[r]], lags.pt = 24,
      type = "PT.asymptotic"
   ))

   # Ljung-Box-Pierce test
   cat(delimiter.stars)
   cat("Portmanteau test:\n")
   print(serial.test(
      x = mts.data.VAR.fit.list[[r]], lags.pt = 6 * 4,
      type = "PT.adjusted"
   ))

   # close connection to write output to file and display file
   sink()
   file.show(
      output.file,
      title = paste("figures/HW10/exercise.W.10.03 - VAR model - ",
         exogen.desc.list[[r]], " - selection and estimation", ".txt",
         sep = ""
      )
   )

   # diagnostic plots
   for (s in 1:2) {
      pdf(paste(
         "HW10/figures/exercise.W.10.03 - VAR model - ",
         exogen.desc.list[[r]], " - diagnostic plots - equation for ",
         mts.desc[s], ".pdf",
         sep = ""
      ))

      res <- ts(
         data = residuals(mts.data.VAR.fit.list[[r]])[, s],
         start = start(mts.data[, s]),
         frequency = frequency(mts.data[, s])
      )
      layout(matrix(c(1, 1, 1, 2, 3, 4), nr = 2, byrow = T))
      par(oma = c(0, 2, 12, 2))
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
            Box.test(x = res, lag = k, type = "Ljung-Box")$p.value
         )
      }
      plot(
         x = (1:acf.lag.max) / frequency(mts.data[, s]),
         y = Ljung.Box.p.value, ylim = c(0, 1), xlab = "lag (in years)",
         ylab = "p-value", main = "p-values for Ljung-Box statistics",
         col = "green4", font.main = 1, cex.main = 1
      )
      pacf(
         x = res, lag.max = acf.lag.max,
         xlab = "lag (in years)", ylab = "ac.f.", main = ""
      )
      title(main = "sample pac.f.", font.main = 1, cex.main = 1)
      mtext(text = paste(
         "Exercise 10.3: VAR model - ",
         exogen.desc.list[[r]],
         " - diagnostic plots\n(equation for ",
         mts.desc[s], ")",
         sep = ""
      ), side = 3, line = 6, outer = T, font = 2, cex = 1)
      mtext(paste("model selection criterion: ", VAR.ic,
         "\nbest model: VAR(", mts.data.VAR.fit.list[[r]]$p, ")",
         sep = ""
      ), side = 3, line = 1, outer = T, at = 0.5)

      dev.off()
   }
}