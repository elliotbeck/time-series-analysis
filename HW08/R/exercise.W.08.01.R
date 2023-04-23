#
# R code for problem 1 of homework 8
#

# user input
acf.lag.max <- 4 * 12 # maximum lag for (p)ac.f.
ccf.coverage.prob <- 0.95 # covergage probability for cc.f.

# specifications for automatic SARIMA estimation
information.criterion <- "BIC" # chose among: AIC, AICc, and BIC
stepwise.model.selection <- T
SARIMA.specs <- list(
   d = NA, D = NA, max.p = 06, max.q = 06, max.P = 12, max.Q =
      06, max.order = 30, start.p = 0, start.q = 0, start.P = 00, start.Q = 00,
   stationary = F
)

# load libraries
library(forecast) # function: forecast
library(gdata) # function: trim

# load 'class.RDdata'
load("data/class.RData")

# definition of delimiters for output
delimiter.stars <- paste("\n", paste(rep("*", 80), sep = "", collapse = ""),
   "\n",
   sep = ""
)
delimiter.hyphen <- paste(paste(rep("-", 80), sep = "", collapse = ""), "\n",
   sep = ""
)

# define lables for graphs
acf.label <- expression(paste("ac.f.(", tau, ")", sep = ""))
pacf.label <- expression(paste("pac.f.(", tau, ")", sep = ""))
ccf.label <- expression(paste("cc.f.(", tau, ")", sep = ""))
lag.label <- expression(paste("lag ", tau, sep = ""))
lag.in.years.label <- expression(paste("lag ", tau, " ( in years)", sep = ""))


# plot time series and associated ac.f and pac.f
pdf("HW08/figures/exercise.W.08.01 - time plots.pdf")

layout(matrix(c(1, 1, 2, 2, 3, 4, 5, 6, 7, 7, 8, 8, 9, 10, 11, 12),
   nr = 4,
   byrow = T
))
par(oma = c(0, 2, 4, 2))
plot(
   x = us.data[, "infl"], type = "b", xlab = "time", ylab = "",
   main = "U.S.
   inflation", col = "green4", font.main = 1, cex.main = 1
)
plot(
   x = us.data[, "unemp"], type = "b", xlab = "time", ylab = "",
   main = "U.S.unemployment rate (in per cent)", col = "green4",
   font.main = 1, cex.main = 1
)
acf(
   x = us.data[, "infl"], lag.max = acf.lag.max, type = "correlation",
   xlab = lag.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = us.data[, "infl"], lag.max = acf.lag.max, type = "partial",
   xlab = lag.label, ylab = pacf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
acf(
   x = us.data[, "unemp"], lag.max = acf.lag.max, type = "correlation",
   xlab = lag.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = us.data[, "unemp"], lag.max = acf.lag.max, type = "partial",
   xlab = lag.label, ylab = pacf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
plot(
   x = diff(us.data[, "infl"], 1), type = "b", xlab = "time", ylab = "",
   main = "change in U.S. inflation", col = "green4", font.main = 1,
   cex.main = 1
)
plot(
   x = diff(us.data[, "unemp"], 1), type = "b", xlab = "time", ylab = "",
   main = "change in U.S. unemployment rate (in per cent)", col = "green4",
   font.main = 1, cex.main = 1
)
acf(
   x = diff(us.data[, "infl"], 1), lag.max = acf.lag.max, type = "correlation",
   xlab = lag.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = diff(us.data[, "infl"], 1), lag.max = acf.lag.max, type = "partial",
   xlab = lag.label, ylab = pacf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
acf(
   x = diff(us.data[, "unemp"], 1), lag.max = acf.lag.max,
   type = "correlation", xlab = lag.label, ylab = acf.label, main = ""
)
title(main = "sample ac.f.", font.main = 1, cex.main = 1)
acf(
   x = diff(us.data[, "unemp"], 1), lag.max = acf.lag.max, type = "partial",
   xlab = lag.label, ylab = pacf.label, main = ""
)
title(main = "sample pac.f.", font.main = 1, cex.main = 1)
mtext(
   text = "Exercise 8.1: U.S. data", side = 3, line = 0, outer = T, font = 2,
   cex = 1
)
dev.off()

# define vectors with names of and description
ts.names <- c("infl", "unemp")
ts.desc <- c("U.S. inflation", "U.S. unemployment rate (in per cent)")

# define lists for estimation results
ts.data.fit.list <- list(infl = NA, unemp = NA)
ts.data.fit.residuals.list <- ts.data.fit.list

for (series in 1:length(ts.names)) {

   # open connection to write output to file
   output.file <- paste(
      "HW08/figures/exercise.W.08.01 - ", ts.desc[series], " - model  selection", ".txt",
      sep = ""
   )
   sink(file = output.file, append = F, type = "output", split = F)

   # write output to file
   cat(delimiter.stars)
   cat(
      paste("automatic model selection (selection criterion: ",
         information.criterion, ")\n",
         sep = ""
      )
   )
   cat(delimiter.hyphen)

   # automatic model selection
   ts.data.fit.list[[series]] <- auto.arima(
      x = us.data[, ts.names[series]],
      d = SARIMA.specs$d, D = SARIMA.specs$D, max.p = SARIMA.specs$max.p,
      max.q = SARIMA.specs$max.q, max.P = SARIMA.specs$max.P,
      max.Q = SARIMA.specs$max.Q, max.order = SARIMA.specs$max.order,
      start.p = SARIMA.specs$start.p, start.q = SARIMA.specs$start.q,
      start.P = SARIMA.specs$start.P, start.Q = SARIMA.specs$start.Q,
      stationary = SARIMA.specs$stationary, ic = tolower(information.criterion),
      stepwise = stepwise.model.selection,
      trace = T, approximation = F
   )
   ts.data.fit.residuals.list[[series]] <- ts(
      data =
         ts.data.fit.list[[series]]$residuals, start = start(us.data[
         ,
         ts.names[series]
      ]), frequency = frequency(us.data[, ts.names[series]])
   )
   cat(delimiter.hyphen)
   print(ts.data.fit.list[[series]])
   cat(delimiter.stars)

   # close connection to write output to file and display file
   sink()
   file.show(output.file,
      title = paste("HW08/figures/exercise.W.08.01 - ", ts.desc[series],
         " - model selection", ".txt",
         sep = ""
      )
   )

   # diagnostic plots
   pdf(
      paste(
         "HW08/figures/exercise.W.08.01 - ", ts.desc[series],
         " - diagnostic plots.pdf"
      )
   )

   layout(matrix(c(1, 2, 3, 4), nr = 2, byrow = T))
   par(oma = c(0, 2, 12, 2))
   plot(
      x = ts.data.fit.residuals.list[[series]], type = "b", xlab = "time",
      ylab = "", main = "residuals", col = "green4", font.main = 1, cex.main = 1
   )
   acf(
      x = ts.data.fit.residuals.list[[series]], lag.max = acf.lag.max,
      type = "correlation", xlab = lag.in.years.label, ylab = acf.label,
      main = ""
   )
   title(main = "sample ac.f. of residuals", font.main = 1, cex.main = 1)
   Ljung.Box.p.value <- NULL
   for (k in 1:acf.lag.max) {
      Ljung.Box.p.value <- c(Ljung.Box.p.value, Box.test(
         x = ts.data.fit.residuals.list[[series]], lag = k, type = "Ljung-Box"
      )$p.value)
   }
   plot(
      x = (1:acf.lag.max) / 12, y = Ljung.Box.p.value, ylim = c(0, 1), xlab =
         lag.in.years.label, ylab = "p-value",
      main = "p-values for Ljung-Box statistics", col = "green4",
      font.main = 1, cex.main = 1
   )
   lines(x = c(-5, acf.lag.max + 5), y = 0.05 * c(1, 1), col = "red", lty = 2)
   acf(
      x = ts.data.fit.residuals.list[[series]], lag.max = acf.lag.max, type =
         "partial", xlab = lag.in.years.label, ylab = pacf.label, main = ""
   )
   title(main = "sample pac.f. of residuals", font.main = 1, cex.main = 1)
   mtext(text = paste("Exercise 8.1: ", ts.desc[series], " - diagnostic plots",
      sep = ""
   ), side = 3, line = 6, outer = T, font = 2, cex = 1)
   mtext(paste("model selection criterion: ", information.criterion,
      "best model: ", trim(capture.output(ts.data.fit.list[[series]])[2]),
      sep = ""
   ),
   side = 3, line = 2, outer = T, at = 0.5
   )

   dev.off()
}

# plot cross-correlation function
pdf(paste("HW08/figures/exercise.W.08.01 - cross-correlation function.pdf"))

par(mfrow = c(1, 1))
sccf <- ccf(
   x = ts.data.fit.residuals.list$unemp,
   y = ts.data.fit.residuals.list$infl,
   ci = ccf.coverage.prob, lag.max = acf.lag.max,
   type = "correlation", xlab = lag.in.years.label, ylab = ccf.label, main = ""
)
mtext(
   text = paste(
      "Exercise 8.1: Cross-correlation function between the SARIMA residuals of ",
      ts.desc[1], " (X) and ", ts.desc[2], " (Y)",
      sep = ""
   ), side =
      3, line = 0, outer = T, font = 2, cex = 1
)

dev.off()