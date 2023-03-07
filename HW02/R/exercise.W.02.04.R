#
# R code for exercise 4 of homework 2
#

# clear memory and close all graphics
rm(list = ls())

# user in put
series.to.analyse <- "temperature" # choose among: temperature, precipitation

# load 'class.RDdata'
load("data/class.RData")


# choose series to be analyzed
if (series.to.analyse == "precipitation") {
   raw.data <- zurich.precip
   ylabel <- "mm"
   title.text <- "Monthly precipitation for the city of Zurich"
   line.color <- "blue"
} else {
   raw.data <- zurich.temp
   ylabel <- "degree Celsius"
   line.color <- "green4"
   title.text <- "Monthly average temperature for the city of Zurich"
}

# define variables
sf <- 1
raw.data.ex.season <- deseason(raw.data)
raw.data.ex.trend.KS <- detrend(raw.data, ma = F, sf)
raw.data.ex.trend.KS.ex.season <-
   deseason(raw.data.ex.trend.KS$detrended.series)


# define axis labels
acf.x.lab <- expression(paste("lag ", tau, sep = ""))
acf.y.lab <- expression(paste("acf(", tau, ")", sep = ""))

#### (a) ####

# time plot and correlogram of deseasonalized data
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))
plot(
   x = raw.data, type = "b", xlab = "Time", ylab = ylabel,
   main = "Raw series", col = line.color
)
plot(
   x = raw.data.ex.season$deseasoned.series, type = "b",
   xlab = "Time", ylab = ylabel, main = "Deseasonalized series",
   col = line.color
)
acf(x = raw.data, xlab = acf.x.lab, ylab = acf.y.lab, main = "")
acf(
   x = raw.data.ex.season$deseasoned.series, xlab = acf.x.lab,
   ylab = acf.y.lab, main = ""
)
mtext(text = title.text, side = 3, line = 0, outer = T)

# plot of seasonal effects
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(raw.data.ex.season$season.effects,
   type = "b", col = line.color,
   xlab = "Month", ylab = "Effect", main = "Seasonal effects"
)

#### (b) ####

# time plots and correlograms of deseasonalized KS-detrended data
par(mfrow = c(2, 3))
par(oma = c(0, 2, 4, 2))
plot(
   x = raw.data, type = "b", xlab = "time", ylab = ylabel,
   main = "raw series", col = line.color
)
lines(
   x = as.numeric(time(raw.data.ex.trend.KS$trend)),
   y = as.numeric(raw.data.ex.trend.KS$trend), col = "red"
)
plot(
   x = raw.data.ex.trend.KS$detrended.series, type = "b",
   xlab = "Time", ylab = ylabel, main = "KS-detrended series", col = line.color
)
plot(
   x = raw.data.ex.trend.KS.ex.season$deseasoned.series,
   type = "b", xlab = "time", ylab = ylabel,
   main = "deseasonalized KS-detrended series", col = line.color
)
acf(x = raw.data, xlab = acf.x.lab, ylab = acf.y.lab, main = "")
acf(
   x = raw.data.ex.trend.KS$detrended.series,
   xlab = acf.x.lab, ylab = acf.y.lab, main = ""
)
acf(
   x = raw.data.ex.trend.KS.ex.season$deseasoned.series,
   xlab = acf.x.lab, ylab = acf.y.lab, main = ""
)
mtext(
   text = paste(title.text, " (KS-detrended)", sep = ""),
   side = 3, line = 0, outer = T
)


#### (c) ####

# plot of seasonal effects
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(raw.data.ex.trend.KS.ex.season$season.effects,
   type = "b",
   col = line.color, xlab = "Month", ylab = "Effect",
   main = "Seasonal effects (after detrending)"
)