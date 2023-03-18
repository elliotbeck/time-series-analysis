#
# R code for exercise 3 of homework 3
#

# user input
data.file.name <- "data/Japan.unemployment.data.dat"
RData.file.name <- "data/class.RData"

# source code
source(file = source.file)

# load 'class.RDdata'
load(file = RData.file.name)

# import data
raw.data <- read.table(file = data.file.name)

# create time series object
raw.data <- ts(data = raw.data[, 2], start = c(1983, 1), frequency = 12)

# specifiy variables for graphs
ylabel <- "per cent"
line.color <- "green4"
title.text <- paste0(
    "harmonized unemployment rates for ",
    # Use regex to extract the country name from the file name:
    gsub("\\..*", "", gsub(".*/", "", data.file.name))
)

# define variables
raw.data.ex.season <- deseason(raw.data)
raw.data.ex.trend.MA <- detrend(raw.data, ma = T)
raw.data.ex.trend.KS <- detrend(raw.data, ma = F)
raw.data.ex.trend.MA.ex.season <- deseason(
    raw.data.ex.trend.MA$detrended.series
)
raw.data.ex.trend.KS.ex.season <- deseason(
    raw.data.ex.trend.KS$detrended.series
)

# time plot and correlogram of deseasonalized data
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))
plot(
    x = raw.data, type = "b", xlab = "time",
    ylab = ylabel, main = "raw series", col = line.color
)
plot(
    x = raw.data.ex.season$deseasoned.series,
    type = "b", xlab = "time", ylab = ylabel,
    main = "deseasonalized series", col = line.color
)
acf(
    x = raw.data, xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
acf(
    x = raw.data.ex.season$deseasoned.series, xlab =
        expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")),
    main = ""
)
mtext(text = title.text, side = 3, line = 0, outer = TRUE)

# time plots and correlograms of deseasonalized MA-detrended data
par(mfrow = c(2, 3))
par(oma = c(0, 2, 4, 2))
plot(
    x = raw.data, type = "b", xlab = "time", ylab = ylabel,
    main = "raw series", col = line.color
)
lines(
    x = as.numeric(time(raw.data.ex.trend.MA$trend)),
    y = as.numeric(raw.data.ex.trend.MA$trend), col = "red"
)
plot(
    x = raw.data.ex.trend.MA$detrended.series, type = "b",
    xlab = "time", ylab = ylabel, main = "MA-detrended series", col = line.color
)
plot(
    x = raw.data.ex.trend.MA.ex.season$deseasoned.series,
    type = "b", xlab = "time", ylab = ylabel,
    main = "deseasonalized MA-detrended series", col = line.color
)
acf(
    x = raw.data, xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
acf(
    x = raw.data.ex.trend.MA$detrended.series,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
acf(
    x = raw.data.ex.trend.MA.ex.season$deseasoned.series,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
mtext(
    text = paste(title.text, " (MA-detrended)", sep = ""),
    side = 3, line = 0, outer = T
)

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
    xlab = "time", ylab = ylabel, main = "KS-detrended series", col = line.color
)
plot(
    x = raw.data.ex.trend.KS.ex.season$deseasoned.series,
    type = "b", xlab = "time", ylab = ylabel,
    main = "deseasonalized KS-detrended series", col = line.color
)
acf(
    x = raw.data, xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
acf(
    x = raw.data.ex.trend.KS$detrended.series,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
acf(
    x = raw.data.ex.trend.KS.ex.season$deseasoned.series,
    xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), main = ""
)
mtext(
    text = paste(title.text, " (KS-detrended)", sep = ""),
    side = 3, line = 0, outer = T
)