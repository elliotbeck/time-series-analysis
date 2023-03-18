#
# R code for exercise 4 of homework 3
#

# user in put
data.file.name <- "data/Dow Jones.data.dat"

# import data
raw.data <- read.csv(file = data.file.name)

# create time series objects
price <- ts(
    data = raw.data$DJ.industrial.average,
    start = c(1896, 5),
    frequency = 12
)
total.return.index <- ts(
    data = raw.data$DJ.industrial.total.return.index,
    start = c(1896, 5),
    frequency = 12
)

# specifiy variables for graphs
line.color <- "green4"

# time plot of time series: price
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))
plot(
    x = price, type = "b", xlab = "time", ylab = "USD",
    main = "price", col = "green4"
)
plot(
    x = log(price), type = "b", xlab = "time", ylab = "USD",
    main = "log(price)", col = "green4"
)
plot(
    x = diff(price, 1), type = "b", xlab = "time", ylab = "log USD",
    main = "price change", col = "green4"
)
plot(
    x = diff(log(price), 1), type = "b", xlab = "time", ylab = "log USD",
    main = "change in log(price)", col = "green4"
)
mtext(
    text = "Dow Jones Industrial Average - Close", side = 3, line = 0,
    outer = T
)

# time plot of time series: total return index
open.graphics.window(width = 11.7, height = 8.3, dpi = 100)
par(mfrow = c(2, 2))
par(oma = c(0, 2, 4, 2))
plot(
    x = total.return.index, type = "b", xlab = "time",
    ylab = "USD", main = "total return index", col = "blue"
)
plot(
    x = log(total.return.index), type = "b", xlab = "time",
    ylab = "USD", main = "log(total return index)", col = "blue"
)
plot(
    x = diff(total.return.index, 1), type = "b", xlab = "time",
    ylab = "log USD", main = "change in total return index", col = "blue"
)
plot(
    x = diff(log(total.return.index), 1), type = "b", xlab = "time",
    ylab = "log USD", main = "change in log(total return index)", col = "blue"
)
mtext(
    text = "Dow Jones Industrial Total Return Index", side = 3,
    line = 0, outer = TRUE
)