#
# R code for exercise 2.1 in Chatfield & Xing
#

# clear memory and close all graphics
rm(list = ls())

# user input
data.file.name <- "HW01/R/exercise.C.02.01.dat"

# import data
data.df <- read.table(file = data.file.name, header = TRUE, sep = ";")

# create time-series object
data.ts <- ts(
    data = data.df$sales,
    start = 1995, frequency = 13
)

# create time plot
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = data.ts, type = "b", xlab = "Time", ylab = "Sales",
    main = "Sales of Company X", col = "green4"
)

# decompose the series
decomp_data <- decompose(data.ts)
plot(decomp_data)