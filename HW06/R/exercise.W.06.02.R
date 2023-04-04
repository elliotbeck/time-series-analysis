#
# R code for exercise 2 of homework 6
#


# clear memory and close all graphics
rm(list = ls())
graphics.off()
options("width" = 300)

# load data
load(file = "data/class.RData")

# user in put
alpha <- c(0.25, 0.5, 75)

# define function to compute SS and make forecasts with exponential smoothing
es.fore.ss <- function(x, alpha, h.max, digits = 3) {
   T <- length(x)
   e <- rep(0, T)
   x.hat <- x[1]
   e[2] <- x[2] - x.hat
   for (t in (3:T)) {
      x.hat <- alpha * e[t - 1] + x.hat
      e[t] <- x[t] - x.hat
   }
   ss <- sum(e^2)
   x.hat.old <- x.hat
   x.hat.new <- x[T]
   fore <- alpha * x[T] + (1 - alpha) * x.hat
   fore <- rep(fore, h.max)
   return(list(fore = round(fore, digits), ss = round(ss, digits)))
}

# define function to compute SS and make forecasts with
# exponential smoothing for integrated series
es.int.fore.ss <- function(x, alpha, h.max, digits = 3) {
   x.diff <- diff(x)
   res <- es.fore.ss(x.diff, alpha, h.max)
   T <- length(x)
   fore <- x[T] + cumsum(res$fore)
   return(list(fore = round(fore, digits), ss = round(res$ss, digits)))
}

# compare to ETS function from package "forecast"
library(forecast)

fit <- ets(diff(us.gnp), model = "ANN", alpha = 0.75)
fit
forecast(fit)
plot(forecast(fit))