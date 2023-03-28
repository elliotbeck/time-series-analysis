#
# R code for exercise 4.5 in Chatfield
#


# construct acf object
n <- 100
s.acf <- c(1, 0.31, 0.37, -0.05, 0.06, -0.21, 0.11, 0.08, 0.05, 0.12, -0.01)


# plot sample ac.f.
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
   x = 0:(length(s.acf) - 1), y = s.acf, type = "h", lwd = 2,
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("sample ac.f.(", tau, ")", sep = "")),
   col = "green4", main = ""
)
lines(x = c(-1, length(s.acf) + 1), y = c(0, 0))
lines(
   x = c(-1, length(s.acf) + 1), y = (-1 / n) - 1.96 * c(1, 1) / sqrt(n),
   lty = 2, col = "blue"
)
lines(
   x = c(-1, length(s.acf) + 1), y = (-1 / n) + 1.96 * c(1, 1) / sqrt(n),
   lty = 2, col = "blue"
)
axis(
   side = 2, at = c(seq(-1, -0.1, 0.1), seq(0.1, 1, 0.1)),
   labels = c(seq(-1, -0.1, 0.1), seq(0.1, 1, 0.1)), tick = TRUE
)
mtext(
   text = "Exercise 4.5: Correlogram of given time series", side = 3,
   line = 0, outer = TRUE, font = 2, cex = 1
)


# plot ac.f. of competing AR(p) process that matches the first
# (p+1) values of the given sequence of autocorrelations, 1<=p<=10
par(mfrow = c(2, 5))
par(oma = c(0, 2, 4, 2))
ARmodels <- acf2AR(s.acf)
for (m in 1:nrow(ARmodels))
{
   ARacf <- ARMAacf(
      ar = ARmodels[m, ][1:m], ma = NULL,
      lag.max = 10, pacf = FALSE
   )
   plot(
      x = 0:(length(s.acf) - 1), y = ARacf, type = "h",
      xlab = expression(paste("lag ", tau, sep = "")),
      ylab = expression(paste("ac.f.(", tau, ")", sep = "")),
      ylim = c(-0.2, 1), lwd = 2, col = "blue"
   )
   points(x = 0:(length(s.acf) - 1), y = s.acf, type = "p", col = "red")
   lines(x = c(-1, length(s.acf) + 1), y = c(0, 0))
   title(
      main = paste("AR(", m, ") process", sep = ""), font.main = 1,
      cex.main = 1
   )
}
mtext(
   text = "Exercise 4.5: ac.f. of AR(p) process [blue bars] that matches the first (p+1) values
    of the given sequence of autocorrelations [red points], 1<=p<=10",
   side = 3, line = 0, outer = TRUE, font = 2, cex = 1
)