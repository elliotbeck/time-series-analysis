#
# R code for exercise 1 of homework 4
#

# define function to compute theoretical ac.f. of AR(2)-process:
# Yule-Walker method
ac.f.AR2 <- function(ar.coef, lag.max) {
   acf.coef <- rep(NA, 1 + lag.max)
   acf.coef[1:2] <- c(1, ar.coef[1] / (1 - ar.coef[2]))
   for (k in 3:(1 + lag.max)) {
      acf.coef[k] <- sum(ar.coef * acf.coef[(k - 1):(k - 2)])
   }
   names(acf.coef) <- 0:lag.max
   return(acf.coef)
}

# user input
max.lag <- 100
ar.coef <- c(0.0, -0.9)

# compute roots of AR operator
AR.operator.roots <- polyroot(c(1, -ar.coef))

# compute ac.f.
ac.f. <- ac.f.AR2(ar.coef, max.lag)

# plot ac.f.
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
   x = 0:max.lag, y = ac.f., type = "h", lwd = 2,
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("ac.f.(", tau, ")", sep = "")),
   col = "green4", main = ""
)
lines(x = c(-10, max.lag + 10), y = c(0, 0))
title(
   main = "Theoretical autocorrelation function",
   font.main = 1, cex.main = 1
)
title.text <- paste("AR(2)-process X(t) = ",
   ifelse(ar.coef[1] == 0, " ", paste(sprintf("%+g", ar.coef[1]),
      "* X(t-1) ",
      sep = ""
   )), sprintf("%+g", ar.coef[2]), "* X(t-2) + Z(t)",
   sep = ""
)
mtext(text = title.text, side = 3, line = 0, outer = T, font = 2, cex = 1)