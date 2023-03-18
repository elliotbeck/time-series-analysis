#
# R code for exercise 2 of homework 4
#

# user in put
models <- c("a", "b", "c")
max.lag <- 20
ar.coef <- rbind(c(-1.6, -0.64), c(0.4, 0.45), c(1.2, -0.85))


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

# define function to invert AR operator of degree 2: AR2 -> MA
AR2toMA <- function(ar.coef, lag.max) {
   ma.coef <- rep(NA, 1 + lag.max)
   ma.coef[1:2] <- c(1, ar.coef[1])
   for (k in 3:(1 + lag.max)) {
      ma.coef[k] <- sum(ar.coef * ma.coef[(k - 1):(k - 2)])
   }
   names(ma.coef) <- 0:lag.max
   return(ma.coef)
}

# choose model (1-3)
model <- 2

# compute roots of AR operator
AR.operator.roots <- polyroot(c(1, -ar.coef[model, ]))

# invert AR operator (Note: There is an internal function called
# 'ARMAtoMA' which does the same job.)
MA.operator <- AR2toMA(
   ar.coef = ar.coef[model, ],
   lag.max = max.lag
)

# compute ac.f. (Note: There is an internal function called 'ARMAacf'
# that does the same job.)
ac.f. <- ac.f.AR2(ar.coef[model, ], max.lag)

# plot coefficients of MA operator
par(mfrow = c(2, 1))

# plot ac.f.
plot(
   x = 0:max.lag, y = ac.f., type = "h", lwd = 2, ylim = c(-1, 1),
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("ac.f.(", tau, ")", sep = "")),
   col = "green4"
)

lines(x = c(-1, max.lag + 1), y = c(0, 0))
title(
   main = "Theoretical autocorrelation function", font.main = 1,
   cex.main = 1
)
title.text <- paste("AR(2) process: X(t) = ",
   ifelse(ar.coef[model, 1] == 0, " ",
      paste(sprintf("%+g", ar.coef[model, 1]),
         "* X(t-1) ",
         sep = ""
      )
   ), sprintf(
      "%+g",
      ar.coef[model, 2]
   ), "* X(t-2) + Z(t)",
   sep = ""
)
mtext(
   text = title.text, side = 3, line = 0,
   outer = TRUE, font = 2, cex = 1
)

plot(
   x = 0:max.lag, y = MA.operator, type = "h", lwd = 2,
   xlab = expression(paste("lag ", tau, sep = "")),
   ylab = expression(paste("psi(", tau, ")", sep = "")),
   col = "green4"
)
title(
   main = "Coefficients of MA operator", font.main = 1,
   cex.main = 1
)
lines(x = c(-1, max.lag + 1), y = c(0, 0))