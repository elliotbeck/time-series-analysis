#
# R code for exercise 3.4 in Chatfield
#

# compute acf
lags <- seq(from = -6, to = 6, step = 1)
a.c.f <- 0.7^abs(lags)
# plot acf of radom series
par(mfrow = c(1, 1))
par(oma = c(0, 2, 4, 2))
plot(
    x = lags, y = a.c.f, xlab = expression(paste("lag ", tau, sep = "")),
    ylab = expression(paste("acf(", tau, ")", sep = "")), col = "green4",
    main = ""
)
mtext(
    text = paste0(
        "autocorrelation coefficients of ",
        "AR(1)-process X(t) - mu = 0.7 * (X(t-1) - mu) + Z(t)"
    ),
    side = 3, line = 0, outer = T
)