#
# R code for problem 1 (b) of homework 12
#

# load libraries
library(sandwich) # function: vcovHAC
library(lmtest) # function: coeftest
library(car) # function: linearHypothesis

# time series to analyze
infl <- us.data[, "infl"]
unemp <- us.data[, "unemp"]
infl.diff.1.lag.1 <- diff(infl[3:(length(infl) - 1)], 1)
infl.diff.1.lag.2 <- diff(infl[2:(length(infl) - 2)], 1)
infl.diff.1.lag.3 <- diff(infl[1:(length(infl) - 3)], 1)
unemp.diff.1.lag.0 <- diff(unemp[4:length(unemp)], 1)


# fit linear model
model.fit <- lm(
    formula = unemp.diff.1.lag.0 ~ 1 + infl.diff.1.lag.1 +
        infl.diff.1.lag.2 + infl.diff.1.lag.3
)


# F tests
hypothesis.matrix <- matrix(c(0, 0, 1, 0, 0, 0, 0, 1), nrow = 2, byrow = TRUE)
lin.hyp.std <- linearHypothesis(
    model = model.fit, hypothesis.matrix = hypothesis.matrix,
    rhs = c(0, 0), test = "F", vcov. = vcov(model.fit)
)
lin.hyp.HAC <- linearHypothesis(
    model = model.fit, hypothesis.matrix = hypothesis.matrix,
    rhs = c(0, 0), test = "F", vcov. = vcovHAC(model.fit)
)

# fitted models
# fitted model (standard)
print(summary(model.fit))
# fitted model (HAC)
print(coeftest(model.fit, vcov = vcovHAC(model.fit)))

# F tests
# based on standard OLS output
print(lin.hyp.std)
# based on HAC estimator for covariance matrix of parameter estimates
print(lin.hyp.HAC)