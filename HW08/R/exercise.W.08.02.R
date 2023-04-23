#
# R code for problem 2 of homework 8
#

# user input
capital.t <- 100
K <- 2
m <- 2
p <- 1
C.0 <- matrix(data = c(1.15, 0.22, 0.22, 0.99), nrow = 2, byrow = TRUE)
C.1 <- matrix(data = c(0.02, 0.01, 0.14, -0.14), nrow = 2, byrow = TRUE)
C.2 <- matrix(data = c(-0.08, -0.09, 0.00, -0.21), nrow = 2, byrow = TRUE)

# definition of delimiters for output
delimiter.hyphen <- paste(
   paste(rep("-", 80), sep = "", collapse = ""), "\n",
   sep = ""
)

# compute Portmanteau statistic Q
Q <- 0
C.0.inv <- solve(C.0)
for (k in 1:K) {
   C.k <- get(paste("C.", k, sep = ""))
   Q <- Q + sum(diag(t(C.k) %*% C.0.inv %*% C.k %*% C.0.inv))
}
Q <- capital.t * Q

# compute Ljung-Box-Pierce statistic Q.tilde
Q.tilde <- 0
for (k in 1:K) {
   C.k <- get(paste("C.", k, sep = ""))
   Q.tilde <- Q.tilde + sum(diag(t(C.k) %*% C.0.inv %*% C.k %*% C.0.inv)) / (capital.t - k)
}
Q.tilde <- capital.t * (capital.t + 2) * Q.tilde

# compute p-values for Q and Q.tilde
deg.freedom <- (K - p) * m^2
Q.pvalue <- pchisq(q = Q, df = deg.freedom, lower.tail = FALSE)
Q.tilde.pvalue <- pchisq(q = Q.tilde, df = deg.freedom, lower.tail = FALSE)

# open connection to write output to file
output.file <- "HW08/figures/exercise.W.08.02.txt"
sink(file = output.file, append = F, type = "output", split = F)

cat(delimiter.hyphen)
cat("Portmanteau statistic\n")
cat(delimiter.hyphen)
cat("Q =", Q, "\n")
cat("degrees of freedom =", deg.freedom, "\n")
cat("p-value =", Q.pvalue, "\n\n")

cat(delimiter.hyphen)
cat("Ljung-Box-Pierce statistic\n")
cat(delimiter.hyphen)
cat("Q.tilde =", Q.tilde, "\n")
cat("degrees of freedom =", deg.freedom, "\n")
cat("p-value =", Q.tilde.pvalue, "\n\n")

# close connection to write output to file and display file
sink()
file.show(output.file, title = paste("exercise.W.08.02.txt", sep = ""))