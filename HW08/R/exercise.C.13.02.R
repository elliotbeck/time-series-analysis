#
# R code for exercise 13.2 of homework 8
#

# clear memory and close all graphics
# user input
Phi_a <- 0.3 * matrix(rep(1, 4), nrow = 2)
Phi_b <- 0.7 * matrix(rep(1, 4), nrow = 2)
Phi_c <- diag(x = c(0.7, 0.3), nrow = 2)
Phi_d <- matrix(data = c(0.9, 0.5, -0.1, 0.3), nrow = 2, byrow = T)
# compute eigenvalues
ev_a <- eigen(x = Phi_a, only.values = T)$values
ev_b <- eigen(x = Phi_b, only.values = T)$values
ev_c <- eigen(x = Phi_c, only.values = T)$values
ev_d <- eigen(x = Phi_d, only.values = T)$values
# open connection to write output to file
output.file <- paste("HW08/figures/exercise.C.13.02.txt", sep = "")
sink(file = output.file, append = F, type = "output", split = F)
for (model in c("a", "b", "c", "d")) {

   # note that a VAR(1) model is weakly stationary if all eigenvalues
   # of the coefficient matrix lie inside the complex unit circle
   if (max(abs(get(paste("ev_", model, sep = "")))) < 1) {
      cat("Model (", model, ") is weakly stationary.\n", sep = "")
   } else {
      cat("Model (", model, ") is NOT weakly stationary.\n", sep = "")
   }
}

# close connection to write output to file and display file
sink()
file.show(output.file, title = paste("exercise.C.13.02.txt", sep = ""))