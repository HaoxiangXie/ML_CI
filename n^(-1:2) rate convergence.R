rm(list = ls())

library(tidyverse)
library(ggplot2)

sim_lm <- function(x, betas, nsims, err = 1) {
  nobs <- dim(x)[1]
  matrix(rnorm(nobs * nsims, mean = x %*% betas, sd = err),
         nrow = nobs, ncol = nsims)
}

betas <- c(2, 3, 4, 5, 6, 1) # the true values of the regression coefficients


#### simulation data(1000 observations, simulate 500 times)


n <- 1000
p <- length(betas) - 1
nsims <- 500

set.seed(1024)
x <- cbind(1, matrix(rnorm(n * p, sd = 5), nrow = n, ncol = p))
x %>% dim()

set.seed(119)
y <- sim_lm(x, betas, nsims, err = 10)
y %>% dim()

# estimate OLS model for each simulation

ols <- function(x, y) {
  model <- lm(y ~ x[, -1]) # exclude the intercept column (1's) when using lm()
  c(model$coefficients[1], model$coefficients[-1]) # re-order the coefficients to match the original script
}

betas_est <- matrix(0, nrow = length(betas), ncol = nsims)
dim(betas_est)

for (i in 1:nsims) betas_est[, i] <- ols(x, y[, i])

# plot kernel densities of estimated coefficients
betas_df <- as.data.frame(t(betas_est))

names(betas_df) <- paste0("beta", 0:p)

ggplot(betas_df, aes(beta1)) + geom_density() +
  geom_vline(xintercept = betas[2], col = "red", linetype = "dashed") # 3

ggplot(betas_df, aes(beta2)) + geom_density() +
  geom_vline(xintercept = betas[3], col = "red", linetype = "dashed") # 4

ggplot(betas_df, aes(beta3)) + geom_density() +
  geom_vline(xintercept = betas[4], col = "red", linetype = "dashed") # 5

ggplot(betas_df, aes(beta4)) + geom_density() +
  geom_vline(xintercept = betas[5], col = "red", linetype = "dashed") # 6



################################################################################       


##### When an estimator's convergence rate is n^(-1/2), 
##### it means that the estimator's accuracy improves 
##### at a rate proportional to the square root of the sample size (n)

##### n^(-1/2): if you want to reduce the error by a factor of 2, 
##### you would need to increase the sample size by a factor of 4


## Sample mean

rm(list = ls())
# Set true mean and standard deviation of the normal distribution
true_mean <- 5
true_sd <- 2

# Define the function to calculate the average absolute error of sample means
average_abs_error <- function(n, num_simulations = 1000) {
  errors <- replicate(num_simulations, {
    # Generate a sample of size n from the normal distribution
    sample_data <- rnorm(n, mean = true_mean, sd = true_sd)
    
    # Calculate the sample mean
    sample_mean <- mean(sample_data)
    
    # Calculate the absolute error of the sample mean
    abs_error <- abs(sample_mean - true_mean)
    
    return(abs_error)
  })
  
  return(mean(errors))
}

# Choose a sample size (n)
n <- 100

# Calculate the average absolute errors for sample sizes n and 4n
error_n <- average_abs_error(n)
error_4n <- average_abs_error(4 * n)
error_16n <- average_abs_error(16 * n)

# Display the results
cat("Average absolute error for sample size", n, ":", error_n, "\n")

cat("Average absolute error for sample size", 4 * n, ":", error_4n, "\n")
cat("Ratio of the average absolute errors:", error_n / error_4n, "\n")

cat("Average absolute error for sample size", 16 * n, ":", error_16n, "\n")
cat("Ratio of the average absolute errors:", error_4n / error_16n, "\n")

## 
rm(list = ls())
# Load library
library(tidyverse)

# Set true parameters for the linear model
true_intercept <- 3
true_slope1 <- 2
true_slope2 <- -1

# Define a function to calculate the average absolute error of the OLS estimator
average_abs_error_ols <- function(n, num_simulations = 1000) {
  errors_slope1 <- errors_slope2 <- vector("numeric", num_simulations)
  
  for (i in 1:num_simulations) {
    # Generate a sample of size n from the normal distribution for predictors
    x1 <- rnorm(n, mean = 5, sd = 2)
    x2 <- rnorm(n, mean = 3, sd = 1)
    epsilon <- rnorm(n, mean = 0, sd = 1)
    
    # Generate the dependent variable using the true parameters
    y <- true_intercept + true_slope1 * x1 + true_slope2 * x2 + epsilon
    
    # Fit the OLS regression
    model <- lm(y ~ x1 + x2)
    
    # Calculate the absolute error of the OLS estimators for the slopes
    errors_slope1[i] <- abs(coef(model)["x1"] - true_slope1)
    errors_slope2[i] <- abs(coef(model)["x2"] - true_slope2)
  }
  
  # Return the mean of the absolute errors for both slopes
  return(c(mean(errors_slope1), mean(errors_slope2)))
}

# Choose an initial sample size (n)
n <- 100

# Calculate the average absolute errors for different sample sizes
error_n <- average_abs_error_ols(n)
error_4n <- average_abs_error_ols(4 * n)
error_16n <- average_abs_error_ols(16 * n)

# Show the results
cat("For predictor x1:\n")
cat("Average absolute error for sample size", n, ":", error_n[1], "\n")

cat("Average absolute error for sample size", 4 * n, ":", error_4n[1], "\n")
cat("Ratio of the average absolute errors:", error_n[1] / error_4n[1], "\n\n")

cat("Average absolute error for sample size", 16 * n, ":", error_16n[1], "\n")
cat("Ratio of the average absolute errors:", error_4n[1] / error_16n[1], "\n\n")


cat("For predictor x2:\n")
cat("Average absolute error for sample size", n, ":", error_n[2], "\n")

cat("Average absolute error for sample size", 4 * n, ":", error_4n[2], "\n")
cat("Ratio of the average absolute errors:", error_n[2] / error_4n[2], "\n")

cat("Average absolute error for sample size", 16 * n, ":", error_16n[2], "\n")
cat("Ratio of the average absolute errors:", error_4n[2] / error_16n[2], "\n\n")









