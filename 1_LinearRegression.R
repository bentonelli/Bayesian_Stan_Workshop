# Code to set up a basic linear regression in Stan
library(rstan)
library(MCMCvis)
library(shinystan)

### Create Data (Fake!) ####
# Create some data where we know the exact relationships between variables
aa <- -1.1 # Alpha
bb <- 0.75 # Beta
ss <- 3.2 # Sigma

xx <- runif(100,-10,10) # Create independent variable, X

yy <- aa + bb * xx + rnorm(length(xx),0,ss) # Create dependent variable, Y with both deterministic part and random error

plot(xx,yy,cex=1.5,pch=19,col="orchid4") # Plot to see relationship

### Prepare Data for Stan ####
input_data <- list(
  N = length(xx),
  xx = xx,
  yy = yy
)

### Run Stan model ####
stan_model <- stan(
  file = "S1_LinearRegression.stan",  # Stan program
  data = input_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4             # number of cores (should use one per chain)
)

### Analyze output ####
MCMCsummary(stan_model)
launch_shinystan(stan_model)

### Also run a frequentist version for kicks ####
glm1 <- glm(yy~xx)
summary(glm1)
