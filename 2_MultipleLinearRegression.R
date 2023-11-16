# Code to set up a multiple linear regression in Stan

# This code needs to be adapted to augment a basic linear regression model.
# We'll add a new parameter, theta, to our model
# Theta will describe the effect of a binary variable on yy (i.e. control versus treatment)

library(rstan)
library(MCMCvis)
library(shinystan)

### Create Data (Fake!) ####
# Create some data where we know the exact relationships between variables
aa <- -1.1 # Alpha
bb <- 0.75 # Beta
ss <- 3.2 # Sigma

# (1) Add a new parameter here called "tt" and set it's value to 0.01
# Setting this value to 0.01 means there is a tiny effect of the treatment on our dependent variable
        #Theta, representing the effect of treatment

xx1 <- runif(100,-10,10) # Create independent variable, X1

# Simulate some new fake data for a new independent variable called "ct" (control,treatment)
# Zeros will represent the control group
# Ones will represent the treatment group
ct <- sample(c(0,1),100,replace=TRUE)

# (2) Augment the code below, adding tt * ct into the equation
yy <- aa + bb * xx1 + rnorm(length(xx1),0,ss) # Create dependent variable, Y

# Think for a second about how this will work...
# When ct = 0 (control), tt * ct = 0. When ct = 1, tt * ct = 0.01.
# So theta only gets "activated" when a datapoint is in the treatment group

plot(xx1,yy,pch=19) # Plot to see relationship between 1st variable (xx1) and y 
boxplot(yy~ct) # Plot to see relationship between 2nd variable (ct) and y 

#Let's combine this into one dataframe
our_data <- data.frame(yy=yy,xx1=xx1,ct=ct)
head(our_data)

### Prepare Data for Stan ####
input_data <- list(
  N = nrow(our_data),
  xx1 = our_data$xx1,
  # (3) Add ct in here

  yy = our_data$yy
)

### Run Stan model ####
# (4) Head over to the stan code and follow the instructions there
stan_model <- stan(
  file = "S2_MultipleLinearRegression.stan",  # Stan program
  data = input_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4             # number of cores (should use one per chain)
)

### Analyze output ####
MCMCsummary(stan_model)
launch_shinystan(stan_model)

### Also run a frequentist version for kicks ####
glm1 <- glm(yy~xx1 + ct)
summary(glm1)
