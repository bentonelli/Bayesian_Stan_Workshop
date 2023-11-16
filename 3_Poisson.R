# Code to set up a poisson regression in Stan

# The goal with running this poisson model is:
# 1. See what happens when a model does not fit well
# 2. How to model a dependent variable that doesn't fit a normal distribution

library(rstan)
library(MCMCvis)
library(shinystan)

### Create Data (Fake!) ####
# We will create some data, this time with a real-world example

# We will ask the question: Do birds tend to lay more eggs in nests in areas with
# greater tree cover?

# Our dependent variable will represent the number of eggs found in a birds nest
# We can imagine that each day a team goes out to search for new nests.
# Each new nest is recorded with the number of eggs in each.
# There are going to be between 0 - a bunch of eggs in each nest.

# Let's imagine nests were surveyed during the first month of the breeding season
# We'll represent this as ordinal date (120 ~= May 1)
day_of_sample <- sample(120:150,100,replace=TRUE)

# We will also measure the amount of tree canopy coverage at each site
# we will assume this increases over the course of the study period (bigger leaves, etc.)
cor_factor <- .005

tree_cover <- rnorm(100,.5+cor_factor*(day_of_sample - 135),.1)

# Let's see how tightly these explanatory variables are correlated
plot(day_of_sample,tree_cover)
cor(day_of_sample,tree_cover)

# OK, now let's use these explanatory variables to create our # eggs data

# We will model number of eggs as coming from a Poisson
# Remember lambda is the single parameter that describes a Poisson distribution

#Set an intercept value
alpha <- .75

# We can say that more time in the breeding season leads to more eggs
beta1 <- .02

# And that more tree cover is associated with birds laying more eggs
beta2 <- .2

# Because lambda has to be positive, we need to take the exponent of the linear predictor
# The linear predictor is the "alpha + beta" part of an equation
linear_predictor <- alpha + beta1 * (day_of_sample-135) + beta2 * (tree_cover-.5)

# You can ignore the transformations here for a second
lambda <- exp(linear_predictor)

# We will now use lambda to simulate our egg data
num_eggs <- rpois(length(lambda),lambda)

#Let's see what this looks like:
hist(num_eggs)

# Let's first try to model this as a normal distribution just to see what happens

### Prepare Data for Stan ####

# Stan does best when covariates are centered at 0, and generally conform to a normal distribution
# As a result, to make things run smoother you sometimes need to transform these
# Let's take day_of_sample and transform it to be centered at 0:

hist(day_of_sample) # Not centered at 0!
day_of_sample_cntr <- day_of_sample - mean(day_of_sample)

hist(day_of_sample_cntr) # Yay

#Do the same for tree cover
tree_cover_cntr <- tree_cover-.5
hist(tree_cover_cntr) # Yay

#Feed the data in
input_data <- list(
  #(1) Add all the necessary things in here. Feel free to use the MLR code as a template
  N = ,
  xx1 = , # This will be day of year
  xx2 = , # This will be tree cover
  yy = num_eggs
)

### Run Stan model ####

# Let's run a model that looks very similar to our last one
# It will have a new block, so check it out!
bad_stan_model <- stan(
  file = "S3_BadPoisson.stan",  # Stan program
  data = input_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4             # number of cores (should use one per chain)
)

### Analyze output ####
#Everything looks ok here:
MCMCsummary(bad_stan_model,params=c("alpha","beta1","beta2","sigma"))

# And here
launch_shinystan(bad_stan_model)

# But now let's look at predicted values of our model
# We can do this by reading in our predicted/simulated values
simulated_values <- MCMCchains(bad_stan_model,params="yy_rep")

#Start by plotting our actual data
par(mfrow=c(2,1))
hist(num_eggs,breaks=100)

#We can take the first saved iteration and plot that
hist(simulated_values[1,],breaks=100) #What's wrong with this picture!
#This means that our model is capable of predicting NEGATIVE eggs (THE HORROR!)
# It also simulates non-integer results. That is also not right

#(2) Before you move on, change the indexing 4 lines above from [1,] to [2,],[3,], and [4,]. 
# How does the simulated output change?

#(3) PAUSE HERE & let Ben know you are done with this section.

### A better model #### 
# Let's run a model that actually models things based on a poisson!

#(4) Before you run this, navigate to the .stan file and set some things up
good_stan_model <- stan(
  file = "S3_GoodPoisson.stan",  # Stan program
  data = input_data,    # named list of data
  chains = 4,             # number of Markov chains
  #(5) Change the code below to run this model with 2000 total iterations, with 1000 of those being warmup 
  warmup = 2000,          # number of warmup iterations per chain
  iter = 4000,            # total number of iterations per chain
  cores = 4             # number of cores (should use one per chain)
)

MCMCsummary(good_stan_model,params=c("alpha","beta1","beta2"))
launch_shinystan(good_stan_model)

#Now lets see if our simulated data does a better job of modeling the data
simulated_values <- MCMCchains(good_stan_model,params="yy_rep")
par(mfrow=c(2,1))
hist(num_eggs,breaks=100)

#We can take the first saved iteration and plot that
hist(simulated_values[1,],breaks=100) #This looks a lot more on the nose!

#(6) Go back and change the cor_factor in the data simulation section from .005 to .02
# Then run the "good" model again with the new data. How do the results change?

#(7) Launch shinystan with the high cor_factor model. Navigate to "Explore" select "bivariate"
# and then select "beta1" under "Select parameter" and "beta2" under "y-axis." Is there are correlation there?
# Think about what that means and why that might be happening.

#(8) Extra challenge! The "transformed parameters" data block is actually unnecessary in this code,
# along with the lin_pred and lambda parameters. Rewrite the .stan file without the "transformed parameters"
# block, instead inserting the linear predictor and transformations directly into the poisson() function.

