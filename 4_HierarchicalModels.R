# Hierarchical Models

library(rstan)
library(MCMCvis)
library(shinystan)
library(dplyr)
# Let's focus on a new "real world" example

# Bergmann's rule suggests that animals at higher latitudes should be larger
# This variation has been observed ACROSS bird species, but also WITHIN bird species

# So let's look at how we can estimate this inter- and intra-specific variation simultaneously

# We can imagine that some species follow this rule "better" than others
# There may be heavy species at low latitudes, and lighter species at high latitudes
# And within species some birds may not change mass with latitude at all, while others do a lot!

### Create Data (Fake!) ####

# The fake data simulation here is a little more complicated than previous examples.
# So don't worry too much about understanding it all

#Set a number of species
num_species <- 20

#Set the mean and sd of each species latitudinal position
avg_sp_lat <- runif(num_species,10,70)
avg_sp_lat_range <- runif(num_species,5,15)

# Define intra-species effects of latitude
sp_lat_weight_rel <- rnorm(num_species,.01,.005)

#Set inter-species effect of latitude on mass
overall_sp_weight_eff <- .75

#Define average species weights and variance
avg_sp_weights <- rnorm(num_species,70+(avg_sp_lat-mean(avg_sp_lat))*overall_sp_weight_eff,15) 
plot(avg_sp_lat,avg_sp_weights,pch=19)
sp_weight_var <- avg_sp_weights/10 + runif(num_species,0,1)

#Set the number of records for each species
num_spec_records <- round(exp(runif(num_species,3,6)))

# We will create weight data for birds from these 10 species, with varying numbers
# of records for each. We'll give each record a latitude as well
full_data <- data.frame(species_num = NULL,latitude=NULL,weight=NULL,measurement_err=NULL)
for (each_sp in 1:num_species){
  spec_num <- rep(each_sp,num_spec_records[each_sp]) # Create species numbers
  spec_lats <- rnorm(num_spec_records[each_sp],avg_sp_lat[each_sp],avg_sp_lat_range[each_sp]) # Randomly select species lats
  
  # Calculate expected weight of each species
  exp_weight <- avg_sp_weights[each_sp]+avg_sp_weights[each_sp]*(spec_lats-avg_sp_lat[each_sp])*sp_lat_weight_rel[each_sp]
  
  spec_weights <- rnorm(num_spec_records[each_sp],exp_weight,sp_weight_var[each_sp])
  
  # We will also assume measurements came from one of three scales, varying in their
  # uncertainty
  measurement_err <- sample(c(.1,.5,1),length(spec_weights),replace=TRUE)
  spec_weights_w_unc <- spec_weights + measurement_err * rnorm(length(spec_weights),0,1)
  
  #Add to full dataset
  spec_data <- data.frame(species_num = spec_num,latitude=spec_lats,weight=spec_weights,measurement_err=measurement_err)
  full_data <- rbind(full_data,spec_data)
}

# Just for fun let's see if there's a relationship between latitude and mass before accounting for species
# Depending on the seed, this plot can look wildly different!
par(mfrow=c(1,1))
plot(full_data$latitude,full_data$weight,col=as.factor(full_data$species_num),pch=19,cex=.5)
abline(lm(full_data$weight~full_data$latitude))
summary(lm(full_data$weight~full_data$latitude))

#While we are at it, let's also look at how variable weights are within species
weight_sd_by_spec <- full_data %>% 
  group_by(species_num) %>% 
  summarise(weight_sd = sd(weight))

# We can see that this varies widely - we can/should account for this in our model...
head(weight_sd_by_spec,10)

# (1) Before moving on (and before peeking a the stan file), 
# write out the model equations that are appropriate for addressing
# our questions. Remember that we want to model the weight as a function of 
# latitude both within and among species. 

### Prepare Data for Stan ####
input_data <- list(
  N = nrow(full_data),
  lat = full_data$latitude,
  weight_measured = full_data$weight,
  unc = full_data$measurement_err,
  # Here we need to do some indexing at the species level
  Nsp = length(unique(full_data$species_num)), # This is the number of unique species
  ii = full_data$species_num # This is the data for species identities
)

### Run Stan model ####
stan_model <- stan(
  file = "S4_HierarchicalModels.stan",  # Stan program
  data = input_data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 4             # number of cores (should use one per chain)
)

### Analyze output ####

# We can first look at the top-level results of this model
MCMCsummary(stan_model,params=c("mu_alpha","mu_beta","theta"))

#Let's look at the slope parameters for each species
MCMCplot(stan_model,params="nu") # Mean latitude of each species (think about why this is centered at 0!)
MCMCplot(stan_model,params="sigma_nu")

MCMCplot(stan_model,params="alpha") # Mean weight of each species (Why are some of these more certain?)
MCMCplot(stan_model,params="beta") # Species-level relationships of latitude x weight

MCMCplot(stan_model,params="sigma") # Species-level relationships of latitude x weight

# Let's look at some relationships here between parameters
alphas_median <- MCMCsummary(stan_model,params="alpha")$'50%' # Median estimates for alphas
nus_median <- MCMCsummary(stan_model,params="nu")$'50%' # Median estimates for alphas
betas_median <- MCMCsummary(stan_model,params="beta")$'50%' # Median estimates for sigmas

#We can see how well our models recovered the average latitude of each species
plot(avg_sp_lat,nus_median,pch=19)

# And the average weights
plot(avg_sp_weights,alphas_median,pch=19)
abline(a=1,b=1)

#and some other stuff
plot(sp_lat_weight_rel,betas_median,pch=19)

plot(alphas_median,betas_median,pch=19)

launch_shinystan(stan_model)
