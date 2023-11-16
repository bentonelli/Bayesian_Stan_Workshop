// Simple linear regression from: https://mc-stan.org/docs/stan-users-guide/linear-regression.html
data {
  int<lower=0> N;
  vector[N] xx1;
  // (1) Add ct in here 
  //hint: it's easiest to define as a vector with <limits> of [length] called ct (in that order)
  // Now updated to add in a new variable (ct)
  vector[N] yy;
}

parameters {
  real alpha;
  real beta;
  // (2) Add the parameter "theta" in here

  real<lower=0> sigma;
}

model {
  //Priors
  alpha ~ normal(0,10);
  beta ~ normal(0,10);
  
  // (3) Set a prior for theta here
  sigma ~ gamma(3,2);
  
  // (4) Update the line below to include the coeffecient theta, and the variable ct
  yy ~ normal(alpha+beta*xx1+theta*ct,sigma);
}
