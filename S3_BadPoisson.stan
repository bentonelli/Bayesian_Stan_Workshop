//This is a basic mlr model, no poisson proccesses to see here
data {
  int<lower=0> N;
  vector[N] xx1;
  vector[N] xx2;
  vector[N] yy;
}
parameters {
  real alpha;
  real beta1;
  real beta2;
  real <lower=0> sigma;
}
model {
  // Notice here that there are no priors set. This is bad form, but the model will still run.
  // Without specified priors, stan will draw parameter values from a standard normal N(0,1)
  // This may be very innappropriate for what you are doing!
  
  yy ~ normal(alpha+beta1*xx1+beta2*xx2,sigma);
}

// Here we have a new block that allows us to simulate data.
generated quantities{
  real yy_rep[N]; // This creates a new variable that will represent model-generated data
  
  // See how the line below is basically a copy of the model line above.
  // Here though, we use an equal sign and the "normal_rng" function to simulate
  // new yys.
  yy_rep = normal_rng(alpha+beta1*xx1+beta2*xx2,sigma);
}
