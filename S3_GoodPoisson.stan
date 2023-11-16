//Poisson Model

data {
  int<lower=0> N;
  vector[N] xx1;
  vector[N] xx2;
  //(1) Set up our variable yy here as an "int". 
  //Think about if/what limits should be placed in this...
  
}
parameters {
  //(2) Set up three parameters, alpha, beta1 and beta2

}

// Here we have a new block used for parameters that are derivative 
transformed parameters{
  vector[N] lin_pred; // Create our linear predictor variable
  vector<lower=0>[N] lambda; // Create lambda
  
  lin_pred = alpha+beta1*xx1+beta2*xx2; // Linear predictor equation
  lambda = exp(lin_pred); // Transform
}

model {
  // Let's set up priors this time...
  alpha ~ normal(1,.5);
  beta1 ~ normal(0,.1);
  beta2 ~ normal(0,.25);
  
  yy ~ poisson(lambda);
}

// In this block, all we have to change is the equation.
generated quantities{
  real yy_rep[N];
  // (3) Set up the equation to get simulated data from a poisson. Hint: poisson_rng is the function you want.
  
}
