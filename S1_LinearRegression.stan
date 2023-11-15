// Simple linear regression from: https://mc-stan.org/docs/stan-users-guide/linear-regression.html

// The data block reads in the data the model needs. It follows the structure of
// the list passed as the "data" argument in the R script.
data {
  int<lower=0> N; //This is the number of data points
  vector[N] xx; // Here is our independent variable
  vector[N] yy; // And our dependent variable
}

// The parameter block sets up what paramters you are trying to estimate. Note
// here that you just define what type these come in (reals, vectors, etc.),
// and also if there are any limits on those parameters.
parameters {
  real alpha; // We have three parameters to model, alpha is the intercept
  real beta; // Beta is the slope
  real<lower=0> sigma; // And sigma is the variance
}

// Now comes the actual model. Here we define the priors as coming from distributions,
// and then write out our model equation. 
model {
  //vector[N] mu; // See below for alternate parameterization discussion
  
  //Priors
  alpha ~ normal(0,10); // 0 is mean, 10 is standard deviation
  beta ~ normal(0,10); // Same here
  
  // Here sigma is drawn from a gamma prior. It doesn't need to be...
  // But - sigma has a lower limit of 0, so drawing from a normal isn't going to
  // help your model fit.
  // To visualize gammas, this applet is helpful: https://homepage.divms.uiowa.edu/~mbognar/applets/gamma.html
  sigma ~ gamma(3,2);  
  
  //Here is where it all comes together!
  yy ~ normal(alpha+beta*xx,sigma);
  
  // We could parameterize the model so that we estimate mu, the mean expected
  // value at each datapoint, but this is more cumbersome in my opinion. If you wanted to do that,
  // you could use the code below.
  
  //mu = alpha+beta*xx;
  //yy ~ normal(alpha+beta*xx,sigma);
}
