
data {
  int<lower=0> N;
  vector[N] lat;
  vector[N] weight_measured;
  vector[N] unc;
  int Nsp;
  int<lower=0,upper=Nsp> ii[N];
}

parameters {
  // Measured weight is a function on the true weight, and the measurement error
  vector[N] weight_true;
  
  real mu_alpha;
  real mu_beta;
  
  vector[Nsp] nu;
  vector<lower=0>[Nsp] sigma_nu;
  real theta;
  
  real<lower=0> sigma_alpha;
  real<lower=0> sigma_beta;
  
  vector[Nsp] alpha;
  vector[Nsp] beta;
  vector<lower=0>[Nsp] sigma;
}

transformed parameters{
  vector[N] lat_transformed;
  lat_transformed = lat-nu[ii];
}

model {
  mu_alpha ~ normal(40,40);
  theta ~ normal(0,1);
  
  mu_beta ~ normal(0,1);
  
  nu ~ normal(40,20);
  sigma_nu ~ gamma(2,5);
  
  sigma_alpha ~ gamma(2,5);
  sigma_beta ~ gamma(1,1);
  
  //Let's estimate the mean latitude of each species
  lat ~ normal(nu[ii],sigma_nu[ii]);
  
  // We can then use that estimated mean latitude here.
  // We will test for an effect of mean species latitude on the mean weight of each species
  alpha ~ normal(mu_alpha + theta * nu,sigma_alpha);
  
  // We can say our slopes with vary, with a mean and sd 
  beta ~ normal(mu_beta,sigma_beta);
  
  sigma ~ gamma(3,3);
  
  //This line will factor in the uncertainty associated with each data point
  weight_measured ~ normal(weight_true,unc);
  //And then use the estimated true weight in the model
  weight_true ~ normal(alpha[ii] + beta[ii] .* lat_transformed, sigma[ii]);
}

