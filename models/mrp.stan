//
// MRP component of model.stan. 
//
// Note that in the main model we use the national polls effectively to produce
// an estimate of national swing. We then take the previous TPP result for
// each division modified by the national swing as a rough prior for the expected
// TPP result. 
//
// In this model, we simply take the prior TPP result for each division as
// our rough prior.

data {
  int<lower=0> n_records;
  int<lower=0> n_covariates;
  int<lower=0> n_divisions; 
  array[n_records] int<lower=0, upper=n_divisions> record_division; # map obs to division
  array[n_divisions] real<lower=0, upper=100> tpp_div_prev;
  matrix[n_records, n_covariates] x;
  array[n_records] int<lower=0> tpp_record;
}

parameters {
  vector[n_covariates] beta; // fixed effect coefs
  array[n_divisions] real<lower=0, upper=100> tpp_div_curr;
}

transformed parameters {
  array[n_records] real<lower=0, upper=1> theta;
  for (n in 1:n_records) {
    int d = record_division[n];
    theta[n] = (tpp_div_curr[d] + x[n] * beta)/100;
  }
}

model {
  for (n in 1:n_records) {
    int d = record_division[n];
    tpp_div_curr[d] ~ normal(tpp_div_prev[d], 5);
    tpp_record[n] ~ bernoulli(theta[n]);
  }
  beta ~ normal(0, 5); 
}
