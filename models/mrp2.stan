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
  
  array[n_records] int<lower=1, upper=2> sex_record;
  array[n_records] int<lower=1, upper=4> age_record;
  array[n_records] int<lower=1, upper=4> educ_record;
  array[n_records] int<lower=0> tpp_record;
}

parameters {
  real epsilon;
  array[4] real b_age;
  array[4] real b_educ;
  array[n_divisions] real<lower=0, upper=100> tpp_div_curr;
}

transformed parameters {
  array[2] real b_sex = {epsilon, -epsilon};
  array[n_records] real eta;
  
  // Offsets
  array[4] real o_age = {-2, -1, 1, 2};
  array[4] real o_educ = {-2, -1, 1, 2};
  
  for (n in 1:n_records) {
    int d = record_division[n];
    int a = age_record[n];
    int s = sex_record[n];
    int u = educ_record[n];
    
    eta[n] = inv_logit(tpp_div_curr[d]/100) + b_age[a] + o_age[a] + b_sex[s] + b_educ[u] + o_educ[u];
  }
}

model {
  for (n in 1:n_records) {
    int d = record_division[n];
    tpp_div_curr[d] ~ normal(tpp_div_prev[d], 5);
    tpp_record[n] ~ bernoulli_logit(eta[n]);
  }
  
  epsilon ~ normal(0, 1); 
  b_age ~ normal(0, 2); 
  b_educ ~ normal(0, 2); 
}
