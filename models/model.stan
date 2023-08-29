//
// We expect that the TPP figure for the upcoming election will be close to
// the TPP figure for the previous election. Swings are almost always <15 points,
// usually <10 points, and often <5 points.
//
// We therefore model
//
//   tpp_curr[d] = tpp_nat_prev[d] + swing[d]
//   swing[d] ~ N(swing_national, 5)
//
// We determine the national swing using our poll-of-polls. That is, roughly,
//
//   swing_national = tn_pollsational_curr - tn_pollsational_prev
//   tn_pollsational_curr ~ dynamic_model()
// 
// We take tpp_curr[d] as our estimate, all else being equal, of the probability
// that someone will support the ALP on a TPP basis. Hence we treat it as a 
// random intercept term.
//
// We also allow other demographic characteristics to impact someone's propensity
// to support the ALP on a TPP basis. Since these effects are likely to be small,
// and TPP figures are all in the range [20, 100], we can safely use a linear
// probability model.
//
//
//   vote[i] ~ Bernoulli(pi[i])
//   pi[i] = tpp_curr[division[i]] + x[i] * beta
//   beta ~ normal(0, 2)
//
// We can then use post-stratification to obtain adjusted estimates 
//
//
// Note that we treat elections as special polls in the dynamic model.

data {
  // Poll-of-polls.
  int<lower=0> n_timesteps; 
  int<lower=0> n_polls; 
  real<lower=0, upper=100> walk0; // start of random walk
  real<lower=0, upper=100> tpp_nat_prev;
  int<lower=1> n_pollsters; 
  array[n_polls] real<lower=0, upper=100> polls;
  array[n_polls] int<lower=1, upper=n_polls+1> poll_pollster; // map obs to pollster
  array[n_polls] int<lower=1, upper=n_timesteps> poll_timestep; // map obs to timestep
  
  // MRP
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
  // Poll-of-polls
  array[n_timesteps] real<lower=0, upper=100> walk;
  real<lower=0> walk_err;
  real<lower=0> poll_err;
  array[n_pollsters] real poll_bias;
  
  // MRP
  real epsilon;
  array[4] real b_age;
  array[4] real b_educ;
  array[n_divisions] real<lower=0, upper=100> tpp_div_curr;
}

transformed parameters {
  array[2] real b_sex = {epsilon, -epsilon};
  array[n_records] real<lower=0, upper=1> theta;
  for (n in 1:n_records) {
    int d = record_division[n];
    int a = age_record[n];
    int s = sex_record[n];
    int u = educ_record[n];
    
    theta[n] = (tpp_div_curr[d] + b_age[a] + b_sex[s] + b_educ[u])/100;
  }
}

model {
  // Poll of polls
  poll_err ~ gamma(3, 1);
  for (p in 1:n_pollsters) {
    poll_bias[p] ~ normal(0, 2);
  }

  walk_err ~ exponential(1);
  walk[1] ~ normal(walk0, walk_err);
  for (n in 2:n_timesteps) {
    walk[n] ~ normal(walk[n-1], walk_err);
  }

  for (n in 1:n_polls) {
    int t = poll_timestep[n];
    int j = poll_pollster[n];
    
    // Remember that elections are treated as a special poll
    if (j <= n_pollsters) {
      polls[n] ~ normal(walk[t] + poll_bias[j], poll_err);
    } else {
      polls[n] ~ normal(walk[t], 0.01); // election
    }
  }
  
  // MRP:
  for (n in 1:n_records) {
    int d = record_division[n];
    tpp_div_curr[d] ~ normal(tpp_div_prev[d], 5);
    tpp_record[n] ~ bernoulli(theta[n]);
  }
  
  epsilon ~ normal(0, 2); 
  b_age ~ normal(0, 5); 
  b_educ ~ normal(0, 5); 
}
