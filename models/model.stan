//
// We expect that the TPP figure for the upcoming election will be close to
// the TPP figure for the previous election. Swings are almost always <15 points,
// usually <10 points, and often <5 points.
//
// We therefore model
//
//   tpp_curr[d] = tpp_prev[d] + swing[d]
//   swing[d] ~ N(swing_national, 5)
//
// We determine the national swing using our poll-of-polls. That is, roughly,
//
//   swing_national = tpp_national_curr - tpp_national_prev
//   tpp_national_curr ~ dynamic_model()
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
  int<lower=0> pp_T; // number timesteps 
  int<lower=0> pp_N; // number observations
  real<lower=0, upper=100> pp_tpp0; // start of random walk
  real<lower=0, upper=100> pp_tpp_prev; // previous TPP
  int<lower=1> pp_P; // number of pollsters
  array[pp_N] real<lower=0, upper=100> pp_obs;
  array[pp_N] int<lower=1, upper=pp_N+1> pp_p; // map obs to pollster
  array[pp_N] int<lower=1, upper=pp_T> pp_t; // map obs to timestep
  
  // MRP
  int<lower=0> mrp_N;
  int<lower=0> mrp_K; # number fixed effect predictors
  int<lower=0> mrp_D; # number divisions
  array[mrp_N] int<lower=0, upper=mrp_D> mrp_d; # map obs to division
  array[mrp_D] real<lower=0, upper=100> mrp_tpp_prev;
  matrix[mrp_N, mrp_K] mrp_x; # fixed effect predictors
  array[mrp_N] int<lower=0> mrp_vote;
}

parameters {
  // Poll-of-polls
  array[pp_T] real<lower=0, upper=100> pp_intention;
  real<lower=0> pp_tpp_err;
  real<lower=0> pp_poll_err;
  array[pp_P] real pp_poll_bias;
  
  // MRP
  vector[mrp_K] mrp_beta; // fixed effect coefs
  array[mrp_D] real<lower=0, upper=100> mrp_tpp_curr;
}

transformed parameters {
  array[mrp_N] real<lower=0, upper=1> theta;
  for (n in 1:mrp_N) {
    int d = mrp_d[n];
    theta[n] = (mrp_tpp_curr[d] + mrp_x[n] * mrp_beta)/100;
  }
}

model {
  // Poll of polls
  pp_poll_err ~ gamma(3, 1);
  for (p in 1:pp_P) {
    pp_poll_bias[p] ~ normal(0, 2);
  }

  pp_tpp_err ~ exponential(1);
  pp_intention[1] ~ normal(pp_tpp0, pp_tpp_err);
  for (n in 2:pp_T) {
    pp_intention[n] ~ normal(pp_intention[n-1], pp_tpp_err);
  }

  for (n in 1:pp_N) {
    int t = pp_t[n];
    int j = pp_p[n];
    
    // Remember that elections are treated as a special poll
    if (j <= pp_P) {
      pp_obs[n] ~ normal(pp_intention[t] + pp_poll_bias[j], pp_poll_err);
    } else {
      pp_obs[n] ~ normal(pp_intention[t], 0.01); // election
    }
  }
  
  // MRP:
  for (n in 1:mrp_N) {
    int d = mrp_d[n];
    mrp_tpp_curr[d] ~ normal(mrp_tpp_prev[d] + pp_intention[pp_T] - pp_tpp_prev, 5);
    mrp_vote[n] ~ bernoulli(theta[n]);
  }
  
  mrp_beta ~ normal(0, 2);
}
