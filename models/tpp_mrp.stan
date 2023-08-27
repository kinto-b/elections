// 
// Poll-of-polls -> MRP 
//


data {
  // Poll-of-polls
  int<lower=0> n_steps;
  int<lower=0> n_obs;
  real<lower=0, upper=100> intention0; // Start of random walk
  int<lower=1> n_pollsters;
  array[n_obs] real<lower=0, upper=100> obs;
  array[n_obs] int<lower=1, upper=n_pollsters+1> obs_pollster; // Map obs to pollster
  array[n_obs] int<lower=1, upper=n_steps> obs_step; // Map obs to timestep
  
  // MRP
  int<lower=1> mrp_time; // timestep that detailed poll taken
  int<lower=0> N;
  int<lower=0> K; # number fixed effect predictors
  matrix[N, K] x; # fixed effect predictors
  array[N] int<lower=1, upper=150> division;
  array[N] int<lower=0> vote;
}

parameters {
  // Poll-of-polls
  array[n_steps] real<lower=0, upper=100> intention;
  real<lower=0> intention_error;
  real<lower=0> poll_error;
  array[n_pollsters] real poll_bias;
  
  // MRP
  vector[K] beta; // fixed effect coefs
  real<lower=0> sigma_delta;
  vector<multiplier=sigma_delta>[150] delta;
}

model {
  // Poll of polls
  poll_error ~ gamma(3, 1);
  for (p in 1:n_pollsters) {
    poll_bias[p] ~ normal(0, 2);
  }

  // Note that first observation in the random walk is an election.
  intention_error ~ exponential(1);
  intention[1] ~ normal(intention0, intention_error);
  for (n in 2:n_steps) {
    intention[n] ~ normal(intention[n-1], intention_error);
  }

  for (n in 1:n_obs) {
    int t = obs_step[n];
    int j = obs_pollster[n];
    
    // Remember that elections are treated as a special poll
    if (j <= n_pollsters) {
      obs[n] ~ normal(intention[t] + poll_bias[j], poll_error);
    } else {
      obs[n] ~ normal(intention[t], 0.01);
    }
  }
  
  // MRP:
  real alpha;
  alpha = log(intention[mrp_time]/(100-intention[mrp_time]));
  vote ~ bernoulli_logit(alpha + x*beta + delta[division]);
  beta ~ normal(0, 2); // Fixed effects shouldn't be huge
  delta ~ normal(0, sigma_delta);
  sigma_delta ~ normal(0, 1);
}
