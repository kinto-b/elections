// 
// Dynamic, national poll-aggregation component of model.stan.
//

data {
  int<lower=0> n_timesteps; 
  int<lower=0> n_polls; 
  real<lower=0, upper=100> walk0; // start of random walk
  real<lower=0, upper=100> tpp_nat_prev;
  int<lower=1> n_pollsters; 
  array[n_polls] real<lower=0, upper=100> polls;
  array[n_polls] int<lower=1, upper=n_polls+1> poll_pollster; // map obs to pollster
  array[n_polls] int<lower=1, upper=n_timesteps> poll_timestep; // map obs to timestep
}

parameters {
  // Poll-of-polls
  array[n_timesteps] real<lower=0, upper=100> walk;
  real<lower=0> walk_err;
  real<lower=0> poll_err;
  array[n_pollsters] real poll_bias;
}

model {
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
}
