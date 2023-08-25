// 
// We assume polls are measuring a latent 'voting intention' variable which
// is expressed with (near) perfect precision in the observed vote at each 
// election. We assume voting intention develops according to a random walk.
// 
// We furthermore assume that each pollster has its own bias. But, to simplify
// things, we assume that they all have the same variance.
//
// Hence our model is roughly,
//
//  poll[t, j] ~ N(intention + bias[j], poll_error)
//  vote[t] ~ N(intention, 0.01)
//  intention[t] ~ N(intention[t-1], intention_error)
//  
// It is doubtful that voting intention shifts dramatically from step to step,
// so we should restrict intention error to be somewhat small. Something
// like this, which has 50% of the distribution between 0.3 and 1.3 makes sense,
// 
// intention_error ~ Exp(1)
//
// I don't have any strong intuitions about poll error. But around two points
// seems reasonable. So perhaps
//
//  poll_error ~ Gamma(3, 1)
//
// 
// As for poll bias, it is very doubtful they would be more than 5 points off.
// Hence something like this is probably adequate: 
//
//  bias ~ N(0, 2)


data {
  int<lower=0> n_steps;
  int<lower=0> n_obs;
  real<lower=0, upper=100> intention0; // Start of random walk
  
  // We will treat the real election outcome as a special pollster, hence the +1
  int<lower=1> n_pollsters;
  array[n_obs] real<lower=0, upper=100> obs;
  array[n_obs] int<lower=1, upper=n_pollsters+1> obs_pollster; // Map obs to pollster
  array[n_obs] int<lower=1, upper=n_steps> obs_step; // Map obs to step
}

parameters {
  array[n_steps] real<lower=0, upper=100> intention;
  real<lower=0> intention_error;
  real<lower=0> poll_error;
  array[n_pollsters] real poll_bias;
}

model {
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
}
