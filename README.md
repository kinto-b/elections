# Combining MRP with poll aggregation.

## Background

We have set of polls reported at the national level. We consider the polls to be observations of latent party support in the electorate, subject to some systematic bias specific the polling firm. We assume that the latent party support evolves dynamically as a random walk. Thus, in pseudo-code, we have

```
t = time[i]
f = firm[i]

poll[i] ~ N(support[t] + bias[f], err_poll)
support[t] ~ N(support[t-1], err_walk)
bias[f] ~ N(0, err_bias)
```

where `i` is the index of the poll observation.

We also have record-level data from a single poll, which we can use to produce electorate level estimates of party support using MRP. Thus, letting `vote` be a binary variable,

```
# Record-level model
vote[i] ~ Bernoulli(pi[i])
pi[i] ~ N(alpha + x[i] * beta, err_pi)

# Electorate level estimates:
support[e] = (sum s: n[s] * x[s] * beta) / (sum s: n[s])
```

where `i` is the index of the unit observation, `e` the index of the electorate, and `s` the index of the (sub-electorate) stratum.

Hence we have, on the one hand, an estimate of the national party support and, on the other, estimates of the electorate level support. We would like to strengthen the MRP estimates by including our estimate of national party support. 

We will do this by combining the two models, using the output from the poll-of-polls as our prior estimate for party support in each of the electorates. 


Thus, if the poll for which we have detailed data occurred at time `t'`, we have

```
vote[i] ~ Bernoulli(pi[i])
pi[i] ~ N(support[t'] + x[i] * beta, err_pi)
```

### Two-party-preferred or first-preferences

Strictly speaking, the model described above is only appropriate for modelling two-party elections or for two-party-preferred (TPP) support. To properly handle first-preference (FP) support in elections with more than two parties we would need to introduce a simplex constraint on the poll-of-polls model and use a multinomial generative model for the MRP step.

We will ignore these issues to begin with, using the TPP polling figures for the poll-of-polls and collapsing the record-level voting intention data to two categories.


## References

For the poll-of-polls step, our data is borrowed from the excellent [Australian Electoral Forecasts project](https://github.com/d-j-hirst/aus-polling-analyser). 

For the MRP step, our  poststratification table is derived from the ABS Census and the polling data is taken from a Life in Australia (LinA) panel conducted by the Social Research Centre. We take pre-processed versions of each from [Alexander et al.](https://github.com/RohanAlexander/ForecastingMultiDistrictElections). 

We note that the LinA panel was conducted between 08/04/2019 and 26/04/2019. It is not included in the polling data that we used to produce the poll-of-polls.

