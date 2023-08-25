library(ggplot2)
library(rstan)
library(dplyr)

# Load --------------------------------------------------------------------

polls <- readr::read_csv("data/polls.csv")
results <- readr::read_csv("data/results.csv") 

# Ignore anything before 1990
polls <- polls[polls$date > "1990-01-01", ]
results <- results[results$date > "1990-01-01", ]

# Collapse pollsters with < 50 obs over the entire period
polls <- polls |> 
  group_by(pollster) |> 
  mutate(pollster = ifelse(n() < 50, "Other", pollster))

# Examine raw ------------------------------------------------------------------

ggplot() +
  geom_point(aes(x=date, y = poll, colour=pollster), data=polls) +
  geom_point(aes(x=date, y=vote), data=results) +
  facet_wrap(~party) +
  theme_light() +
  ggtitle("Federal election polls and results", "(Results are coloured black)")

ggsave("plots/polls.png")

# TPP --------------------------------------------------------------------------
# See models/tpp.stan for a description of the model.

# Focus on 2019 cycle
start_date <- as.Date("2016-08-01")
end_date <- as.Date("2019-05-18")

results$pollster <- "Election"
df <- results |> 
  rename(obs=vote) |> 
  select(-year) |> 
  bind_rows(polls |> rename(obs=poll)) |> 
  filter(party=="tpp") |> 
  select(date, pollster, obs) |> 
  arrange(date) |> 
  filter(date >= start_date, date <= end_date)
df$pollster <- factor(df$pollster)
df$pollster <- forcats::fct_relevel(df$pollster, "Election", after=Inf)
df$week <- ceiling(as.numeric((df$date - as.Date(start_date))/7) )

standat <- list(
  intention0 = df$obs[1], # First obs is an election
  n_steps = max(df$week),
  n_obs = nrow(df),
  n_pollsters = nlevels(df$pollster)-1, # Treating election as special pollster
  obs = df$obs,
  obs_pollster = as.integer(df$pollster),
  obs_step = df$week
)

fit_tpp <- stan("models/tpp.stan", data = standat)

rstan::check_hmc_diagnostics(fit_tpp)

rstan::stan_ess(fit_tpp)
rstan::stan_rhat(fit_tpp)
rstan::stan_mcse(fit_tpp, "intention")
rstan::stan_trace(fit_tpp, "intention[100]")

bayesplot::mcmc_pairs(fit_tpp, regex_pars = "poll_bias")

# The latent intention looks like:
x <- rstan::extract(fit_tpp, "intention")[[1]]
x |> 
  apply(2, quantile, probs = c(0.025, 0.5, 0.975)) |> 
  t() |> 
  as_tibble() |> 
  mutate(
    week = row_number(),
    date = start_date + week*7
  ) |> 
  ggplot(aes(x=date)) +
  geom_line(aes(y = `50%`)) +
  geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3) +
  geom_point(aes(y=obs, colour = pollster), data=df) +
  ggtitle("TPP (Labor)") + ylab("") + theme_classic()

ggsave("plots/tpp.png")

# The house effects look like
x <- rstan::extract(fit_tpp, "poll_bias")[[1]]
x |> 
  as_tibble() |> 
  setNames(levels(df$pollster)[1:6]) |> 
  tidyr::pivot_longer(everything(), names_to = "pollster", values_to = "bias") |> 
  ggplot() +
  geom_violin(aes(pollster, bias, fill=pollster)) +
  theme_classic() + theme(axis.text.x = element_blank()) 

ggsave("plots/tpp-bias.png")




