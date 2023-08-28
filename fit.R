
library(rstan)
library(dplyr)
CONSTS <- config::get()
options(mc.cores=parallel::detectCores())
options(readr.show_col_types = FALSE)

# Load -------------------------------------------------------------------------
polls_national <- readr::read_csv("data/polls_national.csv")
results_national <- readr::read_csv("data/results_national.csv") 

lina <- readr::read_csv("data/lina.csv")
ps <- readr::read_csv("data/poststratification_table.csv")
results_by_division <- readr::read_csv("data/tpp_by_division.csv") 

# Reconcile --------------------------------------------------------------------
# We are going to focus on the 2019 election, because that's what we have 
# record-level data on. So we'll drop off the rest of the data where applicable
results_by_division <- results_by_division |> 
  tidyr::pivot_wider(names_from = year, values_from = tpp) |> 
  transmute(division, tpp = `2019`, tpp_prev = `2016`)

polls_national <- polls_national |> 
  filter(date >= CONSTS$walk_start, date <= CONSTS$walk_end) |> 
  filter(party=="tpp")
  
results_national <- results_national |> 
  filter(year %in% c(2016, 2019)) |> 
  filter(party=="tpp")

# Can only model divisions that appear in the data
ps <- ps |> filter(division %in% lina$division)
results_by_division <- results_by_division |> filter(division %in% lina$division)

# Prepare data for stan ------------------------------------------------------------

# Convert to factor and ensuring that levels match:
lina <- lina |> 
  mutate(across(c(division, education, gender, age_group), factor))

for (var in c('division', 'education', 'gender', 'age_group')) {
  ps[[var]] <- factor(ps[[var]], levels(lina[[var]]))
}

# Collapse pollsters with < 10 obs over the entire period
polls_national <- polls_national |> 
  group_by(pollster) |> 
  mutate(pollster = ifelse(n() < 10, "Other", pollster))

# Merge results with polls, effectively treating the election as a special poll
df_national <- results_national |> 
  filter(year==2016) |> 
  mutate(pollster = "Election") |> 
  rename(obs=vote) |> 
  select(-year) |> 
  bind_rows(polls_national |> rename(obs=poll)) |> 
  select(date, pollster, obs) |> 
  arrange(date)
df_national$pollster <- factor(df_national$pollster)
df_national$pollster <- forcats::fct_relevel(df_national$pollster, "Election", after=Inf)
df_national$week <- ceiling(as.numeric((df_national$date - min(df_national$date))/7))+1

# Fit --------------------------------------------------------------------------

# Create dummies:
X <- model.matrix(~ 0 + gender + age_group + education, data=lina)

# Now create a list to pass to stan
standat <- list(
  # Poll-of-polls
  pp_T =  max(df_national$week),
  pp_N = nrow(df_national),
  pp_P = nlevels(df_national$pollster)-1, # Treating election as special pollster,
  pp_tpp0 = results_national$vote[1],
  pp_tpp_prev = results_national$vote[nrow(results_national)-1],
  pp_obs = df_national$obs,
  pp_t = df_national$week,
  pp_p = as.integer(df_national$pollster),
  
  # MRP
  mrp_N = nrow(X),
  mrp_K = ncol(X),
  mrp_D = nlevels(lina$division),
  mrp_d = as.numeric(lina$division),
  mrp_x = X,
  mrp_tpp_prev = results_by_division$tpp_prev,
  mrp_vote = (lina$tpp_imputed=="ALP")*1
)

# Fit and check diagnostics
fit <- stan("models/model.stan", data = standat, iter = 5000)

rstan::check_hmc_diagnostics(fit)
rstan::stan_ess(fit) 
rstan::stan_rhat(fit)

# Extract results --------------------------------------------------------------

tpp_walk <- rstan::extract(fit, "pp_intention")[[1]]
poll_bias <- rstan::extract(fit, "pp_poll_bias")[[1]]
colnames(poll_bias) <- levels(df_national$pollster)[1:ncol(poll_bias)]

# Extract coefficients
beta_draws <- rstan::extract(fit, "mrp_beta")[[1]]
alpha_draws <-  rstan::extract(fit, "mrp_tpp_curr")[[1]]

# Post-stratify
ps_x <- model.matrix(~ 0 + gender + age_group + education, data=ps)
ps_theta <- ps_x %*% t(beta_draws) + t(alpha_draws)[as.numeric(ps$division), ]
colnames(ps_theta) <- paste0("est[", 1:ncol(ps_theta), "]")

ps_est <- cbind(ps, ps_theta) |> 
  group_by(division) |> 
  summarise(across(starts_with("est["), ~sum(.*number)/sum(number))) |> 
  tidyr::pivot_longer(starts_with("est["), names_to = "rep", values_to = "est") |> 
  mutate(rep = stringr::str_extract(rep, "\\d+"))


# Save -------------------------------------------------------------------------

# Results
saveRDS(tpp_walk, "outputs/tpp_walk.Rds")
saveRDS(poll_bias, "outputs/poll_bias.Rds")
saveRDS(ps_est, "outputs/estimates.Rds")


