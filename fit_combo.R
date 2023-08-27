library(rstan)
library(dplyr)
library(ggplot2)
library(purrr)

# Load --------------------------------------------------------------------

pollaggdat <- readRDS("interim/tpp_standat.Rds")

# Convert to factor, ensuring levels are the same
lina <- read.csv("data/lina.csv") |> 
  mutate(across(c(division, education, gender, age_group), factor))

ps <- read.csv("data/poststratification_table.csv")

for (var in c('division', 'education', 'gender', 'age_group')) {
  ps[[var]] <- factor(ps[[var]], levels(lina[[var]]))
}

# Drop any marginal poststratification cells which don't appear in the data
ps <- ps |> filter_all(~!is.na(.))

# Prior results
past <- read.csv("data/past_results.csv") |> 
  filter(year==2016) |> 
  transmute(
    division = factor(division, levels(lina$division)),
    tpp_prev = 100 * ALP_2PP
  ) |> 
  filter(!is.na(division)) |> 
  arrange(as.numeric(division))

# True results
results <- read.csv("https://results.aec.gov.au/24310/Website/Downloads/HouseTppByDivisionDownload-24310.csv", skip = 1)
results <- results |> 
  transmute(
    division = factor(DivisionNm, levels(lina$division)),
    tpp = Australian.Labor.Party.Percentage
  ) |> filter(!is.na(division))

# Fit --------------------------------------------------------------------------

# Create dummies:
m <- model.matrix(~ gender + age_group + education, data=lina)
m[1:5, 1:5]

standat <- list(
  # Poll-of-polls
  pp_T = pollaggdat$n_steps,
  pp_N = pollaggdat$n_obs,
  pp_P = pollaggdat$n_pollsters,
  pp_tpp0 = pollaggdat$intention0,
  pp_tpp_prev = pollaggdat$obs[which(pollaggdat$obs_pollster==12)[2]],
  pp_obs = pollaggdat$obs,
  pp_t = pollaggdat$obs_step,
  pp_p = pollaggdat$obs_pollster,
  
  # MRP
  mrp_N = nrow(m),
  mrp_K = ncol(m),
  mrp_D = nlevels(lina$division),
  mrp_d = as.numeric(lina$division),
  mrp_x = m,
  mrp_tpp_prev = past$tpp_prev,
  mrp_vote = (lina$tpp_imputed=="ALP")*1
)

fit <- stan("models/model.stan", data = standat)

rstan::check_hmc_diagnostics(fit)
rstan::stan_ess(fit) 
rstan::stan_rhat(fit)


# -------------------------------------------------------------------------

# Extract coefficients
beta_draws <- fit |> posterior::as_draws_df() |> select(starts_with("mrp_beta["))
alpha_draws <- fit |> posterior::as_draws_df() |> select(starts_with("mrp_tpp_curr["))

# Post-stratify
ps_x <- model.matrix(~ gender + age_group + education, data=ps)
ps_theta <- ps_x %*% t(beta_draws) + t(alpha_draws)[as.numeric(ps$division), ]
colnames(ps_theta) <- paste0("theta[", 1:ncol(ps_theta), "]")

ps_est <- cbind(ps, ps_theta) |> 
  group_by(division) |> 
  summarise(across(starts_with("theta["), ~sum(.*number)/sum(number))) |> 
  tidyr::pivot_longer(starts_with("theta["), names_to = "rep", values_to = "est") |> 
  mutate(rep = stringr::str_extract(rep, "\\d+"))
write.csv(ps_est, "data/estimates.csv", row.names = FALSE)

# Separate hist plots into sheets to make them manageable
sheets <- lapply(0:5*25, \(x) 1:25 + x)
purrr::iwalk(sheets, function(sh, i) {
  ps_est |> 
    left_join(results, "division") |> 
    filter(as.numeric(division) %in% sh) |> 
    group_by(division) |>
    ggplot() +
    ggdist::stat_histinterval(aes(x = est, y = division), size=1) +
    geom_point(aes(x=tpp, y = division), colour = "blue", size=1) + 
    geom_vline(xintercept = 50, linetype=2) +
    theme_classic() 
  ggsave(paste0("plots/estimates_sheet", i, ".png"), height=10)
}) 


# Simpler line plot
ordered2 <- function(x, by) ordered(x, x[order(by)])
p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
p_names <- map_chr(c(0.025, 0.25, 0.5, 0.75, 0.975), ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)

ps_sum <- ps_est |> 
  group_by(division) |> 
  summarise(across(est, p_funs)) |> 
  left_join(results, "division") |> 
  left_join(past, "division") |> 
  mutate(division = ordered2(division, `est_50%`))

ps_sum |> 
  ggplot(aes(y=division, x = `est_50%`)) +
  geom_point(size=1) + 
  geom_linerange(aes(xmin=`est_2.5%`, xmax = `est_97.5%`), colour = "darkgrey") +
  geom_linerange(aes(xmin=`est_25%`, xmax = `est_75%`)) +
  geom_point(aes(x=tpp, y = division), colour = "blue", size=1) + 
  geom_point(aes(x=tpp_prev, y = division), colour = "green", size=1) + 
  geom_vline(xintercept = 50, linetype=2) +
  ggtitle("TPP (ALP)", "Posterior median and central 50% (black) and 95% (grey) intervals") + 
  xlab("") + ylab("") + 
  theme_classic() + theme(axis.text.y = element_text(size = 5))



