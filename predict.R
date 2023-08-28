
library(dplyr)
library(purrr)
options(readr.show_col_types = FALSE)


# Load -------------------------------------------------------------------------

ps_est <- readRDS("outputs/estimates.Rds")
results_by_division <- readr::read_csv("data/tpp_by_division.csv") |> 
  filter(year==2019) |> 
  select(division, tpp)

# Seat predictions -------------------------------------------------------------

## First, use similar approach to Rohan et al. ---------------------------------
cut_tpp <- function(x) {
  cut(
    x,
    c(0, 40, 43, 48, 50, 52, 57, 60, 100),
    c("Safe loss","Fairly safe loss","Marginal loss","Very marginal loss","Very marginal win", "Marginal win","Fairly safe win", "Safe win"),
    include.lowest = TRUE
  )
}

p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
p_names <- map_chr(c(0.025, 0.25, 0.5, 0.75, 0.975), ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)

ps_pred1 <- ps_est |> 
  group_by(division) |> 
  summarise(across(est, p_funs)) |> 
  left_join(results_by_division, "division") |> 
  mutate(
    prediction = case_when(
      `est_50%` < 40 ~ "Safe loss",
      `est_50%` < 43 | `est_97.5%` < 50 ~ "Fairly safe loss",
      `est_50%` < 48 | `est_75%` < 50 ~ "Marginal loss",
      `est_50%` > 60 ~ "Safe win",
      `est_50%` > 57 | `est_2.5%` > 50 ~ "Fairly safe win",
      `est_50%` > 52 | `est_25%` > 50 ~ "Marginal win",
      TRUE ~ "Too close to call"
    ),
    prediction = factor(
      prediction,
      c("Safe loss","Fairly safe loss","Marginal loss","Too close to call","Marginal win","Fairly safe win", "Safe win")
    ),
    result = cut_tpp(tpp)
  )


## Next try a riskier prediction approach --------------------------------------

cut_est <- function(x) {
  cut(
    x, 
    c(0, 1, 2.5, 10, 25, 40, 60, 75, 90, 97.5, 99, 100), 
    c("Very safe loss", "Safe loss", "Fairly safe loss", "Marginal loss", 
      "Very marginal loss", "Too close to call", "Very marginal win",
      "Marginal win", "Fairly safe win", "Safe win", "Very safe win"),
    include.lowest = TRUE
  )
}

ps_pred2 <- ps_est |> 
  group_by(division) |> 
  summarise(est = 100*sum(est > 50)/n()) |> 
  left_join(results_by_division, "division") |> 
  mutate(
    prediction = cut_est(est),
    result = cut_tpp(tpp)
  )


# Election predictions ---------------------------------------------------------
# Use draws to simulate elections:

set.seed(2023-08-28)

ps_sim <- ps_est
ps_sim$win <- rbinom(length(ps_est$est), 1, ps_est$est/100)
ps_sim <- ps_sim |> 
  group_by(rep = as.integer(rep)) |> 
  summarise(nseats = sum(win))

# Save --------------------------------------------------------------------

saveRDS(ps_pred1, "outputs/seat_predictionsv1.csv")
saveRDS(ps_pred2, "outputs/seat_predictionsv2.csv")
saveRDS(ps_sim, "outputs/election_simulations.csv")


