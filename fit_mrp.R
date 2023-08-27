
library(rstan)
library(dplyr)
library(ggplot2)


# Load --------------------------------------------------------------------

# Convert to factor, ensuring levels are the same
lina <- read.csv("data/lina.csv") |> 
  mutate(across(c(division, education, gender, age_group), factor))

ps <- read.csv("data/poststratification_table.csv")

for (var in c('division', 'education', 'gender', 'age_group')) {
  ps[[var]] <- factor(ps[[var]], levels(lina[[var]]))
}

# Drop any marginal poststratification cells which don't appear in the data
ps <- ps |> filter_all(~!is.na(.))
#> Turns out this kills of an electorate, but so be it.

# Create post stratification array for Stan
ps_array <- ps |> 
  mutate(across(c(division, education, gender, age_group), as.integer)) |> 
  arrange(division, education, gender, age_group) |> 
  pull(number) |> 
  array(dim = c(4, 2, 4, 150))

standat <- list(
  N = nrow(lina),
  age = as.integer(lina$age_group),
  gender = as.integer(lina$gender),
  education = as.integer(lina$education),
  division = as.integer(lina$division),
  y = (lina$tpp_imputed == "ALP")*1,
  
  P = ps_array
)

# Fit ---------------------------------------------------------------------
#' We run into computational issues if we use:
#'   `tpp ~ (1|gender) + (1|age_group) + (1|education) + (1|division)`
#' 
#' If we only used a single random intercept term, we get better results: 
#'   `tpp ~ gender + age_group + education + (1|division)`
#' 
#' So we'll stick with the latter, even though this means we don't get pooling
#' across gender/age/education groups. 

fit_brms <- brms::brm(
  tpp_imputed ~ gender + age_group + education + (1|division),
  data = lina,
  family = brms::bernoulli(),
  cores = parallel::detectCores()
)

# Poststratify -----------------------------------------------------------------

# Summarise draws
pred <- cbind(
  ps,
  brms::posterior_epred(fit_brms, newdata = ps) |> 
    apply(2, quantile, probs = c(0.025, 0.25, 0.5, 0.75, 0.975)) |> 
    t()
)

# Plot group-level estimates
ordered2 <- function(x, by) ordered(x, x[order(by)])
tpp_plot <- function(pred, breakvar) {
  pred <- pred |> 
    as_tibble() |> 
    group_by_at(breakvar) |> 
    summarise(across(c(`2.5%`:`97.5%`), ~sum(.x*number) / sum(number)))
  
  breakvar <- rlang::sym(breakvar)
  pred |> 
    mutate(y = ordered2(!!breakvar, `50%`)) |> 
    ggplot(aes(y=y, x = `50%`)) +
    geom_point(size=1) + 
    geom_linerange(aes(xmin=`2.5%`, xmax = `97.5%`), colour = "darkgrey") +
    geom_linerange(aes(xmin=`25%`, xmax = `75%`)) +
    ggtitle("TPP (LNP)", "Posterior median and central 50% (black) and 95% (grey) intervals") + 
    xlab("") + ylab("") + theme_classic()
}

tpp_plot(pred, "division") + theme(axis.text.y = element_text(size = 5))
ggsave("plots/mrp_electorates.png", height = 10)

tpp_plot(pred, "gender")
ggsave("plots/mrp_gender.png")

tpp_plot(pred, "age_group")
ggsave("plots/mrp_age_group.png")

tpp_plot(pred, "education")
ggsave("plots/mrp_education.png")

