
library(dplyr)
library(ggplot2)
library(purrr)
CONSTS <- config::get()
PROBS <- c(0.025, 0.25, 0.5, 0.75, 0.975)
options(readr.show_col_types = FALSE)

# Load --------------------------------------------------------------------

# Need the inputs again for plots
polls_national <- readr::read_csv("data/polls_national.csv") |> 
  filter(
    date >= CONSTS$walk_start, date <= CONSTS$walk_end,
    party == "tpp"
  )

results_national <- readr::read_csv("data/results_national.csv") |> 
  filter(year %in% 2016:2019, party=="tpp")

results_by_division <- readr::read_csv("data/tpp_by_division.csv") |> 
  tidyr::pivot_wider(names_from = year, values_from = tpp) |> 
  transmute(division, tpp = `2019`, tpp_prev = `2016`)

# And the outputs
tpp_walk <- readRDS("outputs/tpp_walk.Rds")
poll_bias <- readRDS("outputs/poll_bias.Rds")
ps_est <- readRDS("outputs/estimates.Rds")

# TPP Walk ---------------------------------------------------------------------

# First, just polls:
p <- ggplot() +
  geom_point(aes(x=date, y=poll, colour=pollster), data=polls_national) +
  geom_point(aes(x=date, y=vote), data=results_national) +
  theme_light() +
  ggtitle("TPP (ALP) polls (coloured) and results (black)")
p
ggsave("plots/polls.png")

# Now add the modelled latent intention:
tmp <- tpp_walk |> 
  apply(2, quantile, probs = PROBS) |> 
  t() |> 
  as_tibble() |> 
  mutate(
    week = row_number(),
    date = as.Date(CONSTS$walk_start) + week*7
  )

p + 
  geom_line(aes(x = date, y = `50%`), data = tmp) +
  geom_ribbon(aes(x = date, ymin = `2.5%`, ymax = `97.5%`), alpha = 0.3, data=tmp) +
  geom_ribbon(aes(x = date, ymin = `25%`, ymax = `75%`), alpha = 0.5, data=tmp) +
  ggtitle(
    "TPP (ALP) polls (coloured) and results (black)",
    "Latent intention (line) with central 50% (dark) and 95% (light) intervals"
  ) + 
  xlab("") + ylab("")
ggsave("plots/tpp_walk.png")


# House effects ----------------------------------------------------------------
poll_bias |> 
  as_tibble() |> 
  tidyr::pivot_longer(everything(), names_to = "pollster", values_to = "bias") |> 
  ggplot() +
  geom_boxplot(aes(pollster, bias, fill=pollster)) +
  theme_classic() + theme(axis.text.x = element_blank()) +
  xlab("") + ylab("") +
  ggtitle("Pollster bias")
ggsave("plots/tpp_bias.png")

# Poststratification -----------------------------------------------------------

# First we'll do histogram plots. These look way too crammed if we do more than
# 25 divisions per plot, so we'll split them up into pages:
pages <- lapply(0:5*25, \(x) 1:25 + x)
purrr::iwalk(pages, function(pg, i) {
  ps_est |> 
    left_join(results_by_division, "division") |> 
    filter(as.numeric(factor(division)) %in% pg) |> 
    group_by(division) |>
    ggplot() +
    ggdist::stat_histinterval(aes(x = est, y = division), size=1) +
    geom_point(aes(x=tpp, y=division), colour = "blue", size=1) + 
    geom_point(aes(x=tpp_prev, y=division), colour = "green", size=.75) + 
    geom_vline(xintercept = 50, linetype=2) +
    theme_classic() +
    ggtitle(
      "TPP (ALP) estimates by division", 
      "Actual result (blue) and previous result (green) are also shown"
    )
  ggsave(paste0("plots/estimates_page", i, ".png"), height=10)
}) 


# Next we'll do a simpler plot to get all the divisions on one page.
ordered2 <- function(x, by) ordered(x, x[order(by)])

p <- c(0.025, 0.25, 0.5, 0.75, 0.975)
p_names <- map_chr(c(0.025, 0.25, 0.5, 0.75, 0.975), ~paste0(.x*100, "%"))
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>% set_names(nm = p_names)

ps_sum <- ps_est |> 
  group_by(division) |> 
  summarise(across(est, p_funs)) |> 
  left_join(results_by_division, "division") |> 
  # left_join(lina |> group_by(division) |> summarise(raw = sum(tpp_imputed=="ALP")/n())) |> 
  mutate(division = ordered2(division, `est_50%`))

ps_sum |> 
  ggplot(aes(y=division, x = `est_50%`)) +
  geom_point(size=1) + 
  geom_linerange(aes(xmin=`est_2.5%`, xmax = `est_97.5%`), colour = "darkgrey") +
  geom_linerange(aes(xmin=`est_25%`, xmax = `est_75%`)) +
  geom_point(aes(x=tpp, y = division), colour = "blue", size=1) + 
  geom_point(aes(x=tpp_prev, y = division), colour = "green", size=1) + 
  # geom_point(aes(x=raw*100, y = division), colour = "red", size=1) + 
  geom_vline(xintercept = 50, linetype=2) +
  ggtitle(
    "TPP (ALP) posterior median and central 50% (black) and 95% (grey) intervals", 
    "Actual result (blue) and previous result (green) are also shown"
  ) + 
  xlab("") + ylab("") + 
  theme_classic() + theme(axis.text.y = element_text(size = 5))
ggsave("plots/estimates.png", height = 10)

