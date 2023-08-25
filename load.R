
library(dplyr)
library(ggplot2)

# Consts -----------------------------------------------------------------------

POLLS_URL <- "https://raw.githubusercontent.com/d-j-hirst/aus-polling-analyser/master/analysis/Data/poll-data-fed.csv"
RESULTS_URL <- "https://raw.githubusercontent.com/d-j-hirst/aus-polling-analyser/master/analysis/Data/eventual-results.csv"

# Load --------------------------------------------------------------------
polls <- read.csv(POLLS_URL)
results <- read.csv(
    RESULTS_URL,
    header = FALSE,
    col.names = c("year", "state", "party", "vote")
  )

# Vic elections:
elections <- data.frame(
  state = "fed",
  year = c("1984","1987*","1990", "1993","1996","1998","2001","2004","2007", "2010","2013","2016*","2019","2022"),
  date = c("1 December 1984","11 July 1987","24 March 1990","13 March 1993","2 March 1996","3 October 1998","10 November 2001","9 October 2004","24 November 2007","21 August 2010","7 September 2013","2 July 2016","18 May 2019","21 May 2022")
)
elections$date <- as.Date(elections$date, format = "%d %B %Y")
elections$year <- lubridate::year(elections$date)

# Tidy --------------------------------------------------------------------
polls <- polls |> 
  mutate(across(X.TPP:OTH.FP, as.numeric)) |> 
  mutate(across(X.TPP:OTH.FP, ~ifelse(is.na(.), 0, .))) |> 
  transmute(
    date = as.Date(MidDate),
    pollster = Firm,
    tpp = X.TPP,
    lnp = LNP.FP,
    alp = ALP.FP,
    grn = GRN.FP,
    oth = ONP.FP + NXT.FP + UAP.FP + DEM.FP + DLP.FP + OTH.FP
  ) |> 
  tidyr::pivot_longer(tpp:oth, names_to = "party", values_to = "poll") |> 
  filter(poll > 0)

results <- results |> 
  filter(state=="fed") |> 
  mutate(
    party = gsub("@", "", party),
    party = gsub(" FP", "", party),
    party = tolower(party),
    party = ifelse(
      party %in% c("tpp", "lnp", "alp", "grn"),
      party,
      "oth"
    )
  ) |> 
  mutate_if(is.character, trimws) |> 
  mutate(year = as.integer(year)) |> 
  group_by(year, state, party) |> 
  summarise(vote = sum(vote)) |>
  ungroup() |> 
  left_join(elections, c("year", "state"))

# Save --------------------------------------------------------------------

write.csv(polls, "data/polls.csv", row.names = FALSE)
write.csv(results, "data/results.csv", row.names = FALSE)
