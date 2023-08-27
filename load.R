
library(dplyr)

# Consts -----------------------------------------------------------------------

MRP_URL <- "https://raw.githubusercontent.com/RohanAlexander/ForecastingMultiDistrictElections/master/outputs/data/"
PS_URL <- paste0(MRP_URL, "poststratification_data/census_data.csv")
LINA_URL <- paste0(MRP_URL, "regression_data/LinA.csv")
TPP_BY_DIV_to2016_URL <- paste0(MRP_URL, "elections/data_from_earlier_elections_on_2019_div_basis.csv")
TPP_BY_DIV_2019_URL <- "https://results.aec.gov.au/24310/Website/Downloads/HouseTppByDivisionDownload-24310.csv"

POLLOFPOLLS_URL <- "https://raw.githubusercontent.com/d-j-hirst/aus-polling-analyser/master/analysis/Data/"
POLLS_URL <- paste0(POLLOFPOLLS_URL, "poll-data-fed.csv")
RESULTS_URL <- paste0(POLLOFPOLLS_URL, "eventual-results.csv")


# Load -------------------------------------------------------------------------
df_lina <- read.csv(LINA_URL)
df_ps   <- read.csv(PS_URL)
df_tpp_by_div   <- read.csv(TPP_BY_DIV_to2016_URL) |> 
  transmute(year, division, tpp=100*ALP_2PP) |> 
  bind_rows(read.csv(TPP_BY_DIV_2019_URL, skip=1) |> 
      transmute(
        year = 2019,
        division = DivisionNm, 
        tpp = Australian.Labor.Party.Percentage
      ))

df_polls <- read.csv(POLLS_URL)
df_results <- read.csv(
  RESULTS_URL,
  header = FALSE,
  col.names = c("year", "state", "party", "vote")
)

# We'll create our own list of election dates by hand:
df_elections <- data.frame(
  state = "fed",
  year = c("1984","1987*","1990", "1993","1996","1998","2001","2004","2007", "2010","2013","2016*","2019","2022"),
  date = c("1 December 1984","11 July 1987","24 March 1990","13 March 1993","2 March 1996","3 October 1998","10 November 2001","9 October 2004","24 November 2007","21 August 2010","7 September 2013","2 July 2016","18 May 2019","21 May 2022")
)
df_elections$date <- as.Date(df_elections$date, format = "%d %B %Y")
df_elections$year <- lubridate::year(df_elections$date)

# Tidy --------------------------------------------------------------------
df_polls <- df_polls |> 
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

df_results <- df_results |> 
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
  left_join(df_elections, c("year", "state"))

# Impute ------------------------------------------------------------------
# We will impute our own TPP using a fairly naive method. We first randomly
# assign GRN voters at a ratio of 80/20 to ALP/LNP and everyone else at a ratio
# of 50/50 to ALP/LNP.
set.seed(2023-08-25)
tpp <- c("ALP", "LNP")

df_lina <- df_lina |> 
  mutate(
    tmp_grn = sample(tpp, nrow(df_lina), replace = TRUE, prob = c(8, 2)),
    tmp_oth = sample(tpp, nrow(df_lina), replace = TRUE),
    tpp_imputed = case_when(
      first_pref %in% tpp ~ first_pref,
      first_pref == "GRN" ~ tmp_grn,
      TRUE ~ tmp_oth
    )
  ) |> 
  select(-tmp_grn, -tmp_oth, -ALP_supporter) 


with(df_lina, table(first_pref, tpp_imputed) / nrow(df_lina)) |> round(2)
with(df_lina, table(tpp_imputed) / nrow(df_lina)) |> round(2)
with(df_lina, table(first_pref) / nrow(df_lina)) |> round(2)

# Save --------------------------------------------------------------------

write.csv(df_lina, "data/lina.csv", row.names = FALSE)
write.csv(df_ps, "data/poststratification_table.csv", row.names = FALSE)
write.csv(df_tpp_by_div, "data/tpp_by_division.csv", row.names = FALSE)

write.csv(df_polls, "data/polls_national.csv", row.names = FALSE)
write.csv(df_results, "data/results_national.csv", row.names = FALSE)
