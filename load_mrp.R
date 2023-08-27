
library(dplyr)

# Consts -----------------------------------------------------------------------

MRP_URL <- "https://raw.githubusercontent.com/RohanAlexander/ForecastingMultiDistrictElections/master/outputs/data/"
PS_URL <- paste0(MRP_URL, "poststratification_data/census_data.csv")
POLL_URL <- paste0(MRP_URL, "regression_data/LinA.csv")
RESULTS_URL <- paste0(MRP_URL, "elections/data_from_earlier_elections_on_2019_div_basis.csv")

# Load --------------------------------------------------------------------
df_poll <- read.csv(POLL_URL)
df_ps   <- read.csv(PS_URL)
df_res   <- read.csv(RESULTS_URL)

# Impute ------------------------------------------------------------------
# We will impute our own TPP using a fairly naive method. We first randomly
# assign GRN voters at a ratio of 80/20 to ALP/LNP and everyone else at a ratio
# of 50/50 to ALP/LNP.
set.seed(2023-08-25)
tpp <- c("ALP", "LNP")

df_poll <- df_poll |> 
  mutate(
    tmp_grn = sample(tpp, nrow(df_poll), replace = TRUE, prob = c(8, 2)),
    tmp_oth = sample(tpp, nrow(df_poll), replace = TRUE),
    tpp_imputed = case_when(
      first_pref %in% tpp ~ first_pref,
      first_pref == "GRN" ~ tmp_grn,
      TRUE ~ tmp_oth
    )
  ) |> 
  select(-tmp_grn, -tmp_oth, -ALP_supporter) 


with(df_poll, table(first_pref, tpp_imputed) / nrow(df_poll)) |> round(2)
with(df_poll, table(tpp_imputed) / nrow(df_poll)) |> round(2)
with(df_poll, table(first_pref) / nrow(df_poll)) |> round(2)

# Save --------------------------------------------------------------------

write.csv(df_poll, "data/lina.csv", row.names = FALSE)
write.csv(df_ps, "data/poststratification_table.csv", row.names = FALSE)
write.csv(df_res, "data/past_results.csv", row.names = FALSE)
