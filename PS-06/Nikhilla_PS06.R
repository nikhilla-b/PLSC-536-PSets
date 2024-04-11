
# Calling libraries -------------------------------------------------------

library(tidyverse)
library(haven)
library(modelsummary)
library(panelView)
library(fixest)
library(broom)
library(glue)
library(flextable)
library(scales)


# 1.1 ---------------------------------------------------------------------

annual <- read_dta("data/ScheveStasavage_annual.dta")

panelview(
  1 ~ war2p,
  #is it 1 ~ war2p or topitax ~ war2p
  data = annual,
  index = c("country", "year"),
  axis.lab.gap = c(10, 0),
  xlab = "Year",
  ylab = "Countries",
  main = "Treatment Status of War Mobilization"
)

panelview(
  1 ~ unisuffrage,
  data = annual,
  index = c("country", "year"),
  axis.lab.gap = c(10, 0),
  xlab = "Year",
  ylab = "Countries",
  main = "Treatment Status of Universal Male Suffrage"
)

panelview(
  1 ~ democracy,
  data = annual,
  index = c("country", "year"),
  axis.lab.gap = c(10, 0),
  xlab = "Year",
  ylab = "Countries",
  main = "Treatment Status of Competitive Elections"
)

# 2.1 ---------------------------------------------------------------------

five_year <- read_dta("data/ScheveStasavage_5year.dta")

five_year <-
  five_year |>
  mutate(trend_JAP =
           case_when(
             country == "Japan" &
               !is.na(halfdecade) ~ rank(halfdecade),
             TRUE ~ 0
           ))

five_year <-
  five_year |>
  mutate(trend_NZ =
           case_when(
             country == "New Zealand" &
               !is.na(halfdecade) ~ rank(halfdecade),
             TRUE ~ 0
           ))

# 2.2 ---------------------------------------------------------------------

five_year <-
  five_year |>
  arrange(country, year)

five_year <-
  five_year |>
  group_by(country) |>
  mutate(
    lagged_war_mobilization = lag(war2p, 1),
    lagged_universal_male_suffrage = lag(unisuffrage, 1),
    lagged_competitive_elections = lag(leftexec, 1),
    lagged_gdp = lag(rgdppc, 1)
  )

trend <- colnames(five_year) |>
  str_subset("trend_")

column_1 <-
  feols(
    topitax ~ lagged_war_mobilization + lagged_universal_male_suffrage |
      country + year,
    five_year,
    ssc = ssc(fixef.K = "full")
  )

column_2 <-
  feols(
    topitax ~ lagged_war_mobilization + lagged_universal_male_suffrage + lagged_competitive_elections + lagged_gdp |
      country + year,
    five_year,
    ssc = ssc(fixef.K = "full")
  )

formula_string_3 <-
  glue(
    "topitax ~ {str_c(trend, collapse = ' + ')} + lagged_gdp +
                             lagged_war_mobilization + lagged_universal_male_suffrage + lagged_competitive_elections |
                             country + year"
  )


formula <- as.formula(formula_string_3)

column_3 <-
  feols(formula, data = five_year, ssc = ssc(fixef.K = "full"))


models <- list(column_1, column_2, column_3)


modelsummary(
  models,
  
  coef_omit = "^trend_",
  gof_map = c("nobs", "FE: country", "FE: year"),
  coef_rename = c(
    "lagged_war_mobilization" = "War mobilisation t-1",
    "lagged_universal_male_suffrage" = "Universal male suffrage t-1",
    "lagged_competitive_elections" = "Left executive t-1",
    "lagged_gdp" = "GDP per capita t-1"
  ),
  
)

# 3 -----------------------------------------------------------------------


five_year <-
  five_year |>
  group_by(country) |>
  arrange(year) |> 
  mutate(
    leftexec_lag = lag(leftexec, 1),
    rgdppc_lag = lag(rgdppc, 1),
    war_lag1 = lag(war2p, 1),
    war_lag2 = lag(war2p, 2),
    war_lag3 = lag(war2p, 3),
    suf_lag = lag(unisuffrage, 1),
    war = war2p,
    war_lead1 = lead(war2p, 1),
    war_lead2 = lead(war2p, 2),
    war_lead3 = lead(war2p, 3)
  )


fit <- feols(topitax ~ war_lead1 + war_lead2 + war_lead3 + war +
               war_lag1 + war_lag2 + war_lag3  + leftexec_lag +
               rgdppc_lag + suf_lag | country + year, five_year)

fit |>
  tidy() |>
  filter(!term %in% c("leftexec_lag", "rgdppc_lag", "suf_lag")) |>
  mutate(term = recode_factor(
    term,
    war_lead3 = -15,
    war_lead2 = -10,
    war_lead1 = -5,
    war = 0,
    war_lag1 = 5,
    war_lag2 = 10,
    war_lag3 = 15
)) |> 
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error, ymax = estimate + 2 *
                      std.error),
                width = 0) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  labs(y = "Effects of War Mobilization on Inheritance Tax Rate",
       x = "Years since war was mobilised")
