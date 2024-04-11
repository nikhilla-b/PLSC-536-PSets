


# Calling Libraries -------------------------------------------------------

library(tidyverse)
library(haven)
library(modelsummary)
library(fixest)


# 1.1.1 -------------------------------------------------------------------

miguel_africa <- read_dta("data/miguel_africa.dta")

iv_fixest <-
  feols(any_prio ~ polity2l | gdp_g_l ~ GPCP_g_l #first stage
        , miguel_africa)

summary(iv_fixest)


# 1.1.2 -------------------------------------------------------------------

ols <- lm(any_prio ~ polity2l + gdp_g_l,
          miguel_africa)

summary(ols)

modelsummary(
  list(IV  = iv_fixest,
       OLS = ols),
  coef_map = c(
    `(Intercept)` = "Intercept",
    gdp_g_l = "Lagged Economic Growth",
    fit_gdp_g_l = "Lagged Economic Growth",
    polity2l = "Democracy (Polity IV)"
  ),
  gof_map = c("nobs")
)


# 2.1 ---------------------------------------------------------------------

scheve <- read_dta("data/ScheveStasavage_repdata.dta")

scheve |>
  ggplot(aes(x = year, y = topitaxrate2, group = ccode)) +
  geom_line(size = 0.2) +
  annotate(
    "rect",
    xmin = 1915,
    xmax = 1918,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightblue",
    alpha = 0.5
  ) +
  annotate(
    geom = "rect",
    xmin = 1941,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightblue",
    alpha = 0.5
  ) +
  labs(x = "Year", y = "Top Marginal Tax Rate (%)",
       title = "Top Marginal Inheritance Tax Rate Over Time") +
  theme_minimal()


# 2.2 ---------------------------------------------------------------------

countrywise <-
  scheve |>
  ggplot(aes(x = year, y = topitaxrate2, group = country)) +
  geom_line() +
  geom_line(
    data = select(scheve, -country),
    aes(group = ccode),
    alpha = 0.2,
    size = 0.2
  ) +
  facet_wrap(~ country, nrow = 4) +
  annotate(
    geom = "rect",
    xmin = 1915,
    xmax = 1918,
    ymin = -Inf,
    ymax = Inf,
    fill = "skyblue",
    alpha = 0.5
  ) +
  annotate(
    geom = "rect",
    xmin = 1941,
    xmax = 1945,
    ymin = -Inf,
    ymax = Inf,
    fill = "skyblue",
    alpha = 0.5
  ) +
  labs(x = "Year", y = "Inheritance Tax Rate",
       title = "Country wise Top Marginal Inheritance Tax Rate Over Time")

ggsave("countrywise.png", width = 9, height = 4)

# 3.2 ---------------------------------------------------------------------

scheve_sub <-
  scheve |>
  filter(country %in% c("UK", "Netherlands"), !year %in% c(1915:1948)) |>
  mutate(
    pre_post = case_when(year <= 1914 ~ "Pre-war",
                         year >= 1949 ~ "Post-war"),
    pre_post = fct_relevel(pre_post, "Pre-war")
  )

avg <-
  scheve_sub |>
  group_by(pre_post, country) |>
  summarise(avg = mean((topitaxrate2)))

avg |>
  pivot_wider(id_cols = country,
              names_from = pre_post,
              values_from = avg)

# 3.3 ---------------------------------------------------------------------

fit_did <- lm(topitaxrate2 ~ pre_post * country, scheve_sub)

summary(fit_did)
