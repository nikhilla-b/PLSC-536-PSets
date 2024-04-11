

# Calling libraries -----------------------------------------------------------------------

library(tidyverse)
library(haven)
library(broom)
library(fixest)
library(fs)


# 1.1 ---------------------------------------------------------------------


filenames <- dir_ls("data/banerjee")

countries <- str_remove_all(filenames, "data/banerjee/|.dta")

names(filenames) <- countries

df_list <- map(filenames, read_dta) |>
  list_rbind(names_to = "countries")


# 1.2 ---------------------------------------------------------------------

itt_estimate <- function(x) {
  lm(asset_index_fup ~ assignment + asset_index_bsl,
     df_list |>
       filter(countries == x)) |>
    tidy()
}

results <-
  map(countries, itt_estimate) |>
  set_names(countries) |>
  list_rbind(names_to = "countries") |>
  filter(term == "assignment")

results |>
  ggplot() +
  geom_point(aes(x = countries,
                 y = estimate),
             size = 3) +
  labs(x = "Country",
       y = "ITT Estimate on Asset Index",
       title = "ITT Estimates on Asset Index by Country")

# 2 -----------------------------------------------------------------------

pooled_hh <- read_dta("data/pooled_hh_noncompliance.dta")

# 2.2 ---------------------------------------------------------------------

treatment_mean <-
  pooled_hh |>
  select(assignment, asset_hh_index_bsl) |>
  group_by(assignment) |>
  summarise(mean = mean(asset_hh_index_bsl))

uptake_mean <-
  pooled_hh |>
  group_by(assignment, uptake) |>
  summarise(mean = mean(asset_hh_index_bsl))


# 2.3 ---------------------------------------------------------------------

#1st stage

first_stage <- lm(uptake ~ assignment,
                  pooled_hh) |>
  tidy()

summary(first_stage)

#Reduced form

rf <- lm(asset_hh_index_fup ~ assignment,
         pooled_hh) |>
  tidy()


summary(rf)

rf$estimate[2] / first_stage$estimate[2]


# 2.4 ---------------------------------------------------------------------


iv_fixest <-
  feols(asset_hh_index_fup ~ 1 | uptake ~ assignment, pooled_hh) |>
  tidy()

summary(iv_fixest)


# 3.1 ---------------------------------------------------------------------

miguel_africa <- read_dta("data/miguel_africa.dta")

miguel_africa |>
  pull(country_name) |>
  unique() |>
  length()

miguel_africa |>
  pull(year) |>
  unique() |>
  length()

miguel_africa |>
  nrow()

# 3.2 ---------------------------------------------------------------------

miguel_africa |>
  ggplot(aes(
    x = year,
    y = fct_reorder(country_name, n_wars),
    fill = (as_factor(war))
  )) +
  geom_tile(color = "gray") +
  scale_fill_manual(values = c(
    Conflict = "indianred",
    'Minor conflict' = "pink",
    'No conflict' = "lightgray"
  )) +
  labs(fill = "War Type",
       title = "Wars in Africa",
       y = "Countries in Africa",
       x = "Year") +
  theme_classic()
