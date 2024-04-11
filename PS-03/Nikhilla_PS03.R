
# Calling relevant libraries ----------------------------------------------

library(tidyverse)
library(gt)
library(haven)
library(urbnmapr)
library(sf)


# Reading data set ---------------------------------------------------------

mmm <- read_dta("data/mmm_replication.dta")


# 1.1 ---------------------------------------------------------------------

counties <- get_urbn_map("counties", sf = TRUE)

mmm <-
  mmm |>
  mutate(in_survey = 1)

counties <-
  left_join(counties, mmm, by = "county_fips")

counties |>
  mutate(in_survey = ifelse(is.na(in_survey), 0, in_survey)) |>
  ggplot() +
  geom_sf(aes(fill = factor(in_survey, levels = c("1", "0")))) +
  scale_fill_manual(
    values = c("1" = "red", "0" = "grey"),
    labels = c("In the survey", "Not in the survey")
  ) +
  labs(title = "Malhotra et al study 2013",
       fill = "Counties in the survey")

ggsave("figures/county_map.png", width = 5, height = 5)


# 1.2 ---------------------------------------------------------------------

mmm <-
  mmm |>
  mutate(
    employmentgroup = case_when(
      employed == 0 ~ "Unemployed",
      techwork == 1 ~ "Tech Worker",
      (!lawspecific == "") |
      (!financespecific == "") |
      (!realestatespecific == "") &
      techwork == 0 ~ "Other White Collar",
      TRUE ~ "All other workers"
    )
  )

emp <-
  mmm |>
  mutate(h1bvisas = as_factor(h1bvisas)) |>
  group_by(h1bvisas, employmentgroup) |>
  count() |>
  group_by(employmentgroup) |>
  mutate(prop = (round(n / sum(n), 2)))

total_emp <- emp |>
  group_by(employmentgroup) |>
  summarize(n = sum(n, 0))


emp |>
  as_tibble() |>
  pivot_wider(id_cols     = employmentgroup,
              names_from  = h1bvisas,
              values_from = prop) |>
  left_join(total_emp, by = "employmentgroup") |>
  gt() |>
  cols_label(employmentgroup = "") |>
  tab_spanner(
    label = "H1B Attitude",
    columns = c(
      "Decrease a great deal",
      "Decrease a little",
      "Neither increase nor decrease",
      "Increase a little",
      "Increase a great deal"
    )
  )



# 2 ---------------------------------------------------------------------

mmm <-
  mmm |>
  mutate(
    h1bvisas_scaled =
      case_when(
        h1bvisas == 1 ~ "0",
        h1bvisas == 2 ~ "0.25",
        h1bvisas == 3 ~ "0.5",
        h1bvisas == 4 ~ "0.75",
        TRUE ~ "1"
      ),
    pid_scaled      = partyid / max(partyid),
    educ_scaled     = education / max(education),
    marital_scaled  = case_when(maritalstatus == 4 ~ "1",
                                TRUE ~ "0"),
    race_scaled     = case_when(race == 8 ~ "1",
                                TRUE ~ "0")
  )

mmm$employmentgroup <- as.factor(mmm$employmentgroup)
mmm$employmentgroup <- relevel(mmm$employmentgroup,
                               ref = "All other workers")


model <- lm(
  h1bvisas_scaled ~ employmentgroup +
    dscore +
    gender +
    age +
    I(age ^ 2) +
    marital_scaled +
    educ_scaled +
    race_scaled +
    income +
    pid_scaled +
    techzip2,
  mmm
)

summary(model)

rounded_model <- round(summary(model)$coefficients, 2)


# 3.1 ---------------------------------------------------------------------

graduation <- read_dta("data/graduation_pakistan-peru.dta")

summary <-
  graduation |>
  filter(assignment == 0) |>
  group_by(country) |>
  summarize(
    sample_mean  = mean(ctotal_pcmonth_fup, na.rm = TRUE),
    sample_stdev = sd(ctotal_pcmonth_fup, na.rm = TRUE)
  )


# 3.2 ---------------------------------------------------------------------

graduation <-
  graduation |>
  left_join(summary,
            by = "country")

graduation <-
  graduation |>
  mutate(z_score = (ctotal_pcmonth_fup - sample_mean) / sample_stdev)


# 3.3.1 -----------------------------------------------------------------------

simple <- lm(z_score ~ assignment,
             graduation)

summary(simple)


# 3.3.2 -----------------------------------------------------------------------


model_2 <- lm(z_score ~ assignment + ctotal_pcmonth_bsl,
              graduation)

summary(model_2)


# 3.3.3 -----------------------------------------------------------------------

graduation <-
  graduation |>
  mutate(country_binary = case_when(country == 5 ~ 0,
                                    TRUE ~ 1))

model_3 <-
  lm(z_score ~ assignment + ctotal_pcmonth_bsl + country_binary,
     graduation)

summary(model_3)
