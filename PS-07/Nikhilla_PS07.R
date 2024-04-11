
# Calling libraries -------------------------------------------------------

library(tidyverse)
library(haven)
library(fixest)
library(modelsummary)
library(patchwork)
library(rdd)
library(broom)
library(glue)
library(gt)

# 1.1 -----------------------------------------------------------------------

mexico <- read_dta("data/mexico_localities.dta")

mexico <-
  mexico |>
  mutate(PES_offered = points >= 0)

Rd_1 <-
  mexico |>
  ggplot(aes(x = points, y = lnIMLoc2010_EJawm, group = PES_offered)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "Eligibility scores",
       y = "Log(Poverty Index 2010)",
       title = "RD Plot of Eligibility Scores
      and Poverty Index")

mexico <-
  mexico |>
  group_by(state_id) |>
  mutate(mean_mng = mean(mng_index_raw),
         recentered_mng = mng_index_raw - mean_mng)

Rd_2 <-
  mexico |>
  ggplot(aes(x = points, y = recentered_mng, group = PES_offered)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5, size = 0.5) +
  geom_smooth(se = FALSE, method = "lm") +
  theme_bw() +
  labs(x = "Eligibility scores",
       y = "Management Index Recentred",
       title = "RD Plot of Eligibility Scores
      and Management Index")

Rd_1 + Rd_2

# 1.2 ---------------------------------------------------------------------

sample_averages <- mexico |>
  group_by(bin, cohort) |>
  summarize(mng_avg = mean(recentered_mng),
            sample_size = n())

ggplot() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.5,
             data = sample_averages,
             aes(x = bin, y = mng_avg, size = sample_size)) +
  geom_smooth(
    data = mexico,
    aes(
      x = points,
      y = recentered_mng,
      group = PES_offered,
      color = "Linear Fit"
    ),
    se = FALSE,
    method = "lm"
  ) +
  geom_smooth(
    data = mexico,
    aes(
      x = points,
      y = recentered_mng,
      group = PES_offered,
      color = "Quadratic Fit"
    ),
    method = "lm",
    formula = y ~ poly(x, 2),
    se = FALSE
  ) +
  scale_size_area(max_size = 8) +
  facet_wrap( ~ cohort) +
  theme_bw() +
  labs(x = "Eligibility scores",
       y = "Management Index Recentered",
       title = "RD estimate of PES on land management index across cohorts") +
  scale_color_manual(
    values = c("Linear Fit" = "blue", "Quadratic Fit" = "red"),
    labels = c("Linear Fit", "Quadratic Fit"),
    name = "Fit Type"
  )

# 2.1 ---------------------------------------------------------------------

mexico_2013_2014 <-
  mexico |>
  filter(cohort == "2013-2014")

h <- c(1, 2, 3, 4, 5)

bandwidth_estimates <- function(x) {
  lm(
    mng_index_raw ~ points + PES_offered + PES_offered:points + I(points ^ 2) + PES_offered:I(points ^ 2),
    data = mexico_2013_2014 |>
      filter(points >= -x, points <= x)
  )
}

model_estimates <-
  h |>
  map(bandwidth_estimates) |>
  set_names(h)

modelsummary(model_estimates, gof_map = "nobs", coef_omit = -3)

coefs <-
  model_estimates |>
  map(~ tidy(.))

mexico_model <-
  list_rbind(coefs, names_to = "h") |>
  filter (term == "PES_offeredTRUE")

mexico_model |>
  ggplot(aes(x = h, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 2 * std.error,
                    ymax = estimate + 2 * std.error),
                width = 0.1) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red") +
  geom_line() +
  labs(x = "Bandwidth", y = "Treatment Effect", title = "Coefficient Plot of PES on Management Index for different bandwidths") +
  theme_bw()


# 2.2 ---------------------------------------------------------------------

#calculating IK bandwidth

tri <- IKbandwidth(mexico_2013_2014$points, mexico_2013_2014$mng_index_raw, cutpoint = 0, verbose = FALSE, kernel = "triangular")

rect <- IKbandwidth(mexico_2013_2014$points, mexico_2013_2014$mng_index_raw, cutpoint = 0, verbose = FALSE, kernel = "rectangular")

triangular <-
  RDestimate(mng_index_raw ~ points + PES_offered + points:PES_offered,
             mexico_2013_2014,
             kernel = "triangular")

rectangular <-
  RDestimate(mng_index_raw ~ points + PES_offered + points:PES_offered,
             mexico_2013_2014,
             kernel = "rectangular")

summary(triangular)
summary(rectangular)


# 2.3 ---------------------------------------------------------------------

sd <-
  mexico_2013_2014 |>
  ungroup() |>
  filter(PES_offered == FALSE) |>
  summarise(mean = mean(mng_index_raw),
            sd = sd(mng_index_raw))


# 3 -----------------------------------------------------------------------

feols(mng_index_raw ~ points + PES_offered:points |
        uptake ~ PES_offered,
      mexico_2013_2014)
