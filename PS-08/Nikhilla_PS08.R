
# Calling libraries -------------------------------------------------------

library(tidyverse)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gt)


#1.1 -------------------------------------------------------------------------

osha <- read_csv("data/osha.csv")

lm1 <-
  lm(
    injury_rate ~ estab_age_netsmm1 + exp_imp_netsmm1 + exporter_netsmm1 + foreign_owned_netsmm1 + govt_contractor_netsmm1 + headquarters_netsmm1 + importer_netsmm1 + public_netsmm1 + standalone_netsmm1 + num_yrs_prev_on_sst,
    data = osha
  )

mod1_1 <- osha |>
  mutate(pred_lm1 = predict(lm1, osha),) |>
  summarise(
    TSS = sum((injury_rate - mean(injury_rate)) ^ 2),
    #TSS is measure of variance in real data
    SSR_1 = sum((injury_rate - pred_lm1) ^ 2),
    RS_1 = 1 - (SSR_1 / TSS)
  )


# 1.2 ---------------------------------------------------------------------

#Simple formula

form_simpl <-
  as.formula(
    injury_rate ~ has_tmin1_odi + any_insp_prior +
      any_complaint_tmin13 + num_nonfat_comp_insp_cy_tc99mm1 +
      initial_pen_cy_mzmm1 + ln_initial_pen_cy_mzmm1
  )

mod3 <- lm(form_simpl, data = osha)


#Kitchen sink

kitchen_mod <- osha |>
  select(-c(id, injury_rate, injuries, high_injury_rate)) |>
  names()

kitchen_model <- as.formula(paste0("injury_rate ~ ",
                                   paste(kitchen_mod, collapse = " + ")))

mod2 <- lm(kitchen_model, data = osha)

stats <- osha |>
  mutate(pred2 = predict(mod3, osha),
         pred3 = predict(mod2, osha)) |>
  summarise(
    TSS = sum((injury_rate - mean(injury_rate)) ^ 2),
    SSR_2 = sum((injury_rate - pred2) ^ 2),
    SSR_3 = sum((injury_rate - pred3) ^ 2),
    RS_2 = 1 - (SSR_2 / TSS),
    RS_3 = 1 - (SSR_3 / TSS)
  )

# 2.1 -----------------------------------------------------------------------

set.seed(06510)

osha_split <- rsample::initial_split(osha, prop = .8)

osha_train <- training(osha_split)

osha_test <- testing(osha_split)


# 2.2 ---------------------------------------------------------------------

lasso <- cv.glmnet(kitchen_model, data = osha_train)

mod3_train <- lm(form_simpl, data = osha_train)

mod2_train <- lm(kitchen_model, data = osha_train)


RMSE_train <- osha_train |>
  mutate(
    pred1 = predict(mod3_train, osha_train),
    pred2 = predict(mod2_train, osha_train),
    pred3 = predict(lasso, osha_train, s = "lambda.min")
  ) |>
  summarise(
    rmse_1 = sqrt(mean((injury_rate - pred1) ^ 2)),
    rmse_2 = sqrt(mean((injury_rate - pred2) ^ 2)),
    rmse_3 = sqrt(mean((injury_rate - pred3) ^ 2))
  )

RMSE_test <- osha_test |>
  mutate(
    pred1 = predict(mod3_train, osha_test),
    pred2 = predict(mod2_train, osha_test),
    pred3 = predict(lasso, osha_test, s = "lambda.min")
  ) |>
  summarise(
    rmse_1 = sqrt(mean((injury_rate - pred1) ^ 2)),
    rmse_2 = sqrt(mean((injury_rate - pred2) ^ 2)),
    rmse_3 = sqrt(mean((injury_rate - pred3) ^ 2))
  )

optimal_lambda <- lasso$lambda.min
lasso_coef <- coef(lasso, s = "lambda.min")
non_zero_variables <- sum(lasso_coef != 0)

# 3 -----------------------------------------------------------------------

simple_logit <-
  as.formula(
    high_injury_rate ~ has_tmin1_odi + any_insp_prior +
      any_complaint_tmin13 + num_nonfat_comp_insp_cy_tc99mm1 +
      initial_pen_cy_mzmm1 + ln_initial_pen_cy_mzmm1
  )

simp_log <-
  glm(simple_logit, data = osha_train, family = "binomial")

kitchen_mod <- as.formula(paste0("high_injury_rate ~ ",
                                 paste(setdiff(
                                   names(osha),
                                   c("id", "injury_rate", "high_injury_rate", "injuries")
                                 ), collapse = " + ")))

kitchen_logit <-
  glm(kitchen_mod, data = osha_train, family = "binomial")

lasso_logit <-
  cv.glmnet(kitchen_mod, data = osha_train, family = "binomial")

test_pred <- osha_test |>
  mutate(
    pred1 = predict(simp_log, osha_test, type = "response"),
    pred2 = predict(lasso_logit, osha_test, type = "response", s = "lambda.min"),
    pred3 = predict(kitchen_logit, osha_test, type = "response")
  )

scatter_data <- data.frame(
  pred1 = test_pred$pred1,
  pred2 = test_pred$pred2,
  pred3 = test_pred$pred3,
  high_injury_rate = osha_test$high_injury_rate
)


scatter_data$color <-
  ifelse(scatter_data$high_injury_rate == 1, "red", "gray")
scatter_data$shape <-
  ifelse(scatter_data$high_injury_rate == 1, 17, 16)


scatter_data |>
  ggplot(aes(
    x = pred3,
    y = lambda.min,
    color = factor(high_injury_rate),
    shape = factor(high_injury_rate)
  )) +
  geom_point(size = 1) +
  xlab("Predicted Probability of High Injury (Kitchen Sink Logit Model)") +
  ylab("Predicted Probability of High Injury (LASSO Logit Model)") +
  scale_color_manual(values = c("1" = "red", "0" = "gray"),
                     labels = c("Not High Injury", "High Injury")) +
  scale_shape_manual(values = c("1" = 17, "0" = 16),
                     labels = c("Not High Injury", "High Injury")) +
  theme_minimal() +
  labs(title = "Predicted probability of high injury by 2 models")
