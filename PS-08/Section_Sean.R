### goals:
# compute and compare TSS, SSR, R-squared, RMSE
# use predict with lm(), lasso, logit
# set seed and split sample

# load packages
library(tidyverse)
library(rsample)
library(glmnet)
library(glmnetUtils)
library(glue)
library(gt)

# read subset of data
osha <- read_csv("data/osha.csv") |> 
  select(1:13, has_tmin1_odi,  any_insp_prior, 
         any_complaint_tmin13,  num_nonfat_comp_insp_cy_tc99mm1, 
         initial_pen_cy_mzmm1,  ln_initial_pen_cy_mzmm1, starts_with(c("avg","any","l2")))

# run "simple model"
form_simpl <- injury_rate ~ has_tmin1_odi + any_insp_prior +
  any_complaint_tmin13 + num_nonfat_comp_insp_cy_tc99mm1 +
  initial_pen_cy_mzmm1 + ln_initial_pen_cy_mzmm1

# lm here
mod1 <- lm(form_simpl, data = osha)

# run kitchen sink model

# base R - use setdiff

kitchen_mod <- as.formula(
  paste0("injury_rate ~ ", 
         paste(setdiff(names(osha), c("id","injury_rate","high_injury_rate","injuries")), collapse = " + "))
)

# tidyverse select
kitchen_mod <- osha |> 
  select(-c(id, injury_rate, injuries, high_injury_rate)) |> 
  names()

kitchen_mod <- as.formula(
  paste0("injury_rate ~ ", 
         paste(kitchen_mod, collapse = " + "))
)

mod2 <- lm(kitchen_mod, data = osha)

# make TSS, SSR, R-squared

stats <- osha |> 
  mutate(pred1 = predict(mod1, osha),
         pred2 = predict(mod2, osha)) |> 
  summarise(TSS = sum((injury_rate - mean(injury_rate))^2),
            SSR_1 = sum((injury_rate - pred1)^2),
            SSR_2 = sum((injury_rate - pred2)^2),
            RS_1 = 1 - (SSR_1 / TSS),
            RS_2 = 1 - (SSR_2 / TSS))

# split into training and test - use rsample::initial_split()
# set seed

set.seed(06511)

osha_split <- rsample::initial_split(osha, prop = .8)

osha_train <- training(osha_split)

osha_test <- testing(osha_split)

# compare short, kitchen sink, lasso

mod1 <- lm(form_simpl, data = osha_train)
mod2 <- lm(kitchen_mod, data = osha_train)
mod3 <- cv.glmnet(kitchen_mod, data = osha_train)

mod3$lambda.min

# make RMSE for train
RMSE_train <- osha_train |> 
  mutate(pred1 = predict(mod1, osha_train),
         pred2 = predict(mod2, osha_train),
         pred3 = predict(mod3, osha_train, s = "lambda.min")) |> 
  summarise(rmse_1 = sqrt(mean((injury_rate - pred1)^2)),
            rmse_2 = sqrt(mean((injury_rate - pred2)^2)),
            rmse_3 = sqrt(mean((injury_rate - pred3)^2)))

#?predict.glmnet()
#coef(mod3, s = "lambda.min")

# make RMSE for test
RMSE_test <- osha_test |> 
  mutate(pred1 = predict(mod1, osha_test),
         pred2 = predict(mod2, osha_test),
         pred3 = predict(mod3, osha_test, s = "lambda.min")) |> 
  summarise(rmse_1 = sqrt(mean((injury_rate - pred1)^2)),
            rmse_2 = sqrt(mean((injury_rate - pred2)^2)),
            rmse_3 = sqrt(mean((injury_rate - pred3)^2)))

# now compute logit model
kitchen_mod <- as.formula(
  paste0("high_injury_rate ~ ", 
         paste(setdiff(names(osha), c("id","injury_rate","high_injury_rate","injuries")), collapse = " + "))
)

# add predictions - remember response and lambda min
mod1 <- glm(kitchen_mod, data = osha, family = "binomial")
mod2 <- cv.glmnet(kitchen_mod, data = osha, family = "binomial")

osha <- osha |> 
  mutate(pred1 = predict(mod1, osha, type = "response"),
         pred2 = predict(mod2, osha, type = "response", s = "lambda.min"))
