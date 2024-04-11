#Goodness of fit measure - R squared 

#names(osha)

#tidyverse

kitchen <- osha |> 
  select(-c(id, injury_rate, injuries, high_injury_rate)) |> 
  names

kitchen_formula <- as.formula(paste0("injury_rate ~ ", paste(kitchen, collapse = "+"))

mod2 <- lm(kitchen_formula, data = osha)

#TSS, SSR, R squared

osha <- osha |> 
  mutate(pred1 = predict(mod1, osha),
         pred2 = predict(mod2, osha)
         ) |> 
  summarise(TSS = sum(injury_rate - mean(injury_rate)^2), #TSS is measure of variance in real data 
            SSR_1 = sum((injury_rate - pred1)^2),
            RS_1 = 1 - (SSR_1/TSS))

#split 


set.seed(06511) #other people using our data should get same randomisation 


#count how many coeff were left
#how is RMSE different from MSE
#Why is loess better in test than train data 