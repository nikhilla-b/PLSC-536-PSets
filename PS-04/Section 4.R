#Map/Loops 

filenames <- dir_ls("data/banerjee")

library(tidyverse)

?map()

library(haven)

df_list <- map(filenames, read_dta)

df <- read_dta("data/banerjee/Peru.dta")

df[2, 1] #row and then column 

df_list[[2]]

df_list1 <- map(filenames, read_dta) |> 
  list_rbind(names_to = "country")

country <- str_remove_all(filenames, "data/banerjee/|.dta")

names(filenames) <- country

read_country <- function(x){
  df <- df_list |> 
    filter(countries == x)
}

read_country("Ghana")


# instrument variable -----------------------------------------------------

india <- read_dta("data/pooled_hh_noncompliance.dta")

fit1 <- lm(consumption_index_bsl ~ assignment, data = india) |> 
  tidy()

fit2 <- lm(uptake ~assignment, data = india) |> 
  tidy()

fit1$estimate[2] / fit2$estimate[2]

library(fixest)

feols()

feols(sav_depositamt_fup ~ 1 | uptake )