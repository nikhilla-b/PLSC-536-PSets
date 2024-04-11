library(haven)
library(tidyverse)
library(kableExtra) # for pset3 better use gt, not kable
library(gt)
# we use haven:read_dta to load Stata file into R
# we can see the 
mmm <- read_dta("data/mmm_replication.dta")
colnames(mmm)  
dim(mmm)



## and or operator 
# what is the interpretation of this
mmm$birthyear < 1980 & mmm$employed == 1
# what about this
mmm$birthyear < 1980 | mmm$employed == 1
# what about this
mmm$birthyear < 1980 | mmm$employed == 1 | mmm$techwork == 1
# what about this
mmm$birthyear < 1980 & mmm$employed == 1 & mmm$techwork == 1
# what about this
mmm$birthyear < 1980 & !mmm$employed == 1 & mmm$techwork == 1 
mmm$birthyear < 1980 & mmm$employed != 1 & mmm$techwork == 1 
mmm$birthyear < 1980 & mmm$employed == 0 & mmm$techwork == 1 


#recode and case_when 
# Creating an example dataframe
data_frame <- data.frame(Brand=c("Maruti Suzuki", "Tata Motors",
                                "Mahindra", "Mahindra", "Maruti Suzuki"),
                        Car=c("Swift", "Nexon", "Thar", "Scorpio", "WagonR"),
                        Price=c(400000, 1000000, 500000, 1200000, 900000),
                        Tax=c(2000, 4000, 2500, 5000, 3500))

# Using case_when() to create new variable
data_frame |>
  mutate(
    Price_status = case_when(
      Price > 900000 ~ "High",
      Price >= 500000 & Price <= 900000 ~ "Average",
      TRUE ~ "Low"
      #Price < 500000 ~ "Low" 
    )
  )


#another way to do this with recode but less efficient
data_frame |> mutate(
  result = recode(
    Price,
    "1000000" = "High",
    "1200000" = "High",
    "900000" = "Average",
    "500000" = "Average",
    "400000" = "Low"
  )
)

# back to mmm dataset 
mmm_fmt <- mmm |>
  mutate(
    newvar = case_when(
      birthyear < 1980 & !employed == 1 ~ "Older and unemployed",
      birthyear < 1980 & employed == 1 ~ "Older and employed",
      birthyear >= 1980 & !employed == 1 ~ "Young and unemployed",
      birthyear >= 1980 & employed == 1 ~ "Young and employed"
    )
  )



#now let's get to know xtabs. xtabs allows you to cross-tabulate. 
#now I'll cross tabluate by two variables: newvar and gender. Of course, there are 
#many ways to achieve the same result. A combination of group_by and mutate would work just fine. 
tab <- xtabs(~ newvar + gender, mmm_fmt)

#prop.table allows you to calculate marginal probabilities 
tab <- prop.table(tab, margin = 2) 

#to generate neat tables, you can use gt. and it requires tranforming the table 
#into a tibble object. The following code chunk would do it.
tab |> as_tibble() |>
  pivot_wider(id_cols = newvar,
              names_from = gender,
              values_from  = n) |>
  gt::gt() |>
  gt::fmt_number(decimals = 2) |> cols_label(newvar = "Group",
                                             "0" = "Male",
                                             "1" = "Female")

#another way to generate neat tables is use we already know: kbl. 
#kbl also presents neat tables 

kbl(tab, digits = 2, col.names = c("Male", "Female"), format = "simple")

#putting the code together using pipe 
xtabs(~ newvar + gender, mmm_fmt) |>
  prop.table(margin = 2) |> kbl(digits = 2, col.names = c("Male", "Female"), format = "simple")



gpp <- read_dta("data/graduation_pakistan-peru.dta")
#standardization
gpp_stats <- gpp |>
  summarize(
    c_mean_fup = mean(asset_tot_value_fup, na.rm = TRUE),
    c_sd_fup = sd(asset_tot_value_fup, na.rm = TRUE),
  )

gpp_std <- gpp |>
  transmute(
    asset_tot_value_fup_std = 
      (asset_tot_value_fup - gpp_stats$c_mean_fup) / gpp_stats$c_sd_fup)









