# Calling Libraries -------------------------------------------------------

library(tidyverse)
library(scales)
library(giscoR)
library(sf)
library(ggthemes)


# Code Style --------------------------------------------------------------


#flights |> 
  #filter(
    #carrier == "UA",
    #dest %in% c("IAH", "HOU"), 
    #sched_dep_time > 0900,
    #sched_arr_time < 2000
    #) |>
  #group_by(flight) |>
  #summarize(
    #delay = mean(arr_delay, na.rm = TRUE),
    #cancelled = sum(is.na(arr_delay)), 
    #n = n()
    #) |>
  #filter(n > 10) 

# Problem 1 ---------------------------------------------------------------

#Clean this up 


countries <- 
  gisco_get_countries() 

countries <-
  countries |> 
  mutate(banerjee_study = NAME_ENGL %in% c("Honduras", "Ghana", "Pakistan", "India", "Ethiopia", "Peru")
  )

countries |> 
ggplot() +
  geom_sf(aes(fill = banerjee_study)) +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "grey"),
                    labels = c("other_countries", "countries_studied")) +
  theme_map() +
  labs(title = "Countries studied in Banerjee et al")

ggsave("figures/map.png", width = 5, height = 5)

# Problem 2 ---------------------------------------------------------------

dat <- read_csv("data/graduation_honduras.csv")

dat |> 
  ggplot(aes(x = asset_tot_value_bsl)) +
  geom_histogram(boundary = 0) +
  labs(x = "Total asset value (2014 USD PPP)",
       y = "Frequency", 
       title = "Frequency distribution of total asset value")

ggsave("figures/total-asset-value-2014.png", width = 5, height = 5)




# Problem 3 ---------------------------------------------------------------


# 3.1 ---------------------------------------------------------------------


dat |> 
  select(hh_head_female) |> 
  summarise(average = mean(hh_head_female)
            )

# 17.04% of the households are headed by a woman


# 3.2 ---------------------------------------------------------------------

summary <- 
  dat |> 
  group_by(hh_head_female) |> 
  summarise(
    prop_for_ed = round(mean(hh_education), 2),
    median_asset_value = round(median(asset_tot_value_bsl), 2),
    mean_monthly_income_business = round(mean(ibusiness_month_bsl), 2),
    tot_inc_paid_labour = sum(ipaidlabor_month_bsl),
  )
   


# 3.3 ---------------------------------------------------------------------

count <-
  dat |> 
  count(hh_head_female, hh_education)


# 3.4 ---------------------------------------------------------------------

count |> 
  group_by(hh_head_female) |> 
  mutate(proportion = n/sum(n)
         )
  
