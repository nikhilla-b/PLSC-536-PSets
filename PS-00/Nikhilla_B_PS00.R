# 1 Read & Clean Data

# Calling relevant libraries

library(tidyverse)
library(readxl)
library(janitor)
library(scales)

# 1.1 Read excel

dat_orig <- read_excel("data/states-data.xlsx")

# 1.2 Clean variable names and dplyr function

dat <- dat_orig |> 
  clean_names() |> 
  mutate(biden_vshare = d_2020 / (d_2020 + r_2020)) |> 
  rename(st = state_abbreviated,
         inc = income_percapita,
         disp = disposable_income_percapita,
         inc_med = median_household_income)

# 2 Visualize

# 2.1

dat |> 
  ggplot(aes(x = inc, y = biden_vshare)) + 
  geom_point() +
  labs(
    x = "Household Income Per Capita",
    y = "2020 Democratic Presidential Voteshare"
  )

# 2.2

gg_scatter <- dat |> 
  ggplot (aes(x = inc, y = biden_vshare)) +
  geom_smooth(method = "lm") + 
  geom_text(aes(label = st)) +
  scale_x_continuous(label = dollar) +
  scale_y_continuous(label = percent) +
  labs(
    x = "Household Income Per Capita",
    y = "2020 Democratic Presidential Voteshare") +
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "in"))


# 2.3 Saving figures

ggsave("figures/inc-partisan-2020.png", width = 5, height = 5)


