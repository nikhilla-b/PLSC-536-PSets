library(tidyverse)
library(scales)
library(gt)
library(modelsummary)

# Problem 2.1 ---------------------------------------------------------------

graduation_hh <- read_rds("data/graduation_household.rds")

grad <-
graduation_hh |> 
  mutate(assignment = recode(assignment, 
                             "0" = "control_group", 
                             "1" = "treatment_group"
                             )
         )

grad |> 
  ggplot(aes(x = ctotal_pcmonth_end)) +
  facet_wrap(~assignment) +
  geom_histogram(aes(y = stat(width*density)),
                     boundary = 0, 
                     binwidth = 20
                 ) +
  labs(title = "Total monthly per capita consumption at endline 1 
 for control and treatment groups",
       x = "Total monthly per capita consumption at Endline 1 (2014 USD PPP)",
       y = "Proportion of sample")

ggsave("figures/total-month-pc-endline-1.png", width = 5, height = 5)

# Problem 2.2 -------------------------------------------------------------

summary <- 
  graduation_hh |> 
  group_by(country, assignment) |> 
  summarise(mean = round(mean(ctotal_pcmonth_end, na.rm = TRUE), 2)
            )
            

c_avg <-
  left_join(filter(summary, assignment == "0"),
            filter(summary, assignment == "1"), 
            by = "country"
            ) |> 
  rename("mean_control"   = "mean.x",
         "mean_treatment" = "mean.y"
                   ) |> 
  select(country, mean_control, mean_treatment)


  c_avg |> 
  gt(groupname_col = FALSE)






# Problem 2.4 -------------------------------------------------------------

summary |> 
  ggplot(aes(x = country, y = mean, fill = as.factor(assignment))) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("0" = "grey", "1" = "dark green"),
                    labels =c("0" = "Control Group", "1" = "Treatment Group"))+
  labs(fill = "Assignment Group", 
       y = "Mean consumption in endline", 
       title = "Mean consumption across treatment groups
                for 6 countries in the study")

ggsave("figures/mean_consumption.png", width = 5, height = 5)


# Problem 3.1 -------------------------------------------------------------

fit_peru <- lm(ctotal_pcmonth_end ~ assignment, 
               filter(graduation_hh, country == "Peru")
)

# Problem 3.2 -------------------------------------------------------------

models <- (list(
  "Peru" = lm(ctotal_pcmonth_end ~ assignment, 
              filter(graduation_hh, country == "Peru")
  ),
  "Ghana" =  lm(ctotal_pcmonth_end ~ assignment, 
                filter(graduation_hh, country == "Ghana")
  ),
  "India" = lm (ctotal_pcmonth_end ~ assignment, 
                filter(graduation_hh, country == "India (Bandhan)")
  ),
  "Ethiopia" = lm (ctotal_pcmonth_end ~ assignment, 
                   filter(graduation_hh, country == "Ethiopia")
  ),
  "Honduras" = lm (ctotal_pcmonth_end ~ assignment, 
                   filter(graduation_hh, country == "Honduras")
  ), 
  "Pakistan" = lm (ctotal_pcmonth_end ~ assignment, 
                   filter(graduation_hh, country == "Pakistan")
  )
)
)

modelsum <- 
  modelsummary(models,
               gof_map = "nobs",
               output = "gt" )






