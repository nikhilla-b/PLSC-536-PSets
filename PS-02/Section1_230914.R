library(tidyverse)
library(scales)
library(gt)
library(modelsummary)

graduation_hh <- read_rds("data/graduation_household.rds")

graduation_hh |> 
  ggplot(aes(x = ctotal_pcmonth_end)) +
  facet_wrap(~country) +
  geom_histogram(aes(y = stat(width*density)), boundary = 0, binwidth = 20) 


#use mutate and rename to relabel titles of facetwrap


# Running regression ------------------------------------------------------

m_p <- lm(ctotal_pcmonth_end ~ assignment, graduation_hh)
summary(m_p)

m_g <- lm (ctotal_pcmonth_end ~ assignment, 
           filter(graduation_hh, country == "Ghana"))
summary(m_g)

m_e <- lm (ctotal_pcmonth_end ~ assignment, 
           filter(graduation_hh, country == "Ethiopia"))
summary(m_e)

modelsummary(list(Pooled = m_p, 
                  Ghana = m_g,
                  Ethiopia = m_e))
             #gof_map = "")

?modelsummary()
