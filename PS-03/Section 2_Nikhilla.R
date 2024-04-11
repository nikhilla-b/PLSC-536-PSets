library(haven) #to load stata data files
library(tidyverse)
library(kableExtra)

mmm <- read_dta("data/mmm_replication.dta")
view(mmm)
colnames(mmm)
dim(mmm)
str(mmm)

# and & or| operator 

mmm$birthyear < 1980 & mmm$employed == 1 #if false, one of the conditions is not satisfied

#x tabs - crosstab 
#Transmute creates a new data set with that new variable whereas mutate creates a new variable in the existing dataset 

