
#Sharp RD 
#We dont know about points on the cut off line; so we use point around the cut off - before and after 
#Vi is the distance between data point i from cut off line 
#limit argument 
#Di = 1 or 0 
#we are only concerned about coefficient of D others act as controls or beta 1

#Extrapolation 
#predict things outside data range 

library(rdd)
library(rdrobust)

#local OLS - estimate around neighborhood - drop data points but increase variance but low bias 
#selecting bandwidth
#interaction term is not really needed in RDD 

#Kernel x axis is distance from cut off and y axis is weightage of margin 

#Fuzzy RD - election fraud or death - reach above threshold but does not receive treatment


#randomly turn win into control group - IV strategy then for running regression 


#RD plot


#Take away - difference between two points on cut off line we can never observe
#Now a days people prefer local OLS with kernel over complicated polynomial models 

isnt cutoff IV
Doesnt interaction term give us slope of line after cut off 