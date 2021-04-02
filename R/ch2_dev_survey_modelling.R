#### Chapter 2 ####

#stackoverflow dev survey
#notable mention about the data- 
#we're trying to model around remote work status
#but there's roughly 90% of the data in the Not Remote category
#& this class imbalance will have to be corrected for

#we're trying to predict whether a dev is remote or not
#based on characteristics from their company
#so we're gonna build a classification model

library(tidyverse)
stack_overflow <- read_csv("data/stack_overflow.csv")