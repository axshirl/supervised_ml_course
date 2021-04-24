#### Chapter 2 ####
#stackoverflow dev survey


#we're trying to predict whether a dev is remote or not
#based on characteristics from their company
#so we're gonna build a classification model

library(tidyverse)
stack_overflow <- read_csv("data/stack_overflow.csv")

#looking at the data
glimpse(stack_overflow)
View(stack_overflow)
#rough overview of cols
#respondent num (numeric, looks like index of respondents)
#country (chr, self explanatory. looks clean already)
#salary (float, yearly salary of the respondent)
#years_coded_job (years at a coding job?)
#open_source (bool, does the respondent contribute to open source projects?)
#hobby (bool, does respondent code as hobby?)
#company_size_number (numeric, self explan)
#remote (character, "Not Remote" or "Remote" NOTE CHAR, NOT BOOL. 
#Are there other values of the remote field?)
unique(stack_overflow$remote)
#Nope. Just these two.
#career_satisfaction (numeric, 1-10 satisfaction)
#wonder how salary and remote status affect this tbh. might be a neat
#regression
#and the rest are booleans for if they fit into specific job titles
#i.e. data_scientist == TRUE or web_developer == TRUE

stack_overflow %>%
  count(remote, sort = TRUE)

#~6300 Non-Remote workers (bet this changes big this year!)
#and ~700 Remote workers. Note the large class imbalance.

stack_overflow %>%
  count(country, sort=TRUE)

ggplot(stack_overflow, aes(remote, years_coded_job)) +
  geom_boxplot() +
  labs(x = NULL,
       y = "Years of professional coding experience")
#Cool- Boxplot showing years of coding experience for each group

#What have we learned?
#First- Generally our respondents are not remote workers and generally
#the few that are remote workers have more experience on average
#as well as most of the respondents are American, making up nearly
#half of the respondents.



library(tidymodels)
stack_select <- stack_overflow %>%
  select(-respondent)

#creating train/test split
set.seed(1234)
stack_split <- stack_overflow %>% 
  initial_split(prop = 0.8)

stack_train <- training(stack_split)
stack_test <- testing(stack_split)


#often, our datasets have imbalances- a lot more of 1 category than another
#will often mean that the model will just predict the majority category, or 
#our performance on smaller groups will suffer.

#plan is to downsample- to remove observations from the majority category
#until it reaches  the same size as the minority class

library(themis)
stack_recipe <- recipe(remote ~ ., data = stack_train) %>% 
  step_downsample(remote)

stack_recipe
