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

stack_prep <- prep(stack_recipe)
stack_down <- bake(stack_prep, new_data=NULL) #new_data arg extracts the processed training data
#meaning now we have our balanced data

stack_down %>% 
  count(remote) #Check it out- 573 Not Remote, 573 Remote. Thanos'd the majority group.

#going to use model specs from parsnip (included in tidymodels)
#using workflow(), which lets us build out a workflow w/ recipe/formula and a model
#if you don't add one of these pieces, workflow() will hold an empty spot for that piece
#which means you can write your modelling code w/ the same preprocessor but different
#model specs, for example. i.e. add recipe to workflow object & store
#and then you can add spec to that object and store seperately. workflow in one spot,
#specs in N other spots.

#build logreg model
glm_spec <- logistic_reg() %>%
  set_engine("glm")

stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

stack_glm <- stack_wf %>%
  add_model(glm_spec) %>%
  fit(data = stack_train)
#note how workflow works here-
#you add components as you want them. reminds me of ggplot kinda? can
#stop at any element and cut & reuse all prior pieces at once

#viewing stack_glm at this point will give us slopes for the logreg model

#using stack_recipe from before- this was the recipe w/ the downsample step
#decision tree model
tree_spec <- decision_tree() %>%
  set_engine("rpart") %>%
  set_mode("classification")

stack_wf <- workflow() %>%
  add_recipe(stack_recipe)

stack_tree <- stack_wf %>%
  add_model(tree_spec) %>%
  fit(data = stack_train)

#note n=1146- because we downsampled on Remote status




