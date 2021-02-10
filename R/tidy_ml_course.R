#install.packages(c('tidyverse', 'tidymodels','randomForest'))
library(tidyverse)
cars2018 <- read_csv("data/cars2018.csv")

# Print the cars2018 object
glimpse(cars2018)

# Plot the histogram
ggplot(cars2018, aes(x = mpg)) +
  geom_histogram(bins = 25) +
  labs(x = "Fuel efficiency (mpg)",
       y = "Number of cars")

# Deselect the 2 columns to create cars_vars
car_vars <- cars2018 %>%
  select(-model, -model_index)

# Fit a linear model
fit_all <- lm(mpg ~ ., data = car_vars)

# Print the summary of the model
summary(fit_all)

require(tidymodels)

#split to train/test sets
set.seed(1234)
car_split <- car_vars %>%
  initial_split(prop = 0.8,
                strata = transmission)

car_train <- training(car_split)
car_test <- testing(car_split)

#training and testing data

glimpse(car_train)
glimpse(car_test)

#I have a sneaking suspicion there was additional 
#transformation done here... namely char -> factor conversion
#loading her intermediaries to check
car_train <- readRDS("data/c1_train.rds")
car_test <- readRDS("data/c1_test.rds")

#linear model spec
lm_mod <- linear_reg() %>%
  set_engine('lm')

#training lm
fit_lm <- lm_mod %>%
  fit(log(mpg) ~ .,
      data = car_train)

fit_lm

#rf model spec
rf_mod <- rand_forest() %>%
  set_engine("randomForest") %>%
  set_mode("regression")

#train rf 
fit_rf <- rf_mod %>%
  fit(log(mpg) ~ ., 
      data = car_train)

fit_rf

#8



