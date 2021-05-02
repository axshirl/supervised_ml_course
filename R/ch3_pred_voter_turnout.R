#data comes from the Democracy Fund Voter Study Group
#and is freely available on their site
#a lot of the question answers will often be coded as
#Strongly Agree to Strongly Disagree, 1 to 4

#We want to predict _if_ someone voted in the 2016 election
#therefore, we want to predict group membership w/ a classification model

library(tidyverse)
voters <- read_csv("data/voters.csv")

glimpse(voters)

voters %>% 
  count(turnout16_2016)
#Only 264 "Did not vote"
#6428 "Voted"
#Huuuugely weighted towards voting- we need to be careful about
#our model heavily preferring voting 

#checking some groups-
#We'd imagine that folks that didn't vote more often feel negatively about the process
#& that these things are similarly correlated
voters %>%
  group_by(turnout16_2016) %>% 
  summarise(`Elections don't matter` = mean(RIGGED_SYSTEM_1_2016 <= 2), 
            `Economy is getting better` = mean(econtrend_2016 == 1), 
            `Crime is very important` = mean(imiss_a_2016 == 2))

voters <- voters %>% 
  mutate(turnout16_2016 = factor(turnout16_2016))

#histogram of thoughts on economy (1 = better, 2 = same, 3 = worse, 4 = do not know)
voters %>% 
  ggplot(aes(econtrend_2016, after_stat(density), fill = turnout16_2016)) + 
  geom_histogram(alpha = 0.5, position = "identity", binwidth = 1) + 
  labs(title = "Overall, is the economy getting better or worse") 




