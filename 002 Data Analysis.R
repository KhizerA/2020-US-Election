### Preamble ###
### Purpose: Running a logistic regression on the cleaned VSG survey and post-stratifying the results 
##           with the cleaned ACS data
### Author: Khizer Asad
### Date: 2 November 2020
### Contact: k.asad@mail.utoronto.ca
### License: MIT 
### Pre-Requisites: 
##  - Please refer to the 001 Data Cleaning.R script first to retrieve the data sets that will be used


### Workspace setup ###
library(tidyverse)
library(ggplot2)
library(broom)
# Reading in our data 
survey <- read.csv('VSG.csv')
census <- read.csv('PS-cells.csv')

### Logistic regression ###
survey$state <- tolower(survey$state)
model <- lme4::glmer(vote_biden ~ gender + age + race + hispanic + foreign + income + (1|state),
                     data = survey, family = binomial(link= "logit"))

### Post-Stratification ###
# replacing DC with Maryland in states since it's not in the survey data
census$state <- ifelse(census$state == "district of columbia", "maryland", census$state)
# Adding a column to census data containing the log odds of voting for Biden
census$estimate <- 
  model %>%
  predict(newdata = census)
# Converting logit estimate to probability 
census$estimate <- exp(census$estimate) 
census$estimate <- census$estimate/(1+census$estimate)
# Summarising by geography, taken from Rohan's code example 
state_prob <- census %>% 
  mutate(state_predict_prop = estimate*proportion) %>%
  group_by(state) %>%
  summarise(prediction = sum(state_predict_prop))
state_prob$prediction[20] <- state_prob$prediction[20] / 2 


### Popular Vote ###
pop_vote <- tibble(Vote_Biden = 0, Vote_Trump = 0)
# Calculating the number of votes for Biden and Trump 
# Assuming that all votes are independent, then the sum of people voting for Biden is 
# probability of a cell voting for Biden * number of people in that cell
for (i in 1:nrow(census)) {
  bid <- round(census$n[i] * census$estimate[i])
  pop_vote$Vote_Biden <- pop_vote$Vote_Biden + bid
  pop_vote$Vote_Trump <- pop_vote$Vote_Trump + (census$n[i] - bid)
}
total <- pop_vote$Vote_Biden + pop_vote$Vote_Trump
# % of popular vote
pop_vote$Vote_Biden <- pop_vote$Vote_Biden/ total
pop_vote$Vote_Trump <- pop_vote$Vote_Trump/ total


### Electoral College Votes ###
# Reading in votes per state 
ec <- read.csv('electoral college.csv')
ec$State <- tolower(ec$State)
# Determining the number of seats for Biden 
# Biden wins a states seats if he has a majority of probability to win
seats <- data.frame(state = tolower(state.name), votes = rep(0,50) )
seats$votes <- ifelse(state_prob$prediction > 0.5, ec$Votes, 0)
# Adjusting for Maine and Nebraska that vote by proportion 
seats$votes[19] <- floor(ec$Votes[19]*state_prob$prediction[19])
seats$votes[27] <- floor(ec$Votes[27]*state_prob$prediction[27])
