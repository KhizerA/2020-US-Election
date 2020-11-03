### Preamble ###
### Purpose: Prepping both data sets for logistic regression and post-stratification
### Author: Khizer Asad
### Date: 2 November 2020
### Contact: k.asad@mail.utoronto.ca
### License: MIT 
### Pre-Requisites: 
## - Need to request for and download the Voter Study Group Nationscape Data set 
##   (https://www.voterstudygroup.org/publication/nationscape-data-set)
## - And also the IPUMS ACS data for 2018 (variables will be specified in the code)

### Workspace setup ###
library(haven)
library(tidyverse)
library(janitor)
library(labelled)

# Reading in the survey data
raw_survey <- read_dta("ns20200625.dta") 
raw_survey <- labelled::to_factor(raw_survey)

# Reading in the post-stratification data 
raw_acs <- read_dta("usa_00002.dta.gz")
raw_acs <- labelled::to_factor(raw_acs) 

### Survey Data Cleaning ###
# Survey Data for logistic regression
# Filtering to keep only the variables we want 
reduced_data <- 
  raw_survey %>% 
  select(interest, 
         registration, 
         vote_2016,
         vote_intention,
         vote_2020, 
         ideo5,
         employment,
         foreign_born,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         state,
         congress_district,
         age)
# Cleaning the data to make it easier for analysis and works with the post-strat data
# indicator variable to determine if someone was born in the US 
reduced_data$foreign_born <- gsub("The United States", 0, reduced_data$foreign_born)
reduced_data$foreign_born <- gsub("Another country", 1, reduced_data$foreign_born)
# indicator to determine if someone is hispanic
reduced_data$hispanic <- ifelse(reduced_data$hispanic == "Not Hispanic", 0, 1)
reduced_data$hispanic <- tolower(reduced_data$hispanic)
# matching the race variables to the ACS race options as those are more limited
reduced_data$race_ethnicity <- gsub("Black, or African American", "black",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Asian Indian)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Chinese)", "chinese",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Filipino)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Japanese)", "japanese",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Korean)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Vietnamese)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Asian \\(Other)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Pacific Islander \\(Native Hawaiian)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Pacific Islander \\(Guamanian)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Pacific Islander \\(Samoan)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Pacific Islander \\(Other)", "other asian or pacific islander",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- gsub("Some other race", "other race",reduced_data$race_ethnicity )
reduced_data$race_ethnicity <- tolower(reduced_data$race_ethnicity)
# Creating age ranges based on other election polls, as leaving age as is may limit the 
# applicability of our findings.
reduced_data$age[(reduced_data$age >= 18 & reduced_data$age <= 24)] <- "18 to 24"
reduced_data$age[(reduced_data$age >= 25 & reduced_data$age <= 34)] <- "25 to 34"
reduced_data$age[(reduced_data$age >= 35 & reduced_data$age <= 44)] <- "35 to 44"
reduced_data$age[(reduced_data$age >= 45 & reduced_data$age <= 54)] <- "45 to 54"
reduced_data$age[(reduced_data$age >= 55 & reduced_data$age <= 64)] <- "55 to 64"
reduced_data$age[(reduced_data$age >= 64)] <- "64+"
# Indicator to determine if someone is voting for Joe Biden
# removing all non Biden and Trump results as realistically only they have a chance
# of winning in every state, and our ultimate focus is on the electoral college results
# rather than popular vote
reduced_data <- 
  reduced_data %>% 
  filter(vote_2020 == "Donald Trump" | vote_2020 == "Joe Biden" ) %>%
  mutate(vote_2020 = 
           ifelse(vote_2020 == "Donald Trump", 0,
                  ifelse(vote_2020 == "Joe Biden", 1, 2)))
# Matching gender to post-strat survey 
reduced_data$gender <- tolower(reduced_data$gender)

### Post-Stratification Data Cleaning ###
# ACS data 
# Filtering to keep the variables that overlap with the survey variables 
reduced_acs <- 
  raw_acs %>% 
  select(region, 
         stateicp,
         sex,
         age,
         race,
         hispan,
         yrimmig, 
         educd,
         empstat,
         ftotinc)
#age, race, hisp, immig, edu, income
# Matching the age ranges to those created previously 
reduced_acs$age <- ifelse(reduced_acs$age == "90 (90+ in 1980 and 1990)", 90, reduced_acs$age)
reduced_acs$age <- as.integer(reduced_acs$age)
reduced_acs$age <- ifelse(reduced_acs$age < 18 , NA , reduced_acs$age)
reduced_acs$age[(reduced_acs$age >= 18 & reduced_acs$age <= 24)] <- "18 to 24"
reduced_acs$age[(reduced_acs$age >= 25 & reduced_acs$age <= 34)] <- "25 to 34"
reduced_acs$age[(reduced_acs$age >= 35 & reduced_acs$age <= 44)] <- "35 to 44"
reduced_acs$age[(reduced_acs$age >= 45 & reduced_acs$age <= 54)] <- "45 to 54"
reduced_acs$age[(reduced_acs$age >= 55 & reduced_acs$age <= 64)] <- "55 to 64"
reduced_acs$age[(reduced_acs$age >= 64)] <- "64+"
# Matching racial categories with the other survey
reduced_acs$race <- gsub("other race, nec", "other race", reduced_acs$race)
reduced_acs$race <- gsub("black/african american/negro", "black", reduced_acs$race)
reduced_acs$race <- gsub("two major races", "other race", reduced_acs$race)
reduced_acs$race <- gsub("three or more major races", "other race", reduced_acs$race)
# Indicator for whether someone is hispanic or not 
reduced_acs$hispan <- ifelse(reduced_acs$hispan == "not hispanic", 0, 1)
# Indicator for being born in the US or outside
reduced_acs$yrimmig <- ifelse(reduced_acs$yrimmig == "n/a", 0, 1)
colnames(reduced_acs)[7] <- "immig"
# Changing the income variables to ranges so to match the election survey
colnames(reduced_acs)[10] <- "inc"
reduced_acs$inc <- ifelse(reduced_acs$inc == 9999999, NA, reduced_acs$inc)
reduced_acs <-
  reduced_acs %>% 
  mutate(inc =
           ifelse(inc <= 14999, "Less than $14,999",
                  ifelse(inc >= 15000 & inc <= 19999, "$15,000 to $19,999",
                         ifelse(inc >= 20000 & inc <= 24999, "$20,000 to $24,999",
                                ifelse(inc >= 25000 & inc <= 34999, "$25,000 to $29,999",
                                       ifelse(inc >= 30000 & inc <= 39999, "$30,000 to $34,999",
                                              ifelse(inc >= 35000 & inc <= 44999, "$35,000 to $39,999",
                                                     ifelse(inc >= 40000 & inc <= 44999, "$40,000 to $44,999",
                                                            ifelse(inc >= 45000 & inc <= 49999, "$45,000 to $49,999",
                                                                   ifelse(inc >= 50000 & inc <= 54999, "$50,000 to $54,999",
                                                                          ifelse(inc >= 55000 & inc <= 59999, "$55,000 to $59,999",
                                                                                 ifelse(inc >= 60000 & inc <= 64999, "$60,000 to $64,999",
                                                                                        ifelse(inc >= 65000 & inc <= 69999, "$65,000 to $69,999",
                                                                                               ifelse(inc >= 70000 & inc <= 74999, "$70,000 to $74,999",
                                                                                                      ifelse(inc >= 75000 & inc <= 79999, "$75,000 to $79,999",
                                                                                                             ifelse(inc >= 80000 & inc <= 84999, "$80,000 to $84,999",
                                                                                                                    ifelse(inc >= 85000 & inc <= 89999, "$85,000 to $89,999",
                                                                                                                           ifelse(inc >= 90000 & inc <= 94999, "$90,000 to $94,999",
                                                                                                                                  ifelse(inc >= 95000 & inc <= 99999, "$95,000 to $99,999",
                                                                                                                                         ifelse(inc >= 100000 & inc <= 124999, "$100,000 to $124,999",
                                                                                                                                                ifelse(inc >= 125000 & inc <= 149999, "$125,000 to $149,999",
                                                                                                                                                       ifelse(inc >= 150000 & inc <= 174999, "$150,000 to $174,999",
                                                                                                                                                              ifelse(inc >= 175000 & inc <= 199999, "$175,000 to $199,999",
                                                                                                                                                                     ifelse(inc >= 200000 & inc <= 249999, "$200,000 to $249,999",
                                                                                                                                                                            ifelse(inc >= 250000, "$250,000 and above", 0
                                                                                                                                                                            ))))))))))))))
                                                                          ))))))))))) %>% 
  filter(inc != 0)


# Creating unified state names between the two data sets
names_matcher <- tibble(stateicp = state.name, state = state.abb)
reduced_data <- 
  reduced_data %>% 
  left_join(names_matcher)


### Output ###
# Creating data frames for our final variables 
# For election survey
VSG <- 
  data.frame(
    state = reduced_data$stateicp,
    gender = reduced_data$gender,
    age = reduced_data$age,
    race = reduced_data$race_ethnicity,
    hispanic = reduced_data$hispanic,
    foreign = reduced_data$foreign_born, 
    income = reduced_data$household_income,
    vote_biden = reduced_data$vote_2020)
VSG <- na.omit(VSG) 
# For ACS data
ACS <- 
  data.frame(
    state = reduced_acs$stateicp,
    gender = reduced_acs$sex,
    age = reduced_acs$age,
    race = reduced_acs$race,
    hispanic = reduced_acs$hispan,
    foreign = reduced_acs$immig, 
    income = reduced_acs$inc
  )
ACS <- na.omit(ACS)
# Creating a data frame with all possible cells for post-stratification
cells <- ACS %>%
  group_by(state, gender, age, race, hispanic, foreign, income) %>%
  count()
# Adding a column that includes the proportion of each cell relative to state population
# taken from make_abs_data file from Rohan's example scripts 
cells <- 
  cells %>% 
  group_by(state) %>% 
  mutate(total = sum(n)) %>% 
  mutate(proportion = n / total) %>% 
  ungroup() %>% 
  select(-total)

# Writing the final products to CSV files for analysis 
write.csv(VSG, 'VSG.csv')
write.csv(cells, 'PS-cells.csv')
