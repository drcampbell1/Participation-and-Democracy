#Introduction to the labs#

# I mentioned that the study of participation involves looking at evidence.
# Most of the questions are about: levels; styles; and factors that influence participation
# In these labs, we"ll look at the evidence.


#To do that, we need data and software to analyse it. These labs use RStudio to analyse the data.
# This is a programming-based environment, but I've written all of the code for you.
#All you need to do is run it.

#Let's start by looking at the data. Run the following line:

ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
options(warn = -1)
library(tidyverse)

#Introduction to the data#

#In the labs we'll be using the ESS. This is the European Social Survey -
#a big cross-sectional survey conducted in 30 countries in Europe.


#Coverage
# To make the file more manageable, I've cut it down quite a bit.

#Basic Terminology#

                  #1. Dependent variable: something we are looking to explain, e.g. one (or more) forms of participation:

                  #2. Independent variable : something that influences our dependent variable (age, for example, may influence voting).


                  #3. Observation: someone we ask about their participation and age.

#Overall, then, this file contains :

                      # 8 different forms of participation;

                        # 8 different variables that may influence participation (or 9 if we include country)

                        # In total we have 12472 observations - or 12472 people gave us answers to our questions.

ESS %>% 
count(cntry, sort=TRUE)
nrow(ESS)
#Let's look at the data file and see what forms of participation it contains#

View(ESS)
#We have:

          #1. Voting;

          #2. Contacting politicians;

          #3 working for a political party;

          #4 wearing political paraphenalia;

          #5. signing a petition;

          #6. demonstrating;

          #7. boycotting;

          #8 voting for the radical right


#And we also have variables that might influence these forms of participation, including:

                #1. Age; 2. Education; 3. gender; 4. unemployed. These are *structural* characteristics.


                #And we can also look at the attitudes people hold shapes their participation. We have attitudes on:

                # Economic satisfaction; trust in politics; trust in European parliament.


#Let's finish today by looking at some forms of participation:

#How many people said they voted at the last national election?

          ESS %>% 
          filter(!is.na(vote1)) %>% 
          count(vote1)

#Percentages are easier, so let's add them in:

          ESS %>% 
          filter(!is.na(vote1)) %>% 
          count(vote1) %>% 
          mutate('%' = round(n/sum(n)*100, digits=1))

#Let's look at contacting politicians

          ESS %>% 
          filter(!is.na(contact)) %>% 
          count(contact) %>% 
          mutate('%' = round(n/sum(n)*100, digits=1))

#Lets look at demonstrating

          ESS %>% 
          filter(!is.na(demo)) %>% 
          count(demo) %>% 
          mutate('%' = round(n/sum(n)*100, digits=1))

# Let's look at voting for the radical right

          ESS %>% 
          filter(!is.na(rright)) %>% 
          count(rright) %>% 
          mutate('%' = round(n/sum(n)*100, digits=1))


# One graph to finish: let's compare the countries across the different forms of participation.

ESS %>% 
  drop_na() %>% 
  pivot_longer((vote1:demo), names_to = "mode", values_to = "value") %>% 
  group_by(cntry, mode) %>% 
  count(value) %>% 
  mutate(percent = n/sum(n)*100) %>% 
  filter(value==str_remove(value, "not")) %>% 
  mutate(value = fct_reorder(value, percent)) %>% 
  ggplot(aes(reorder(value, percent), percent))+
  geom_col(fill="steelblue")+
  facet_wrap(~cntry, scales = "free_x")+
  coord_flip()+
  theme_minimal()+
  scale_x_discrete(labels = c("party worker", 
                               "demonstrated", 
                              "worn badge", 
                              "contacted", 
                              "petitioned",
                              "voted"))+
  labs(x="", 
       y = "%", 
       title = "Comparing Political Participation Across\nEuropean Democracies (%)", 
       caption = "Source: European Social Survey")

# From which country do you think people are most engaged? 
