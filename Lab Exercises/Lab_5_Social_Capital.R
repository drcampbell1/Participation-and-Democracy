# In this lab, we're going to follow-up and examine some of the trends in social capital.
#Of all the forms of participation, this is by far the hardest to measure. So we need
# remember that our findings are limited.

# Let's start by looking at the data:

ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
attach(ESS)
library(tidyverse)
options(warn = -1)

# Question 1: How social are we?

# We're going to start by looking at the variable "social". This is a question asking how
# frequently people met with their friends.

ESS %>% filter(!is.na(social)) %>% count(social) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Socialise with Friends in European Democracies",
  col.names = c('Socialise', 'N', '%'), align="lccc")

# Fewer people socialise less than once a week, so let's find out how numerous the more socially active people
# are in our countries. To do this I've created a new variable "socialise", which combines never/rare and 1-2 per month into one
# category and 1 per week and more than once per week into one category.

ESS %>% filter(!is.na(socialise)) %>% group_by(cntry) %>%
              count(socialise) %>% mutate(percent = round(n/sum(n)*100, digits=1)) %>%
              knitr::kable("pandoc", caption = "Socialise with Friends in European Democracies",
              col.names = c('Country', 'Socialise', 'N', '%'), align="lcccc")

# We can graph this to make it clear

ESS %>% group_by(cntry) %>% filter(!is.na(socialise)) %>%
  count(socialise) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!socialise=="rare") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop, fill=cntry)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#E69F00", "#009E73", "#0072B2", "#D55E00", "#56B4E9", "#CC79A7"))+
  labs(x="", y="", title="Figure 1: Socialising by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Do the figures differ by age category?

ESS %>% group_by(agecat) %>% filter(!is.na(socialise), !is.na(agecat)) %>%
  count(socialise) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!socialise=="rare") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 2: Socialising by Age Category", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# Do the figures differ by Education

ESS %>% group_by(educat) %>% filter(!is.na(socialise), !is.na(educat)) %>%
  count(socialise) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!socialise=="rare") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: Socialising and Education", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# And Gender
ESS %>% group_by(gender) %>% filter(!is.na(socialise)) %>%
  count(socialise) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!socialise=="rare") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Socialising and Gender", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Section 2: Does this influence participation?

# Although we have established the people are relatively social, one might ask: does it matter?

# Does the propensity to be socially active spur participation? And does it vary across countries?

# Voting

ESS %>% filter(!is.na(socialise), !is.na(vote1)) %>%
  group_by(socialise) %>% count(vote1) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote1 == "did not vote") %>%
  knitr::kable("pandoc", caption = "Socialising and Voting",
  col.names = c('Socialising', 'Voting', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()

ESS %>% filter(!is.na(socialise), !is.na(vote1)) %>%
  group_by(socialise, cntry) %>% count(vote1) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote1 == "did not vote") %>%
  ggplot(aes(x=socialise, y=prop, fill=vote1)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 5: Voting by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_grid(~cntry)+
  guides(fill=FALSE)

# Contacting

ESS %>% filter(!is.na(socialise), !is.na(contact)) %>%
  group_by(socialise) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "have not contacted") %>%
  knitr::kable("pandoc", caption = "Socialising and Contacting Politicians",
  col.names = c('Socialising', 'Contacting', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()


# Contacting across countries

ESS %>% filter(!is.na(socialise), !is.na(contact)) %>%
  group_by(socialise, cntry) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "have not contacted") %>%
  ggplot(aes(x=socialise, y=prop, fill=contact)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 6: Contacting by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_grid(~cntry)+
  guides(fill=FALSE)

# Petition

ESS %>% filter(!is.na(socialise), !is.na(petition)) %>%
  group_by(socialise) %>% count(petition) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petition == "have not signed petition") %>%
  knitr::kable("pandoc", caption = "Socialising and Petition",
  col.names = c('Socialising', 'Petition', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()

# Petitioning across countries by socialising

ESS %>% filter(!is.na(socialise), !is.na(petition)) %>%
  group_by(socialise, cntry) %>% count(petition) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petition == "have not signed petition") %>%
  ggplot(aes(x=socialise, y=prop, fill=petition)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 7: Petitioning by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_grid(~cntry)+
  guides(fill=FALSE)


# Demonstrating

ESS %>% filter(!is.na(socialise), !is.na(demo)) %>%
  group_by(socialise) %>% count(demo) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!demo == "have not demonstrated") %>%
  knitr::kable("pandoc", caption = "Socialising and Demonstrating",
  col.names = c('Socialising', 'Demonstrating', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()

# Demonstrating Across Countries

ESS %>% filter(!is.na(socialise), !is.na(demo)) %>%
  group_by(socialise, cntry) %>% count(demo) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!demo == "have not demonstrated") %>%
  ggplot(aes(x=socialise, y=prop, fill=demo)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 3: Demonstrating by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_grid(~cntry)+
  guides(fill=FALSE)

# So, time to draw some conclusions.
# Does social capital matter?

