# In the lecture I argued that demonstrating has expanded and diversified as a form of participation.
# Whilst it was once relatively uncommon, it has now grown in frequency.
# Whilst it was once the preserve of younger and more ideologically extreme males, it is now
# used by a wide demographic.

# This will become clear as we look at the data.

ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
attach(ESS)
library(tidyverse)
options(warn = -1)

# Question 1: How common is demonstrating in European democracies?

ESS %>% filter(!is.na(demo)) %>% count(demo) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Demonstrating in European Democracies",
  col.names = c('Demonstrating', 'N', '%'), align="lcc")

# Let's break this down by the six countries for which we have data:

ESS %>% filter(!is.na(demo)) %>% group_by(cntry) %>% count(demo) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Demonstrating in European Democracies",
  col.names = c('Country', 'Demonstrating', 'N', '%'), align="lccc")

# We can graph this to make it clear

ESS %>% group_by(cntry) %>% filter(!is.na(demo)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="have not demonstrated") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Demonstrating by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the age pattern of the demonstrators?

ESS %>% group_by(agecat) %>% filter(!is.na(demo), !is.na(agecat)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="have not demonstrated") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 2: Demonstrating by Age Category", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the education profile of demonstrators?

ESS %>% group_by(educat) %>% filter(!is.na(demo), !is.na(educat)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="have not demonstrated") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: Demonstrating by Education", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Males only?

ESS %>% group_by(gender) %>% filter(!is.na(demo), !is.na(gender)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="have not demonstrated") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Demonstrating by Gender", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(uemp) %>% filter(!is.na(demo), !is.na(uemp)) %>%
  count(demo) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!demo=="have not demonstrated") %>%
  ggplot(aes(x=uemp, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 5: Demonstrating by Unemployed", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What does all of the above tell us about demonstrators?


# Question 2: Do demonstrators participate?

ESS %>% filter(!is.na(demo), !is.na(vote1)) %>%
  group_by(demo) %>% count(vote1) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote1 == "did not vote") %>%
  knitr::kable("pandoc", caption = "Demonstrating and Voting",
  col.names = c('Demonstrating', 'Voting', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()

ESS %>% filter(!is.na(demo), !is.na(vote1)) %>%
  group_by(demo, cntry) %>% count(vote1) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!vote1 == "did not vote") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 6: Demonstrating and Voting by Country", caption="ESS 2016")+
  theme_bw()+
  facet_wrap(~cntry, nrow = 2)+
  guides(fill=FALSE)

 # Petitioning 

ESS %>% filter(!is.na(demo), !is.na(petition)) %>%
  group_by(demo) %>% count(petition) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petition == "have not signed petition") %>%
  knitr::kable("pandoc", caption = "Demonstrating and Petition",
  col.names = c('Demonstrating', 'Petition', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()

# Petitioning across countries by demonstrating

ESS %>% filter(!is.na(demo), !is.na(petition)) %>%
  group_by(demo, cntry) %>% count(petition) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!petition == "have not signed petition") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 8: Petitioning by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_wrap(~cntry, nrow = 2)+
  guides(fill=FALSE)

# Contacting by Demonstrating

ESS %>% filter(!is.na(demo), !is.na(contact)) %>%
  group_by(demo) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "have not contacted") %>%
  knitr::kable("pandoc", caption = "Contacting and Petition",
  col.names = c('Demonstrating', 'Contacting', 'N', 'Percentage'),
  digits=2, align="cccc") %>% print()


# Contacting across countries by demonstrating

ESS %>% filter(!is.na(demo), !is.na(contact)) %>%
  group_by(demo, cntry) %>% count(contact) %>%
  mutate(prop=n/sum(n)*100) %>%
  filter(!contact == "have not contacted") %>%
  ggplot(aes(x=demo, y=prop)) +
  geom_bar(stat="identity", position = "dodge")+
  labs(x="", y="%", title="Figure 9: Contacting by Socialising and Country", caption="ESS 2016")+
  theme_bw()+
  facet_wrap(~cntry, nrow = 2)+
  guides(fill=FALSE)

# So what can we conclude overall about demonstrators? Does it look like it is a small group of people who demonstrate
# or a more homogenous section of the population? 
