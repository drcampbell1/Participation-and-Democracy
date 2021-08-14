# As we've discussed in the lectures, there are other forms of protest politics.
# In this lecture, we're going to focus on petitioning and boycotting.
# These are important because people may show their support for a group or cause by signing a public
# petition supporting it, or they may boycott products for political or ethical reasons.


ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
attach(ESS)
library(tidyverse)
options(warn = -1)

# Question 1: How common is petitioning in European democracies?

ESS %>% filter(!is.na(petition)) %>% count(petition) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Petitioning in European Democracies",
               col.names = c('Petitioning', 'N', '%'), align="lcc")

# Let's break this down by the six countries for which we have data:

ESS %>% filter(!is.na(petition)) %>% group_by(cntry) %>% count(petition) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Petitioning in European Democracies",
               col.names = c('Country', 'Petitioning', 'N', '%'), align="lccc")

# We can graph this to make it clear

ESS %>% group_by(cntry) %>% filter(!is.na(petition)) %>%
  count(petition) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petition=="have not signed petition") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 1: Petitioning by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# Question 2: How common is boycotting in European democracies?

ESS %>% filter(!is.na(boyct)) %>% count(boyct) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Boycotting in European Democracies",
               col.names = c('Boycotting', 'N', '%'), align="lcc")

# Let's break this down by the six countries for which we have data:

ESS %>% filter(!is.na(boyct)) %>% group_by(cntry) %>% count(boyct) %>%
  mutate(percent = round(n/sum(n)*100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Boycotting in European Democracies",
               col.names = c('Country', 'Boycotting', 'N', '%'), align="lccc")

# We can graph this to make it clear

ESS %>% group_by(cntry) %>% filter(!is.na(boyct)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="have not boycotted") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 2: Boycotting by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# Section 2: Who takes part in these forms of participation?

# What is the age profile of petitioners?

ESS %>% group_by(agecat) %>% filter(!is.na(petition), !is.na(agecat)) %>%
  count(petition) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petition=="have not signed petition") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 3: Petitioning by Age Category", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the age profile of people who boycott?

ESS %>% group_by(agecat) %>% filter(!is.na(boyct), !is.na(agecat)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="have not boycotted") %>%
  ggplot(aes(x=agecat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Boycotting by Age Category", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)



# What is the education profile of petitioners?

ESS %>% group_by(educat) %>% filter(!is.na(petition), !is.na(educat)) %>%
  count(petition) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petition=="have not signed petition") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 5: Petitioning by Education", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What is the education profile of people who boycott?

ESS %>% group_by(educat) %>% filter(!is.na(boyct), !is.na(educat)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="have not boycotted") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 6: Boycotting by Education", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# What is the gender profile of petitioners?

# Males only?

ESS %>% group_by(gender) %>% filter(!is.na(petition), !is.na(gender)) %>%
  count(petition) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!petition=="have not signed petition") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 7: Petitioning by Gender", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(gender) %>% filter(!is.na(boyct), !is.na(gender)) %>%
  count(boyct) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!boyct=="have not boycotted") %>%
  ggplot(aes(x=gender, y=prop)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 8: Boycotting by Gender", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What, then, might you conclude about protest politics on the basis of what we've looked at here?

# Do very few people take part?

# Are they concentrated in one age group?

# Are they better educated?

# Is there gender imbalance?
