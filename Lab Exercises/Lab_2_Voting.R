ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
options(warn = -1)
library(tidyverse)


# Exercise 1: Let's look at voting#
#Let's ask a simple question: do people vote?

ESS %>% filter(!is.na(vote1)) %>% count(vote1) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('voted', 'N', '%'), align="lccc")

#How does this vary by country?#

ESS %>% group_by(cntry) %>% filter(!is.na(vote1)) %>%
  count(vote1) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Electoral Turnout in European Democracies",
  col.names = c('Country', 'Voted', 'N', '%'), align="cccc")

#We can present this graphically to make it clearer#
ESS %>% group_by(cntry) %>% filter(!is.na(vote1)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!vote1=="did not vote") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 1: Turnout by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()

#Exercise 2: Let's test our theories#
#Q1: Do Socio-Economic Resources Matter#
#What about Education?#

ESS%>% group_by(educat) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate("%" =n/sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Education and Electoral Turnout",
  col.names = c('Education', 'Voted', 'N', '%'), align="ccccc", digits=1)

#We can graph the relationship

ESS%>% group_by(educat) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 2: Turnout by Education", caption="ESS 2016")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  geom_bar(stat="identity", position="dodge")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")


#Turnout by Education and Country#

ESS%>% group_by(educat, cntry) %>% filter(!is.na(vote1), !is.na(educat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=educat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 3: Turnout by Education and Country", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~cntry, nrow=2)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")

#Q2: Does Rationality Influence Turnout?

ESS %>% group_by(econsat) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop= n /sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Perceptions of the Economy and Electoral Turnout",
  col.names=c('Economic Perceptions', 'Voted', 'N', '%'), align="ccccc", digits=1)

#We can graph this relationship

ESS%>% group_by(econsat) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=econsat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 4: Turnout by Perceptions of the Economy", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("dissatisfied" = "dissatisfied", "neither dissatisfied nor satisfied" = "neither", "satisfied" = "satisfied"))


#Does it differ between the countries?

ESS%>% group_by(econsat, cntry) %>% filter(!is.na(vote1), !is.na(econsat)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>% mutate("%" =prop*100) %>%
  ggplot(aes(x=econsat, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 5: Turnout by Economic Satisfaction and Country", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~cntry, nrow=2)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("dissatisfied" = "dissatisfied", "neither dissatisfied nor satisfied" = "neither", "satisfied" = "satisfied"))


#Q3 Does culture Matter?

ESS %>% group_by(ptrust) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop= n / sum(n)*100) %>%
  knitr::kable("pandoc", caption = "Trust in Politics and Electoral Turnout",
  col.names=c('Trust', 'Voted', 'N', '%'), align="ccccc", digits=1)

ESS%>% group_by(ptrust) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 6: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))

ESS%>% group_by(ptrust, cntry) %>% filter(!is.na(vote1), !is.na(ptrust)) %>%
  count(vote1) %>% mutate(prop=prop.table(n*100)) %>%
  ggplot(aes(x=ptrust, y=prop, fill=vote1))+
  labs(x="", y="", title="Figure 7: Turnout by Trust in Politics", caption="ESS 2016")+
  geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("#FF0000", "#0000FF"))+
  facet_wrap(~cntry, nrow=2)+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  theme(legend.title = element_blank())+
  theme(legend.position = "bottom")+
  scale_x_discrete(labels=c("low" = "low", "medium" = "medium", "high" = "high"))


# Bonus Question: Let's ask if young people vote. 
# We can do this by comparing the percentage of young people found in the sample with the percentage found amongst the voters?

a <- ESS %>% 
filter(!is.na(age)) %>% 
mutate(generation = if_else(age >= 18 & age <= 25, "18-25", "26+")) %>% 
count(generation) %>% 
mutate(percent = round(n /sum(n)*100, digits = 1)) 

b <- ESS %>% 
filter(!is.na(age)) %>% 
mutate(generation = if_else(age >= 18 & age <= 25, "18-25", "26+")) %>% 
filter(vote1 == "voted") %>% 
group_by(generation) %>% 
count(vote1) %>% 
ungroup() %>% 
mutate(percent = round(n/sum(n)*100, digits = 1))

left_join(a, b, by = "generation") %>% 
select(-c(n.x, n.y, vote1)) %>% 
knitr::kable("pandoc", 
             caption = "Percentage of Age within Sample and Percentage of Age Cohort amongst Voters",
             col.names = c('Generation', 
                           'Percentage Within Sample', 
                           'Percentage Within Voters'), 
             align="ccc") %>% 
print()

# Three questions to conclude with:

      #So, thinking over the theories we've look at: which ones work?

      #Do all work equally well in all countries?

      #Does it matter that young people seem less likely to vote?
