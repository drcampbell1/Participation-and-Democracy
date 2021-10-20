# In the lecture, we introduced theories about who votes for the radical right

# In this lab, we're going to test some aspects of those theories with evidence from the ESS

ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
attach(ESS)
library(tidyverse)
options(warn = -1)

ESS %>% filter(!is.na(rright)) %>% count(rright) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Voted for the Radical Right in European Democracies",
               col.names = c('Voted Radical Right', 'N', '%'), align="lccc")

#How does this vary by country?#

ESS %>% group_by(cntry) %>% filter(!is.na(rright)) %>%
     count(rright) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>% filter(!rright =="no") %>% 
     knitr::kable("pandoc", caption = "Comparing Voting for Radical Right in European Democracies",
     col.names = c('Country', 'Voted Radical Right', 'N', '%'), align="cccc")


# Let's graph this to make the findings stand out a bit better


ESS %>% group_by(cntry) %>% filter(!is.na(rright)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop)) +
  geom_col()+
  geom_text(aes(label = round(prop*100, digits = 1)), color = "white", vjust = 1.2)+
  labs(x="", y="", title="Figure 1: Voted for Radical Right Party by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# What is the age profile of voters for the radical right?

a <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>% 
             group_by(cntry) %>% 
             summarise(mean= mean(age, na.rm=T))
b <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>%
             group_by(cntry, rright) %>% summarise(avg_age_vt_rr= mean(age, na.rm=T)) %>% 
             filter(!is.na(rright), !rright=="no")

left_join(a, b, by = "cntry") %>% select(-rright) %>%
          knitr::kable("pandoc", caption = "Average Age of Population and Voters of Radical Right Parties in European Democracies",
          col.names = c('Country', 'Average Age Population', 'Average Age Voter Radical Right Party'), digits =1, align="cccc")

# Let's look at the gender profile of radical right voters


c <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>%
  group_by(cntry) %>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)


d <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>%
  filter(!is.na(rright)) %>%
  group_by(cntry, gender) %>% 
  filter(!rright=="no") %>% count(rright) %>% 
  group_by(cntry) %>% 
  mutate(perc_vt_pp=n/sum(n)*100) %>% 
  select(-rright)

left_join(c, d, by=c("cntry", "gender")) %>% select(-n.x, -n.y) %>%
  knitr::kable("pandoc", caption = "Gender Balance in Population and Voters of Radical Right Parties in European Democracies",
               col.names = c('Country', 'Gender', 'Percentage Population', 'Percentage Voter of Radical Right Political Party'),
               digits =1, align="cccc") %>% print()

# Unemployment
e <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>%
      filter(!is.na(uemp)) %>%  
      group_by(cntry) %>%
       count(uemp) %>%
       mutate(perc_pop=n/sum(n)*100)

f <- ESS %>% filter(cntry %in% c("Germany", "France", "UK")) %>%
  filter(!is.na(rright), !is.na(uemp)) %>%
  group_by(cntry, uemp) %>% 
  filter(!rright=="no") %>% count(rright) %>% 
  group_by(cntry) %>% 
  mutate(perc_vt_pp=n/sum(n)*100) %>% 
  select(-rright)

left_join(e, f, by=c("cntry", "uemp")) %>% select(-n.x, -n.y) %>%
  knitr::kable("pandoc", caption = "Unemployment and Voters of Radical Right Parties in European Democracies",
               col.names = c('Country', 'Unemployemnt', 'Percentage Population', 'Percentage Voter of Radical Right Political Party'),
               digits =1, align="cccc") %>% print()


#Let's look at Education

ESS %>% group_by(educat) %>% filter(!is.na(rright), !is.na(educat)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 2: Voting for the Radical Right by Education", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(educat, cntry) %>% filter(!is.na(rright), !is.na(educat)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=educat, y=prop)) +
  geom_col()+
  facet_wrap(~cntry, nrow=2)+
  labs(x="", y="", title="Figure 3: Voting for the Radical Right by Education and Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

#Let's look at what voters of the radical right think about government?

#Do voters of the radical right trust government?

ESS %>% group_by(ptrust) %>% filter(!is.na(rright), !is.na(ptrust)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=ptrust, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 4: Political Trust and Voting for the Radical Right", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(ptrust, cntry) %>% filter(!is.na(rright), !is.na(ptrust)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=ptrust, y=prop)) +
  geom_col()+
  facet_wrap(~cntry, nrow=2)+
  labs(x="", y="", title="Figure 5: Political Trust and Voting for the Radical Right", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)


# Trust in European Parliament

ESS %>% group_by(tr_ep) %>% filter(!is.na(rright), !is.na(tr_ep)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=tr_ep, y=prop)) +
  geom_col()+
  labs(x="", y="", title="Figure 6: Trust in the European Parliament and Voting for the Radical Right", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(tr_ep, cntry) %>% filter(!is.na(rright), !is.na(tr_ep)) %>%
  count(rright) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!rright=="no") %>%
  ggplot(aes(x=tr_ep, y=prop)) +
  geom_col()+
  facet_wrap(~cntry, nrow=2)+
  labs(x="", y="", title="Figure 7: Trust in the European Parliament and Voting for the Radical Right", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# What can we conclude about voters for the radical right?
