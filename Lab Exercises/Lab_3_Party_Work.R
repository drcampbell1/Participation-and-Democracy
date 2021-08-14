# In the lecture, we found a particularly challenging picture of political parties. We found:

                  # (1) weakening attachments

                  # (2) falling (and concentrating) membership; and

                  # (3) Negative public images.

# In this lab, we're going to look at who works for political parties,
# examining their socio-economic background to see if there is inequality.

ESS <- foreign::read.dta("data/ESS.dta", convert.factors=TRUE)
attach(ESS)
library(tidyverse)
options(warn = -1)

#Let's begin by looking at the general levels of working for parties:

ESS %>% filter(!is.na(wparty)) %>% count(wparty) %>%
  mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Work for Party in European Democracies",
  col.names = c('Worked', 'N', '%'), align="lccc")

#How does this vary by country?#

ESS %>% group_by(cntry) %>% filter(!is.na(wparty)) %>%
  count(wparty) %>% mutate("%" = round(n/sum(n) * 100, digits=1)) %>%
  knitr::kable("pandoc", caption = "Comparing Work for Party in European Democracies",
  col.names = c('Country', 'Worked', 'N', '%'), align="cccc")

# Let's graph this to make the findings stand out a bit better

ESS %>% group_by(cntry) %>% filter(!is.na(wparty)) %>%
  count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!wparty=="have not worked for pp") %>%
  ggplot(aes(x=reorder(cntry, -prop), y=prop, fill=cntry)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("#999999", "#000000", "#404040", "#BABABA", "#333333", "#CCCCCC"))+
  labs(x="", y="", title="Figure 1: Worked for Party by Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

# How does the age of the population differ from that of workers of political parties?

a <- ESS %>% group_by(cntry) %>% summarise(mean= mean(age, na.rm=T))
b <- ESS %>% group_by(cntry, wparty) %>% summarise(avg_age_pp= mean(age, na.rm=T)) %>% filter(!is.na(wparty), !wparty=="have not worked for pp")
left_join(a, b, by = "cntry") %>% select(-wparty) %>%
     knitr::kable("pandoc", caption = "Average Population and Workers of Parties in European Democracies",
     col.names = c('Country', 'Average Age Population', 'Average Age Worker Political Party'), digits =1, align="cccc") %>% print()

# Is there gender balance amongst workers of political parties compared with the population?#

c <- ESS %>% group_by(cntry) %>%
  count(gender) %>%
  mutate(perc_pop=n/sum(n)*100)
d <- ESS %>% filter(wparty=="have worked for pp") %>% group_by(cntry, gender) %>%
       count(wparty) %>% group_by(cntry) %>% mutate(perc_wk_pp=n/sum(n)*100) %>%
       select(-wparty)
left_join(c, d, by=c("cntry", "gender")) %>% select(-n.x, -n.y) %>%
  knitr::kable("pandoc", caption = "Gender Balance in Population and Workers of Parties in European Democracies",
               col.names = c('Country', 'Gender', 'Population', 'Workers of Political Party'),
               digits =1, align="cccc") %>% print()

#Let's take a look at Education#


ESS %>% group_by(educat) %>% filter(!is.na(wparty), !is.na(educat)) %>%
       count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
       filter(!wparty=="have not worked for pp") %>%
       ggplot(aes(x=educat, y=prop, fill=educat)) +
       geom_bar(stat="identity")+
       labs(x="", y="", title="Figure 2: Worked for Party by Education", caption="ESS 2016")+
       scale_y_continuous(labels=scales::percent)+
       theme_bw()+
       guides(fill=FALSE)

ESS %>% group_by(educat, cntry) %>% filter(!is.na(wparty), !is.na(educat)) %>%
  count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!wparty=="have not worked for pp") %>%
  ggplot(aes(x=educat, y=prop, fill=educat)) +
  geom_bar(stat="identity")+
  facet_grid(~cntry)+
  labs(x="", y="", title="Figure 3: Worked for Party by Education and Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)+
  scale_x_discrete(labels=c("Basic" = "HS", "University degree" = "Uni", "postgrad" = "pg"))


#Economically Optimistic Party Workers?

ESS %>% group_by(econsat) %>% filter(!is.na(wparty), !is.na(econsat)) %>%
  count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!wparty=="have not worked for pp") %>%
  ggplot(aes(x=econsat, y=prop, fill=econsat)) +
  geom_bar(stat="identity")+
  labs(x="", y="", title="Figure 4: Worked for Party by Economic Optimism", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)

ESS %>% group_by(econsat, cntry) %>% filter(!is.na(wparty), !is.na(econsat)) %>%
  count(wparty) %>% mutate(prop=prop.table(n*100)) %>%
  filter(!wparty=="have not worked for pp") %>%
  ggplot(aes(x=econsat, y=prop, fill=econsat)) +
  geom_bar(stat="identity")+
  facet_grid(~cntry)+
  labs(x="", y="", title="Figure 5: Worked for Party by Economic Optimism and Country", caption="ESS 2016")+
  scale_y_continuous(labels=scales::percent)+
  theme_bw()+
  guides(fill=FALSE)+
  scale_x_discrete(labels=c("dissatisfied" = "dis", "neither dissatisfied nor satisfied" = "neither", "satisfied" = "sat"))

rm(a,b,c,d)


# What can we conclude about people who work for parties? Are they a model of equality and diversity?
