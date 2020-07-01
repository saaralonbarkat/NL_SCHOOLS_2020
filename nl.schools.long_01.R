#library(sjstats)
library(tidyverse)
library(car)


# Aggregated dataset for the two studies

t1 <- school.s2_01.full %>% 
  dplyr::select(-Q30.8,-Q30.10) %>% 
  mutate(study = 2)
t2 <- school.s1_01 %>% 
  dplyr::select(-Q30.8,-Q30.10)%>% 
  mutate(study = 1)


school.all <- t1 %>% 
  full_join(t2)



# Making long datasets:

## Study 1: long

school.s1_01.long <- school.s1_01 %>%
  select(pid,score.teacher.1:score.teacher.3,
         decision,
         advice,
         mobile,
         female,
         age,
         income,
         man.check.all,
         reaction.time) %>% 
  gather(key="key",value = "ILE",-pid,-decision,-advice,-man.check.all,-mobile,-female,-age,-income,-reaction.time) %>%
  arrange(pid) %>% 
  mutate(ILE = factor(ILE),
         teacher = key %>% Recode("'score.teacher.1'=1;
                                  'score.teacher.2'=2;
                                  'score.teacher.3'=3"),
         teacher.fired = case_when(
           teacher==1 & decision==1 ~ 1,
           teacher==2 & decision==2 ~ 1,
           teacher==3 & decision==3 ~ 1,
           TRUE ~ 0)) %>% 
  mutate(teacher = factor(teacher)) %>% 
  select(pid,
         teacher,
         ILE,
         teacher.fired,
         advice,
         female,
         age,
         income,
         man.check.all,
         mobile,
         reaction.time) %>% 
  mutate(teacher.lab = teacher %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"),
         advice.algorithm = advice %>% Recode("'algorithm'=1;'human'=0"))





## Study 2: long

school.s2_01.long <- school.s2_01 %>%
  select(pid,score.teacher.1:score.teacher.3,
         decision,
         moroccan.teacher.cond,
         advice,
         mobile,
         female,
         age,
         income,
         man.check.all,
         ethnicity.dutch,
         reaction.time,
         Q29.3) %>% 
  gather(key="key",value = "ILE",-pid,-decision,-moroccan.teacher.cond,-advice,-man.check.all,-mobile,-female,-age,-income,-ethnicity.dutch,-reaction.time,-Q29.3) %>%
  arrange(pid) %>% 
  mutate(ILE = factor(ILE),
         teacher = key %>% Recode("'score.teacher.1'=1;
                                  'score.teacher.2'=2;
                                  'score.teacher.3'=3"),
         moroccan = case_when(
           teacher==1 & moroccan.teacher.cond==1 ~ 1,
           teacher==2 & moroccan.teacher.cond==2 ~ 1,
           teacher==3 & moroccan.teacher.cond==3 ~ 1,
           TRUE ~ 0),
         moroccan.teacher.cond.1 = case_when(
           moroccan.teacher.cond==0 ~ "Dutch (no moroccans)",
           teacher==1 & moroccan.teacher.cond==1 ~ "Moroccan",
           teacher==2 & moroccan.teacher.cond==2 ~ "Moroccan",
           teacher==3 & moroccan.teacher.cond==3 ~ "Moroccan",
           TRUE ~ "Dutch (other teacher Moroccan)"),
         teacher.fired = case_when(
           teacher==1 & decision==1 ~ 1,
           teacher==2 & decision==2 ~ 1,
           teacher==3 & decision==3 ~ 1,
           TRUE ~ 0)) %>% 
  mutate(teacher = factor(teacher),
         moroccan.lab = moroccan %>% Recode("0='Dutch';
                                            1='Moroccan'")) %>% 
  select(pid,
         teacher,
         ILE,
         teacher.fired,
         moroccan,
         moroccan.lab,
         moroccan.teacher.cond.1,
         advice,
         female,
         age,
         income,
         man.check.all,
         mobile,
         ethnicity.dutch,
         reaction.time,
         Q29.3) %>% 
  mutate(teacher.lab = teacher %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"),
         advice.algorithm = advice %>% Recode("'algorithm'=1;'human'=0"))


school.s2_01.full.long <- school.s2_01.full %>%
  select(pid,score.teacher.1:score.teacher.3,
         decision,
         moroccan.teacher.cond,
         advice,
         mobile,
         female,
         age,
         income,
         man.check.all,
         ethnicity.dutch,
         reaction.time) %>% 
  gather(key="key",value = "ILE",-pid,-decision,-moroccan.teacher.cond,-advice,-man.check.all,-mobile,-female,-age,-income,-ethnicity.dutch,-reaction.time) %>%
  arrange(pid) %>% 
  mutate(ILE = factor(ILE),
         teacher = key %>% Recode("'score.teacher.1'=1;
                                  'score.teacher.2'=2;
                                  'score.teacher.3'=3"),
         moroccan = case_when(
           teacher==1 & moroccan.teacher.cond==1 ~ 1,
           teacher==2 & moroccan.teacher.cond==2 ~ 1,
           teacher==3 & moroccan.teacher.cond==3 ~ 1,
           TRUE ~ 0),
         moroccan.teacher.cond.1 = case_when(
           moroccan.teacher.cond==0 ~ "Dutch (no moroccans)",
           teacher==1 & moroccan.teacher.cond==1 ~ "Moroccan",
           teacher==2 & moroccan.teacher.cond==2 ~ "Moroccan",
           teacher==3 & moroccan.teacher.cond==3 ~ "Moroccan",
           TRUE ~ "Dutch (other teacher Moroccan)"),
         teacher.fired = case_when(
           teacher==1 & decision==1 ~ 1,
           teacher==2 & decision==2 ~ 1,
           teacher==3 & decision==3 ~ 1,
           TRUE ~ 0)) %>% 
  mutate(teacher = factor(teacher),
         moroccan.lab = moroccan %>% Recode("0='Dutch';
                                            1='Moroccan'")) %>% 
  select(pid,
         teacher,
         ILE,
         teacher.fired,
         moroccan,
         moroccan.lab,
         moroccan.teacher.cond.1,
         advice,
         female,
         age,
         income,
         man.check.all,
         mobile,
         ethnicity.dutch,
         reaction.time) %>% 
  mutate(teacher.lab = teacher %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"),
         advice.algorithm = advice %>% Recode("'algorithm'=1;'human'=0"))




## All: long

school.all.long <- school.all %>%
  select(pid,score.teacher.1:score.teacher.3,
         decision,
         advice,
         mobile,
         female,
         age,
         income,
         study,
         man.check.all,
         reaction.time) %>% 
  gather(key="key",value = "ILE",-pid,-decision,-advice,-man.check.all,-mobile,-female,-age,-income,-study,-reaction.time) %>%
  arrange(study,pid) %>% 
  mutate(ILE = factor(ILE),
         teacher = key %>% Recode("'score.teacher.1'=1;
                                  'score.teacher.2'=2;
                                  'score.teacher.3'=3"),
         teacher.fired = case_when(
           teacher==1 & decision==1 ~ 1,
           teacher==2 & decision==2 ~ 1,
           teacher==3 & decision==3 ~ 1,
           TRUE ~ 0)) %>% 
  mutate(teacher = factor(teacher)) %>% 
  select(pid,
         teacher,
         ILE,
         teacher.fired,
         advice,
         female,
         age,
         income,
         man.check.all,
         mobile,
         study,
         reaction.time) %>% 
  mutate(teacher.lab = teacher %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"),
         advice.algorithm = advice %>% Recode("'algorithm'=1;'human'=0"))




