library(tidyverse)
library(car)
library(lubridate)


#######Wave 1######

school.s2w1_00 <- read.csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.2/study_2.csv") 
school.s2w1_00_lab <- read.csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.2/study_2.lab.csv") 



school.s2w1_00[is.na(school.s2w1_00)] <- 0

school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(advice = case_when(
    Q3.1==1 | Q14.1==1 ~ "human",
    Q4.1==1 | Q15.1==1 ~ "algorithm"
  ),
  congruence = case_when(
    Q5.2==1 | Q6.2==1 | Q7.2==1 | Q8.2==1 | Q9.2==1 | Q10.2==1 | Q11.2==1 | Q12.2==1 |
    Q16.2==1 | Q17.2==1 | Q18.2==1 | Q19.2==1 | Q20.2==1 | Q21.2==1 | Q22.2==1 | Q23.2==1 
    ~ "high",
    Q5.3==1 | Q6.3==1 | Q7.3==1 | Q8.3==1 | Q9.3==1 | Q10.3==1 | Q11.3==1 | Q12.3==1 |
      Q16.3==1 | Q17.3==1 | Q18.3==1 | Q19.3==1 | Q20.3==1 | Q21.3==1 | Q22.3==1 | Q23.3==1 
    ~ "low",
  ),
  moroccan.teacher.cond = case_when(
    Q5.1==1 | Q9.1==1 | Q16.1==1 | Q20.1==1 ~ 0,
    Q6.1==1 | Q10.1==1 | Q17.1==1 | Q21.1==1 ~ 1,
    Q7.1==1 | Q11.1==1 | Q18.1==1 | Q22.1==1 ~ 2,
    Q8.1==1 | Q12.1==1 | Q19.1==1 | Q23.1==1 ~ 3
  )) %>% 
  mutate(moroccan.teacher.cond.lab = moroccan.teacher.cond %>% Recode("0='0 none';
                                                               1='1 teacher 1_moroccan';
                                                               2='2 teacher 2_moroccan';
                                                               3='3 teacher 3_moroccan'")) %>% 
  mutate(congruence.lab = Recode(congruence,"'high'='1.high';'low'='2.low'")) %>% 
  mutate(mobile = case_when(
    Q2.1==1 ~ 1,
    Q13.1==1 ~ 0
  ))%>% 
  mutate(advice.algorithm = advice %>% 
           Recode("'algorithm'=1;
                  'human'=0"))



#Timer
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(timer.minutes = school.s2w1_00_lab$Duration..in.seconds./60 %>% round(0))
#Timer



# Manipulation checks

## Score
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(decision = Q5.5 + Q6.5 + Q7.5 + Q8.5 + Q9.5 + Q10.5 + Q11.5 + Q12.5 +
           Q16.5 + Q17.5 + Q18.5 + Q19.5 + Q20.5 + Q21.5 + Q22.5 + Q23.5,
         score.teacher.1 = case_when(
           congruence=="high"  ~ 8,
           congruence=="low" ~ 4,
         ),
         score.teacher.2 = case_when(
           congruence=="high"  ~ 4,
           congruence=="low" ~ 6,
         ),
         score.teacher.3 = case_when(
           congruence=="high"  ~ 6,
           congruence=="low" ~ 8,
         )) %>% 
  mutate(teacher.score.min = case_when(
    congruence=="high"  ~ 2,
    congruence=="low" ~ 1,
  )) %>% 
  mutate(score.teacher.selected = case_when(
    decision==1  ~ score.teacher.1,
    decision==2  ~ score.teacher.2,
    decision==3  ~ score.teacher.3
  )) %>%
  mutate(score.teacher.selected.guess = Q26.4) 

t1 <- school.s2w1_00_lab %>% 
  mutate(decision.lab =  paste0(Q5.5, Q6.5, Q7.5, 
                                 Q8.5, Q9.5, Q10.5,
                                 Q11.5, Q12.5,
                                 Q16.5, Q17.5, Q18.5, 
                                 Q19.5, Q20.5, Q21.5, 
                                 Q22.5, Q23.5) %>% str_replace_all("NA",""))

school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(decision.lab = t1$decision.lab,
         decision.lab.number = decision %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"))



## Reaction time
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(reaction.time = Q5.6_3 + Q6.6_3 + Q7.6_3 + Q8.6_3 + Q9.6_3 + Q10.6_3 + Q11.6_3 + Q12.6_3 +
           Q16.6_3 + Q17.6_3 + Q18.6_3 + Q19.6_3 + Q20.6_3 + Q21.6_3 + Q22.6_3 + Q23.6_3) %>% 
  mutate(reaction.time_log = log(reaction.time+1))


## Advice type
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(man.check.advice = case_when(
    advice=="human" & Q26.2==2  ~ 1,
    advice=="algorithm" & Q26.2==1  ~ 1,
    TRUE ~ 0,
  ))

## HR person's evaluation
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(man.check.evaluation = case_when(
    decision==1 & Q26.5==1  ~ 1,
    decision==2 & Q26.5==3  ~ 1,
    decision==3 & Q26.5==2  ~ 1,
    TRUE ~ 0
  ))

## ILE score
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(man.check.score.exact = ifelse(score.teacher.selected.guess==score.teacher.selected,1,0),
         man.check.score = case_when(
    score.teacher.selected==8 & Q26.3==11  ~ 1,
    score.teacher.selected==4 & Q26.3==12  ~ 1,
    score.teacher.selected==6 & Q26.3==13  ~ 1,
    TRUE ~ 0
  ))

## IMC
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(IMC =ifelse(str_detect(Q28.7_TEXT,c("9|Negen|negen"))==T,1,0))

# Demographics
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(female = Q1.3 %>% Recode("1=0;2=1;3=NA;4=NA"),
         age = Q30.2_1 %>% na_if(0),
         school.children = ifelse(Q30.3_2==1|Q30.3_3==1,1,0),
         highschool.children = Q30.3_3,
         education = school.s2w1_00_lab$Q30.4,
         income = Q30.5,
         province = school.s2w1_00_lab$Q30.6,
         ethnicity.dutch = Q30.7,
         ethnicity.dutch.lab = school.s2w1_00_lab$Q30.7,
         work.education = Q30.8 %>% Recode("2=1;1=0;0=NA"),
         work.education.text = Q30.9 %>% Recode("0=NA"),
         comments.general = Q30.10 %>% Recode("0=NA"))



# Outcome variables
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(follow.advice = ifelse(decision==teacher.score.min,1,0),
         follow.advice.1 = ifelse(score.teacher.selected==4,1,0)) %>% 
  mutate(fire.moroccan =ifelse(decision.lab=="F. El Amrani", 1,0)) %>%
  mutate(follow.advice.moroccan = ifelse(fire.moroccan==1 & follow.advice==1,1,0))
  



# Reflection about decision
school.s2w1_00 <- school.s2w1_00 %>% 
  mutate(weight.evaluation = Q28.4,
         weight.advice = Q28.2+Q28.3,
         input.order = Q28.6 %>% Recode("1='ILE score first';
                                        2='HR evaluation first';
                                        3='Other'"))




school.s2w1_00 <- school.s2w1_00 %>% 
  filter(decision>0)




#######Wave 2######


school.s2w2_00 <- read.csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.2/study_2_w2.csv") 
school.s2w2_00_lab <- read.csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.2/study_2_w2.lab.csv") 


school.s2w2_00[is.na(school.s2w2_00)] <- 0

school.s2w2_00 <- school.s2w2_00 %>%
  mutate(
    start.date = dmy_hm(V8),
    end.date = dmy_hm(V9)
  ) %>% 
  filter(as_date(start.date) >= as.Date("2020-04-01"))

school.s2w2_00_lab <- school.s2w2_00_lab %>% 
  mutate(
    start.date = dmy_hm(StartDate),
    end.date = dmy_hm(EndDate)
  ) %>% 
  filter(as_date(start.date) >= as.Date("2020-04-01"))




school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(advice = case_when(
    Q3.1==1 | Q14.1==1 ~ "human",
    Q4.1==1 | Q15.1==1 ~ "algorithm"
  ),
  congruence = case_when(
    Q5.2==1 | Q7.2==1 | Q9.2==1 | Q11.2==1 | 
      Q16.2==1 | Q18.2==1 | Q20.2==1 | Q22.2==1  
    ~ "high",
    Q386==1 | Q6.3==1 | Q392==1 | Q10.3==1 | 
      Q374==1 | Q17.3==1 | Q380==1 | Q21.3==1  
    ~ "low",
  ),
  moroccan.teacher.cond = case_when(
    Q5.1==1 | Q384==1 | Q9.1==1 | Q390==1 | 
      Q16.1==1 | Q372==1 | Q20.1==1 | Q378==1 ~ 0,
    Q6.1==1 | Q10.1==1 | Q17.1==1 | Q21.1==1 ~ 1,
    Q7.1==1 | Q11.1==1 | Q18.1==1 | Q22.1==1 ~ 2
  )) %>% 
  mutate(moroccan.teacher.cond.lab = moroccan.teacher.cond %>% Recode("0='0 none';
                                                               1='1 teacher 1_moroccan';
                                                               2='2 teacher 2_moroccan';
                                                               3='3 teacher 3_moroccan'")) %>% 
  mutate(congruence.lab = Recode(congruence,"'high'='1.high';'low'='2.low'")) %>% 
  mutate(mobile = case_when(
    Q2.1==1 ~ 1,
    Q13.1==1 ~ 0
  ))%>% 
  mutate(advice.algorithm = advice %>% 
           Recode("'algorithm'=1;
                  'human'=0"))



#Timer
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(timer.minutes = school.s2w2_00_lab$Duration..in.seconds./60 %>% round(0),
         finished = V10)
#Timer



# Manipulation checks

## Score
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(decision = Q5.5 + Q388 + Q6.5+ Q7.5+ Q9.5+ Q394+ Q10.5+ Q11.5+
                    Q16.5+ Q376+ Q17.5+ Q18.5+ Q20.5+ Q382+ Q21.5+ Q22.5,
         score.teacher.1 = case_when(
           congruence=="high"  ~ 8,
           congruence=="low" ~ 4,
         ),
         score.teacher.2 = case_when(
           congruence=="high"  ~ 4,
           congruence=="low" ~ 6,
         ),
         score.teacher.3 = case_when(
           congruence=="high"  ~ 6,
           congruence=="low" ~ 8,
         )) %>% 
  mutate(teacher.score.min = case_when(
    congruence=="high"  ~ 2,
    congruence=="low" ~ 1,
  )) %>% 
  mutate(score.teacher.selected = case_when(
    decision==1  ~ score.teacher.1,
    decision==2  ~ score.teacher.2,
    decision==3  ~ score.teacher.3
  )) %>%
  mutate(score.teacher.selected.guess = Q26.4) 

t1 <- school.s2w2_00_lab %>% 
  mutate(decision.lab =  paste0(Q5.5, Q388, Q6.5, Q7.5, Q9.5, Q394, Q10.5, Q11.5,
                                Q16.5, Q376, Q17.5, Q18.5, Q20.5, Q382, Q21.5, Q22.5) %>% str_replace_all("NA",""))

school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(decision.lab = t1$decision.lab,
         decision.lab.number = decision %>% Recode("1='Teacher 1';
                                                   2='Teacher 2';
                                                   3='Teacher 3'"))


## Reaction time
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(reaction.time = Q5.6_3 + Q389_3 + Q6.6_3 + Q7.6_3 + Q9.6_3 + Q395_3 + Q10.6_3 + Q11.6_3 +
           Q16.6_3 + Q377_3 + Q17.6_3 + Q18.6_3 + Q20.6_3 + Q383_3 + Q21.6_3 + Q22.6_3) %>% 
  mutate(reaction.time_log = log(reaction.time+1))




## Advice type
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(man.check.advice = case_when(
    advice=="human" & Q26.2==2  ~ 1,
    advice=="algorithm" & Q26.2==1  ~ 1,
    TRUE ~ 0,
  ))

## HR person's evaluation
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(man.check.evaluation = case_when(
    decision==1 & Q26.5==1  ~ 1,
    decision==2 & Q26.5==3  ~ 1,
    decision==3 & Q26.5==2  ~ 1,
    TRUE ~ 0
  ))

## ILE score
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(man.check.score.exact = ifelse(score.teacher.selected.guess==score.teacher.selected,1,0),
         man.check.score = case_when(
           score.teacher.selected==8 & Q26.3==11  ~ 1,
           score.teacher.selected==4 & Q26.3==12  ~ 1,
           score.teacher.selected==6 & Q26.3==13  ~ 1,
           TRUE ~ 0
         ))

## IMC
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(IMC =ifelse(str_detect(Q28.7_TEXT,c("9|Neg|neg"))==T,1,0))

# Demographics
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(female = Q1.3 %>% Recode("1=0;2=1;3=NA;4=NA"),
         age = Q30.2_1,
         school.children = ifelse(Q30.3_2==1|Q30.3_3==1,1,0),
         highschool.children = Q30.3_3,
         education = school.s2w2_00_lab$Q30.4,
         income = Q30.5,
         province = school.s2w2_00_lab$Q30.6,
         ethnicity.dutch = Q30.7,
         ethnicity.dutch.lab = school.s2w2_00_lab$Q30.7,
         work.education = Q30.8 %>% Recode("2=1;1=0;0=NA"),
         work.education.text = Q30.9 %>% Recode("0=NA"),
         comments.general = Q30.10 %>% Recode("0=NA"))



# Outcome variables
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(follow.advice = ifelse(decision==teacher.score.min,1,0),
         follow.advice.1 = ifelse(score.teacher.selected==4,1,0)) %>% 
  mutate(fire.moroccan =ifelse(decision.lab=="F. El Amrani", 1,0)) %>%
  mutate(follow.advice.moroccan = ifelse(fire.moroccan==1 & follow.advice==1,1,0))




# Reflection about decision
school.s2w2_00 <- school.s2w2_00 %>% 
  mutate(weight.evaluation = Q28.4,
         weight.advice = Q28.2+Q28.3,
         input.order = Q28.6 %>% Recode("1='ILE score first';
                                        2='HR evaluation first';
                                        3='Other'"))




school.s2w2_00 <- school.s2w2_00 %>% 
  filter(finished>0)



school.s2_00.full <- school.s2w1_00 %>% 
  full_join(school.s2w2_00) 

  


##
school.s2_00.full <- school.s2_00.full %>% 
  filter(moroccan.teacher.cond==0|
           moroccan.teacher.cond==1 & congruence == "low" | 
           moroccan.teacher.cond==2 & congruence == "high")
##



#Perceived performance

school.s2_00.full <- school.s2_00.full %>%
  mutate(perceived.performance.item1 = Q29.2 %>% na_if(0),
         perceived.performance.item2 = Q29.3 %>% na_if(0),
         perceived.performance.item3 = Q29.4 %>% na_if(0)) 

school.s2_01.full <- school.s2_00.full %>% 
  filter(IMC==1,timer.minutes>=3) %>% 
  mutate(man.check.all = ifelse(man.check.advice == 1 &
                                  (man.check.score.exact == 1),
                                1,0)) 


school.s2_00 <- school.s2_00.full %>% 
  filter(moroccan.teacher.cond==0|
           moroccan.teacher.cond==1 & congruence == "low" | 
           moroccan.teacher.cond==2 & congruence == "high") %>% 
  mutate(moroccan.teacher.cond.bi = ifelse(moroccan.teacher.cond==0,0,1))
school.s2_01 <- school.s2_00 %>% 
  filter(IMC==1,timer.minutes>=3,ethnicity.dutch==1) %>% 
  mutate(man.check.all = ifelse(man.check.advice == 1 &
                                  (man.check.score.exact == 1),
                                1,0)) 

school.s2_02 <- school.s2_01 %>% 
  filter(man.check.all==1) 


save.image()

