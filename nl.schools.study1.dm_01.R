library(tidyverse)
library(car)


school.s1_00 <- read_csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.1/study_1.csv") 
school.s1_00_lab <- read_csv("C:/SAAR/UNIVERSITY/R/NL.SCHOOLS/data/study.1/study_1.lab.csv") 



school.s1_00[is.na(school.s1_00)] <- 0

school.s1_00 <- school.s1_00 %>% 
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
    ~ "medium",
    Q5.4==1 | Q6.4==1 | Q7.4==1 | Q8.4==1 | Q9.4==1 | Q10.4==1 | Q11.4==1 | Q12.4==1 |
      Q16.4==1 | Q17.4==1 | Q18.4==1 | Q19.4==1 | Q20.4==1 | Q21.4==1 | Q22.4==1 | Q23.4==1 
    ~ "low",
  ),
  teacher.order = case_when(
    Q5.1==1 | Q9.1==1 | Q16.1==1 | Q20.1==1 ~ "Verhagen-Jansen-den Heijer",
    Q6.1==1 | Q10.1==1 | Q17.1==1 | Q21.1==1 ~ "den Heijer-Jansen-Verhagen",
    Q7.1==1 | Q11.1==1 | Q18.1==1 | Q22.1==1 ~ "Jansen-Verhagen-den Heijer",
    Q8.1==1 | Q12.1==1 | Q19.1==1 | Q23.1==1 ~ "Jansen-den Heijer-Verhagen"
  )) %>% 
  mutate(congruence.lab = Recode(congruence,"'high'='1.high';'medium'='2.meduim';'low'='3.low'")) %>% 
  mutate(mobile = case_when(
    Q2.1==1 ~ 1,
    Q13.1==1 ~ 0
  ))%>% 
  mutate(advice.algorithm = advice %>% 
           Recode("'algorithm'=1;
                  'human'=0"))



#Timer
school.s1_00 <- school.s1_00 %>% 
  mutate(timer.minutes = school.s1_00_lab$"Duration (in seconds)"/60 %>% round(0))
#Timer



# Manipulation checks

## Score
school.s1_00 <- school.s1_00 %>% 
  mutate(decision = Q5.6 + Q6.6 + Q7.6 + Q8.6 + Q9.6 + Q10.6 + Q11.6 + Q12.6 +
           Q16.6 + Q17.6 + Q18.6 + Q19.6 + Q20.6 + Q21.6 + Q22.6 + Q23.6,
         score.teacher.1 = case_when(
           congruence=="high"  ~ 8,
           congruence=="medium" ~ 4,
           congruence=="low" ~ 4,
         ),
         score.teacher.2 = case_when(
           congruence=="high"  ~ 4,
           congruence=="medium" ~ 8,
           congruence=="low" ~ 6,
         ),
         score.teacher.3 = case_when(
           congruence=="high"  ~ 6,
           congruence=="medium" ~ 6,
           congruence=="low" ~ 8,
         )) %>% 
  mutate(teacher.score.min = case_when(
    congruence=="high"  ~ 2,
    congruence=="medium" ~ 1,
    congruence=="low" ~ 1,
  )) %>% 
  mutate(score.teacher.selected = case_when(
    decision==1  ~ score.teacher.1,
    decision==2  ~ score.teacher.2,
    decision==3  ~ score.teacher.3
  )) %>%
  mutate(score.teacher.selected.guess = Q26.4) %>% 
mutate(decision.lab = decision %>% Recode("1='1.Verhagen';
                                          2='2.Jansen';
                                          3='3.den Heijer'"))

## Reaction time
school.s1_00 <- school.s1_00 %>% 
  mutate(reaction.time = Q5.7_3 + Q6.7_3 + Q7.7_3 + Q8.7_3 + Q9.7_3 + Q10.7_3 + Q11.7_3 + Q12.7_3 +
           Q16.7_3 + Q17.7_3 + Q18.7_3 + Q19.7_3 + Q20.7_3 + Q21.7_3 + Q22.7_3 + Q23.7_3) %>% 
  mutate(reaction.time_log = log(reaction.time+1))
         


## Advice type
school.s1_00 <- school.s1_00 %>% 
  mutate(man.check.advice = case_when(
    advice=="human" & Q26.2==2  ~ 1,
    advice=="algorithm" & Q26.2==1  ~ 1,
    TRUE ~ 0,
  ))

## HR person's evaluation
school.s1_00 <- school.s1_00 %>% 
  mutate(man.check.evaluation = case_when(
    decision==1 & Q26.5==1  ~ 1,
    decision==2 & Q26.5==3  ~ 1,
    decision==3 & Q26.5==2  ~ 1,
    TRUE ~ 0
  ))

## ILE score
school.s1_00 <- school.s1_00 %>% 
  mutate(man.check.score.exact = ifelse(score.teacher.selected.guess==score.teacher.selected,1,0),
         man.check.score = case_when(
    score.teacher.selected==8 & Q26.3==11  ~ 1,
    score.teacher.selected==4 & Q26.3==12  ~ 1,
    score.teacher.selected==6 & Q26.3==13  ~ 1,
    TRUE ~ 0
  ))

## IMC
school.s1_00 <- school.s1_00 %>% 
  mutate(IMC =ifelse(str_detect(Q28.7_TEXT,c("9|Negen|negen"))==T,1,0))

# Demographics
school.s1_00 <- school.s1_00 %>% 
  mutate(female = Q1.3 %>% Recode("1=0;2=1;3=NA;4=NA"),
         age = Q30.2_1 %>% na_if(0),
         school.children = ifelse(Q30.3_2==1|Q30.3_3==1,1,0),
         highschool.children = Q30.3_3,
         education = Q30.4 %>% Recode("1='1. VMBO/Mavo';
                                      2='2. Havo';
                                      3='3. Vwo';
                                      4='4. MBO';
                                      5='5. HBO Bachelor';
                                      6='6. WO Bachelor';
                                      7='7. HBO Master';
                                      8='8. WO Master of hoger';
                                      9='9. Anders'"),
         income = Q30.5,
         province = school.s1_00_lab$Q30.6,
         work.education = Q30.7 %>% Recode("2=1;1=0;0=NA"),
         work.education.text = Q30.8 %>% Recode("0=NA"),
         comments.general = Q30.9 %>% Recode("0=NA"))



# Outcome variables
school.s1_00 <- school.s1_00 %>% 
  mutate(follow.advice = ifelse(decision==teacher.score.min,1,0),
         follow.advice.1 = ifelse(score.teacher.selected==4,1,0),
         follow.advice.2 = ifelse(score.teacher.selected==6,1,0))



# Reflection about decision
school.s1_00 <- school.s1_00 %>% 
  mutate(weight.evaluation = Q28.4,
         weight.advice = Q28.2+Q28.3,
         input.order = Q28.6 %>% Recode("1='ILE score first';
                                        2='HR evaluation first';
                                        3='Other'"))

#Perceived performance



school.s1_00 <- school.s1_00 %>%
  mutate(perceived.performance.item1 = Q29.2 %>% na_if(0),
         perceived.performance.item2 = Q29.3 %>% na_if(0),
         perceived.performance.item3 = Q29.4 %>% na_if(0)) 

school.s1_00 <- school.s1_00 %>% 
  filter(decision>0)

school.s1_01 <- school.s1_00 %>% 
  filter(IMC==1,timer.minutes>=3)


school.s1_01 <- school.s1_01 %>% 
  mutate(man.check.all = ifelse(man.check.advice == 1 &
                                  (man.check.score.exact == 1),
                                1,0)) 

school.s1_02 <- school.s1_01 %>% 
  filter(man.check.all==1) 

save.image()
