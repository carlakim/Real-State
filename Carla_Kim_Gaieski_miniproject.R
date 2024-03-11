library(tidyverse)

#Part I:HMDA Data

hmda <- read_csv("data/hmda_2014_ca_all-records_codes.csv")

#Q1: Load the data to R and clean the data set as in Task of Dplyr lecture
#Assign the dataset to an object named as “sample”
sample <- hmda %>% filter(loan_purpose==1,property_type==1,action_taken==1|action_taken==3, loan_amount_000s<1000)%>% 
  arrange(desc(loan_amount_000s))%>% 
  mutate(ap = if_else(action_taken==1,1,0))%>% 
  select(applicant_race_1, ap, loan_amount_000s, denial_reason_1, applicant_income_000s)

view(sample)

#Q2. Calculate loan approval probability for each race as in the class task.
sample %>% group_by(applicant_race_1)%>%
  summarise(ap_rate=mean(ap))

#Q3. Visualize your outcome in Q2 as in the class task (Hint: geom_col())
task1 <- hmda %>% filter(loan_purpose==1,property_type==1,action_taken==1|action_taken==3, loan_amount_000s<1000)%>% 
  arrange(desc(loan_amount_000s))%>% 
  mutate(ap = if_else(action_taken==1,1,0))%>% 
  select(applicant_race_1, ap, loan_amount_000s, denial_reason_1, applicant_income_000s) %>% 
  group_by(applicant_race_1)%>%
  summarise(ap_rate=mean(ap))

ggplot(task1,aes(applicant_race_1,ap_rate)) + geom_col()

#Q4. Run the following codes & explain
sample %>% filter(!is.na(denial_reason_1)) %>%
  count(denial_reason_1)%>%
  mutate(share=n/sum(n))%>% ggplot(aes(denial_reason_1, share)) +
  geom_bar(stat="identity") +
  scale_x_discrete(limits=c(1:9))

#Q5. Run Q4 for “Black or African American”. Repeat for “White”. What’s the main difference between the two groups?
#black or african american
sample %>% filter(applicant_race_1==3) %>% 
  filter(!is.na(denial_reason_1)) %>%
  count(denial_reason_1) %>% mutate(share=n/sum(n))%>%
  ggplot(aes(denial_reason_1, share)) + geom_bar(stat="identity") +
  scale_x_discrete(limits=c(1:9))

#white
sample %>% filter(applicant_race_1==5) %>% 
  filter(!is.na(denial_reason_1)) %>%
  count(denial_reason_1) %>% mutate(share=n/sum(n))%>%
  ggplot(aes(denial_reason_1, share)) + geom_bar(stat="identity") +
  scale_x_discrete(limits=c(1:9))

#Part II: Census Data

library(ipumsr)

ddi <- read_ipums_ddi("data/usa_00002.xml")
data <- read_ipums_micro(ddi)
write_rds(data, "data/census_age_2019.rds")
data %>% view()

#Q1 remove observations in “Not in identifiable area”. Then, calculate average ages and populations for MSAs.
data_msa <- data %>% filter(MET2013 != 00000) 
data_msa %>% 
  group_by(MET2013) %>% 
  summarize(avg_age=weighted.mean(AGE, wt=PERWT))

data_msa %>% 
  group_by(MET2013) %>% 
  summarise(pop=sum(PERWT))

#Q2 Choose cities with more than 1 million population. Show the 10 youngest MSAs.
data_msa %>% 
  group_by(MET2013) %>% 
  summarise(pop=sum(PERWT), avg_age=weighted.mean(AGE, wt=PERWT)) %>% 
  filter(pop>1000000) %>% 
  arrange(avg_age) %>% 
  head(10)

#Q3 Calculate population by AGE. Then, draw a diagram with x-axis = AGE and y-axis=population by AGE
dataage <- data_msa %>% 
  group_by(AGE) %>% 
  summarise(pop_age=sum(PERWT))

ggplot(dataage, aes(x = AGE, y=pop_age)) + geom_point() + geom_line()


#Q4  Isolate your sample with individuals whose AGE is between 31 and 55 and OCC is not N/A (“OCC !=0”)
#create a new variable showing age groups as “31-35”, “36-40”, “41-45”, “46- 50”, “51-55” using “if_else()” function
#Show the 3 occupations with the most people by the age groups.
data_rag <-data_msa %>% 
  filter(AGE >= 31 & AGE <= 55, OCC !=0) %>% 
  mutate(age_range= if_else(AGE >= 31 & AGE <= 35, "31-35",
                           if_else(AGE >= 36 & AGE <= 40, "36-40",
                                   if_else(AGE >= 41 & AGE <= 45, "41-45",
                                           if_else(AGE >= 46 & AGE <= 50, "46-50",
                                                   if_else(AGE >= 51 & AGE <= 55, "51-55",
                                                           "False"))))))
data_rag %>% group_by(age_range,OCC) %>% 
  summarise(ppl_age = sum(PERWT)) %>% 
  top_n(3,ppl_age) %>% arrange(age_range,ppl_age)

#Q5 For the age group we defined in Q4, show the 3 occupations with the highest incomes.
data_rag %>% group_by(age_range) %>% top_n(3,INCTOT) %>%  arrange(age_range,desc(INCTOT))

#using mean weighted
data_rag %>% group_by(age_range,OCC) %>% 
  summarise(meanw_inc = weighted.mean(INCTOT,w=PERWT))%>% 
  arrange(age_range,desc(meanw_inc)) %>% 
  top_n(3)

#using mean
data_rag %>% group_by(age_range,OCC) %>% 
  summarise(mean_inc = mean(INCTOT)) %>% 
  arrange(age_range,desc(mean_inc)) %>% 
  top_n(3) 

#using median
data_rag %>% group_by(age_range,OCC) %>% 
  summarise(median_inc = median(INCTOT,w=PERWT)) %>% 
  arrange(age_range,desc(median_inc)) %>% 
  top_n(3)

