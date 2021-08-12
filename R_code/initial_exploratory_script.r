library(tidyverse)
library(haven)
library(cesdata)# Be sure to install the most recent version of my cesdata
#remotes::install_github('sjkiss/cesdata')
data("ces19_kiss")
#Read in the data file 

str(ces19_kiss)
nrow(ces19_kiss)
#Just rename to cps to make it simpler

cps<-ces19_kiss

#### Variable Labels#### 
#search for our variables
library(labelled)
look_for(cps, "income") #income volatility is kiss_q1
look_for(cps, "job")
look_for(cps, "business")


##looks like kiss_q2_* are the probability of loss and kiss_q3_* are the consequences. 

#Print all the variable labels
cps %>% 
  select(starts_with('kiss')) %>% 
  map(., var_label)

#check that the missing values do not overlap

table(cps$kiss_q2_business, cps$kiss_q3_job)
##There is no overlap. 
#Check value labels 

cps %>% 
  select(starts_with('kiss')) %>% 
  val_labels()
## Note, low numbers are currently high risk, high numbers are low risk; we should reverse these and set 6 to missing 

#hgis code takes 1q, sets 5 to be NA and reverses the values. 

cps %>% 
  mutate(kiss_q1_out=case_when(
    kiss_q1 == 1 ~ 4,
    kiss_q1== 2 ~ 3,
    kiss_q1== 3~ 2 ,
    kiss_q1 == 4 ~ 1,
    kiss_q1==5 ~ NA_real_)
    ) -> cps
#This code takes the q2 and q3 questions, sets 6 to be NA and reverses the values
cps %>% 
  mutate_at(vars(starts_with("kiss_q2")|starts_with("kiss_q3")), 
    list(out=
      ~ case_when(
      . == 5 ~ 1,
      . == 4 ~ 2,
      . ==3 ~ 3,
      . ==2 ~ 4, 
      . == 1 ~ 5,
        . == 6 ~ NA_real_
    ))) -> cps


#let's rename the variables right away

cps %>% 
  rename(., "volatility"=kiss_q1_out, "job_prob"=kiss_q2_job_out, "job_cons"=kiss_q3_job_out, "bus_prob"=kiss_q2_business_out, "bus_cons"=kiss_q3_business_out)-> cps
names(cps)



#Analysis
#Is perception of likelihood of job loss correlated with the consequences

cps %>% 
  select(contains("job_")) %>% 
  cor(., use="complete.obs")

##Answer: No. Likelihood of job loss is *not* correlated with the consequences of job loss. 

## Is consequences of job loss correlated with income. 

look_for(cps, "income")

cps$cps19_income_number %>% 
 summary()
length(cps$cps19_income_number)

#Answer, the greater the income , the lower the consequences of job loss. 
cor(cps$cps19_income_number, cps$job_cons, use="complete.obs")

## Is there a curvilinear relationship. 

cps %>% 
  ggplot(aes(x=cps19_income_number, cps$job_cons))+geom_point()+geom_smooth(method="loess")+geom_jitter()

#fascinating. There is a curvilinear relationship although there are super few cases at the high end. 

# We can re-test teh correlation above, splitting incomes at about $250000

cps %>% 
  mutate(income2=case_when(
    cps19_income_number < 250001 ~ "regular",
    cps19_income_number > 250000 ~ "super-rich"
  ))-> cps


cps %>% 
  group_by(income2) %>% 
  summarize(avg=mean(cps19_income_number))
cps %>% 
  group_by(income2) %>% 
  names()
 summarise(out=cor(cps19_income_number, job_cons, use="everything"))
cps %>% 
  select(cps19_income_number, income2, job_cons) %>% 
group_by(income2) %>% 
  summarize(out=cor(cps19_income_number, job_cons, use="pairwise.complete.obs"))

#Yes! Consequences of job loss are curvilinear. They decline with income up to a certain point and then increase with income after that point. So, up to a certain point, increasing income provides a cushion against job loss; after that point, you have too much skin in the game and a job loss would be crushing. 
### Is there a formal way to identify that point? Presumably testing some kind of model fit with dummy variables set at certain ticks. 

#Try visualizing at different points. 

cps %>% 
  mutate(income3=case_when(
    cps19_income_number < 200001 ~ "regular",
    cps19_income_number > 20000 ~ "super-rich"
  ))-> cps

cps %>% 
  mutate(income4=case_when(
    cps19_income_number < 150001 ~ "regular",
    cps19_income_number > 150000 ~ "super-rich"
  ))-> cps

cps %>% 
  mutate(income5=case_when(
    cps19_income_number < 100001 ~ "regular",
    cps19_income_number > 100000 ~ "super-rich"
  ))-> cps
#
#
cps %>% 
  filter(is.na(income2)==F) %>% 
  ggplot(., aes(x=cps19_income_number, y=job_cons))+geom_point()+geom_smooth(method="lm")+facet_wrap(~income2)+labs(title="breakpoint set at 250")

##
cps %>% 
  filter(is.na(income3)==F) %>% 
  ggplot(., aes(x=cps19_income_number, y=job_cons))+geom_point()+geom_smooth(method="lm")+facet_wrap(~income3)+labs(title="breakpoint set at 200")
#
cps %>% 
  filter(is.na(income4)==F) %>% 
  ggplot(., aes(x=cps19_income_number, y=job_cons))+geom_point()+geom_smooth(method="lm")+facet_wrap(~income4)+labs(title="breakpoint set at 150")

#
cps %>% 
  filter(is.na(income5)==F) %>% 
  ggplot(., aes(x=cps19_income_number, y=job_cons))+geom_point()+geom_smooth(method="lm")+facet_wrap(~income5)+labs(title="breakpoint set at 100")

## Descriptives
table(as_factor(cps$cps19_employment))
look_for(cps, "employ")
?poly
poly(cps$cps19_income_number, 2)
str(cps)
