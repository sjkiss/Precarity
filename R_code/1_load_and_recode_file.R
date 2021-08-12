library(tidyverse)
library(haven)
library(here)
##Load data
cps<-read_dta(here("Data", "cps_data.dta"))
View(cps)

str(cps)
#search for our variables
library(labelled)
look_for(cps, "income") #income volatility is kiss_q1

look_for(cps, "job")
look_for(cps, "business")
##looks like kiss_q2_* are the probability of loss and kiss_q3_* are the consequences. 

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

##Note there are missing values

#### If we want to use weights We need to make a survey design object ####

## Make survey design file
#install.packages("survey")
#install.packages("srvyr")
library(survey)
library(srvyr)

cps %>% 
  filter(is.na(cps19_weight_general_lfs)==F) -> cps

cps<-as_survey_design(.data=cps,weights=cps19_weight_general_lfs)

poly(na.omit(cps$variables$cps19_income_number), -2)
