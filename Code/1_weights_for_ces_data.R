#### This code was used to create the weights necessary for the CES ####

library(cansim)
library(tidyverse)
library(car)
library(here)
#comment out if you are not simon iss
#options(cansim.cache_path="~/skiss/cansim")
#We use Cansim Table 14-10-0020-01 for three of the four variables
get_cansim_table_overview('14-10-0020-01') 
#Get Table
education_employment<-get_cansim('14-10-0020-01') 
#Glimpse
glimpse(education_employment)

#Create a master data framne with some basic variable renames for ease and filter out all other than employment rate and 2019
education_employment %>% 
  rename(., "Year"="REF_DATE", "Degree"="Educational attainment",  "LFS"="Labour force characteristics", "Age"="Age group") %>% 
  filter(.,Year==2019, LFS=="Employment")-> education_employment
education_employment %>% 
  View()
table(education_employment$LFS)
glimpse(education_employment)
#Find employment rate for level of degree attainment. 
education_employment %>% 
filter(Degree!="Total, all education levels", Degree!="Bachelor's degree", Degree!="Above bachelor\'s degree", LFS=="Employment", Sex=="Both sexes", GEO=='Canada', Age=="15 years and over") %>% 
  select(Degree, LFS,VALUE) %>% 
    mutate(pct=(VALUE/sum(VALUE))*100) %>% 
  write.csv(file=here("Precarity", "Data", "education_employment.csv"))

#EMPLOYMENT BY PROVINCE
education_employment %>% 
  filter(GEO!="Canada", Sex=="Both sexes", Age=="15 years and over", Degree=="Total, all education levels") %>% 
  select(GEO, Sex, Age, Degree, VALUE, Year, LFS) %>% 
  mutate(pct=(VALUE/sum(VALUE))*100) %>% 
write.csv(file=here("Precarity", "Data", "province_employment.csv"))

#employment by sex
education_employment %>% 
  filter(GEO=="Canada", Sex!="Both sexes", Age=="15 years and over", Degree=="Total, all education levels") %>% 
  select(GEO, Sex, Age, Degree, VALUE, Year, LFS) %>% 
  mutate(pct=(VALUE/sum(VALUE))*100) %>% 
  write.csv(file=here("Precarity", "Data", "sex_employment.csv"))

#EMPLOYMENT BY AGE
#Get new cansim table Table 14-10-0327-01 for five year age groups
library(cansim)
library(tidyverse)
age_employment<-get_cansim('14-10-0327-01')
age_employment %>% 
  rename(., "Year"="REF_DATE",  "LFS"="Labour force characteristics", "Age"="Age group") %>% 
  filter(.,Year==2019, LFS=="Employment") %>% 
  filter(GEO=="Canada", Sex=="Both sexes") %>% 
  filter(Age=="15 to 19 years" | 
           Age=="20 to 24 years"| 
           Age=="25 to 29 years"|
           Age=="30 to 34 years"| 
           Age=="35 to 39 years"|
           Age=="40 to 44 years"|
           Age=='45 to 49 years'|
           Age=="50 to 54 years"|
         Age=="55 to 59 years"|
           Age=="60 to 64 years"|
           Age=="65 to 69 years"|
           Age=="70 years and over") %>% 
  mutate(pct=(VALUE/sum(VALUE))*100) %>% 
select(GEO, Sex, Age, LFS,VALUE, pct) %>% 
  write.csv(file=here("Precarity", "Data", "age_employment.csv"))

