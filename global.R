#health data cleaning pre shiny app script 
#Laura Weller, December 2019 

#Some libraries
library(tidyverse)
library(readxl)
library(magrittr)
library(readr)
library(dplyr)

#Load Control data 
control_raw <- read.csv("Deaths_mortality_rates_by_age_group_test.csv")

#Change data type for each column 
control_raw$REF_DATE <- as.numeric(control_raw$REF_DATE)
control_raw$Characteristics <- as.character(control_raw$Characteristics)
control_raw$Age.at.time.of.death <- as.character(control_raw$Age.at.time.of.death)
control_raw$Sex <- as.character(control_raw$Sex)
control_raw$GEO <- as.character(control_raw$GEO)

#recode Age 
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, all ages']<-'All'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 20 to 24 years']<-'20 to 24'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 25 to 29 years']<-'25 to 29'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 30 to 34 years']<-'30 to 34'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 35 to 39 years']<-'35 to 39'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 40 to 44 years']<-'40 to 44'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 45 to 49 years']<-'45 to 49'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 50 to 54 years']<-'50 to 54'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 55 to 59 years']<-'55 to 59'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 60 to 64 years']<-'60 to 64'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 65 to 69 years']<-'65 to 69'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 70 to 74 years']<-'70 to 74'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 75 to 79 years']<-'75 to 79'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 80 to 84 years']<-'80 to 84'
control_raw$Age.at.time.of.death[control_raw$Age.at.time.of.death=='Age at time of death, 85 to 89 years']<-'85 to 89'

#recode sex
control_raw$Sex[control_raw$Sex=='Both sexes']<-'B'
control_raw$Sex[control_raw$Sex=='Females']<-'F'
control_raw$Sex[control_raw$Sex=='Males']<-'M'

#recode Region 
control_raw$GEO[control_raw$GEO=='Alberta, place of residence']<-'AB'
control_raw$GEO[control_raw$GEO=='British Columbia, place of residence']<-'BC'
control_raw$GEO[control_raw$GEO=='Canada, place of residence']<-'CANADA'
control_raw$GEO[control_raw$GEO=='Manitoba, place of residence']<-'MB'
control_raw$GEO[control_raw$GEO=='New Brunswick, place of residence']<-'NB'
control_raw$GEO[control_raw$GEO=='Newfoundland and Labrador, place of residence']<-'NL'
control_raw$GEO[control_raw$GEO=='Nova Scotia, place of residence']<-'NS'
control_raw$GEO[control_raw$GEO=='Ontario, place of residence']<-'ON'
control_raw$GEO[control_raw$GEO=='Prince Edward Island, place of residence']<-'PE'
control_raw$GEO[control_raw$GEO=='Quebec, place of residence']<-'QC'
control_raw$GEO[control_raw$GEO=='Saskatchewan, place of residence']<-'SK'

#Filter & clean data from each column, delete unneeded parameters 
control_raw <- control_raw%>%
  select(Year="REF_DATE", 
         Region="GEO",
         Age="Age.at.time.of.death",
         Sex,
         Variable="Characteristics",
         Value="VALUE")%>%
  filter(Year>2000,
         Year<2018,
         Region!='Northwest Territories, place of residence',
         Region!='Northwest Territories including Nunavut, place of residence',
         Region!='Nunavut, place of residence',
         Region!='Yukon, place of residence',
         Region!='Unknown province or territory of residence',
         Age!="Age at time of death, under 1 year",
         Age!="Age at time of death, 1 to 4 years",
         Age!="Age at time of death, 5 to 9 years",
         Age!="Age at time of death, 10 to 14 years",
         Age!="Age at time of death, 15 to 19 years",
         Age!="Age at time of death, not stated",
         Age!="Age at time of death, 90 years and over")

#Separate mortality rate
control_rate <- control_raw%>%
  filter(Variable=='Mortality rate per 1,000 population')

#Recode Rate variables 
colnames(control_rate)[colnames(control_rate)=="Variable"]<-"Rate"
colnames(control_rate)[colnames(control_rate)=="Value"]<-"Control_rate"

#Separate mortality count 
control_count <- control_raw%>%
  filter(Variable=='Number of deaths')

#Recode mortality count variables 
colnames(control_count)[colnames(control_count)=="Variable"]<-"Number"
colnames(control_count)[colnames(control_count)=="Value"]<-"Control_number"

#Merge number and rates into one file 
control_mortality  <- left_join(control_count, control_rate, by=c("Sex","Year","Age","Region"))


#Create denominator for calculating new rate
control_mortality  <- within(control_mortality , Control_denom <- Control_number/(Control_rate/1000))%>%
  select(Year,Sex,Age,Region,Control_number,Control_rate, Control_denom)%>%
  mutate(Control_rate=Control_rate/1000)


#Load data from first model 
model1_raw <- read_excel("Mortality_NPHS(tbl).xlsx",
                                 sheet="tPop_Count")

#Filter and rename column variables 
model1_raw <- model1_raw%>%
  select(Region= "Self-reported Province",
         "Sex",
         "Year",
         Age="split( lifespan_age, AGE_GROUP20_5 )",
         Model1_rate="Death hazard rate",
         Model1_number="Deaths",
         Model1_denom="Person-years")

#Load data from second model 
model2_raw <- read_excel("Mortality_DP(tbl).xlsx",
                                        sheet="tPop_Count")
#Filter and rename column variables 
model2_raw <- model2_raw%>%
  select(Region= "Self-reported Province",
         "Sex",
         "Year",
         Age="split( lifespan_age, AGE_GROUP20_5 )",
         Model2_rate="Death hazard rate",
         Model2_number="Deaths",
         Model2_denom="Person-years")


#Load data from third model 
model3_raw <- read_excel("Mortality_MPoRT_V2(tbl).xlsx",
                           sheet="tPop_Count")

#Filter and rename column variables 
model3_raw <- model3_raw%>%
  select(Region="Self-reported Province",
         "Sex",
         "Year",
         Age="split( lifespan_age, AGE_GROUP20_5 )",
         Model3_rate="Death hazard rate",
         Model3_number="Deaths",
         Model3_denom="Person-years")

#Merge model variables together
model1_model2_merge <- inner_join(model1_raw, model2_raw, by=c("Year","Sex","Region","Age"))
models_merge <- inner_join(model1_model2_merge, model3_raw, by=c("Year","Sex","Region","Age"))

#Filter age and year for merged columns 
models_merged <- models_merge%>%
  filter(Age!="[min,20[",
         Age!="[90,95[",
         Age!="[95,100[",
         Age!="[100,max]",
         Year<2018
  )

#Recode age groups 
models_merged$Age[models_merged$Age=='All']<-'All'
models_merged$Age[models_merged$Age=='[20,25[']<-'20 to 24'
models_merged$Age[models_merged$Age=='[25,30[']<-'25 to 29'
models_merged$Age[models_merged$Age=='[30,35[']<-'30 to 34'
models_merged$Age[models_merged$Age=='[35,40[']<-'35 to 39'
models_merged$Age[models_merged$Age=='[40,45[']<-'40 to 44'
models_merged$Age[models_merged$Age=='[45,50[']<-'45 to 49'
models_merged$Age[models_merged$Age=='[50,55[']<-'50 to 54'
models_merged$Age[models_merged$Age=='[55,60[']<-'55 to 59'
models_merged$Age[models_merged$Age=='[60,65[']<-'60 to 64'
models_merged$Age[models_merged$Age=='[65,70[']<-'65 to 69'
models_merged$Age[models_merged$Age=='[70,75[']<-'70 to 74'
models_merged$Age[models_merged$Age=='[75,80[']<-'75 to 79'
models_merged$Age[models_merged$Age=='[80,85[']<-'80 to 84'
models_merged$Age[models_merged$Age=='[85,90[']<-'85 to 89'

#Recode sex 
models_merged$Sex[models_merged$Sex=='All']<-'B'
models_merged$Sex[models_merged$Sex=='Male']<-'M'
models_merged$Sex[models_merged$Sex=='Female']<-'F'

#Recode province 
models_merged$Region[models_merged$Region=='Alberta']<-'AB'
models_merged$Region[models_merged$Region=='All']<-'CANADA'
models_merged$Region[models_merged$Region=='British Columbia']<-'BC'
models_merged$Region[models_merged$Region=='Manitoba']<-'MB'
models_merged$Region[models_merged$Region=='New Brunswick']<-'NB'
models_merged$Region[models_merged$Region=='Newfoundland']<-'NL'
models_merged$Region[models_merged$Region=='Nova Scotia']<-'NS'
models_merged$Region[models_merged$Region=='Ontario']<-'ON'
models_merged$Region[models_merged$Region=='P.E.I.']<-'PE'
models_merged$Region[models_merged$Region=='Quebec']<-'QC'
models_merged$Region[models_merged$Region=='Saskatchewan']<-'SK'

#Merge with control data to produce one data set
model_control <- inner_join(control_mortality , models_merged, by=c("Year","Sex","Age","Region"))

#Gather variables to start to create a skinny table, grouping rate variables 
model_control_rate <- model_control%>%
  select(Year,Sex,Age,Region,Control_rate,Model1_rate,Model2_rate,Model3_rate)%>%
  gather(key=Source,value=Rate,by=c(Control_rate,Model1_rate,Model2_rate,Model3_rate))

#Create labels for each model and control data type 
model_control_rate$Source[model_control_rate$Source=='Control_rate']<-'Control'
model_control_rate$Source[model_control_rate$Source=='Model1_rate']<-'Model1'
model_control_rate$Source[model_control_rate$Source=='Model2_rate']<-'Model2'
model_control_rate$Source[model_control_rate$Source=='Model3_rate']<-'Model3'

#Gather variables to start to create a skinny table, grouping count variables 
model_control_number <- model_control%>%
  select(Year,Sex,Age,Region,Control_number,Model1_number,Model2_number,Model3_number)%>%
  gather(key=Source,value=Number,by=c(Control_number,Model1_number,Model2_number,Model3_number))

#Create labels for each model and control data type 
model_control_number$Source[model_control_number$Source=='Control_number']<-'Control'
model_control_number$Source[model_control_number$Source=='Model1_number']<-'Model1'
model_control_number$Source[model_control_number$Source=='Model2_number']<-'Model2'
model_control_number$Source[model_control_number$Source=='Model3_number']<-'Model3'

#Gather variables to start to create a skinny table, grouping denominator variables 
# model_control_denom <- model_control%>%
#   select(Year,Sex,Age,Region,Control_denom,Model1_denom,Model2_denom,Model3_denom)%>%
#   gather(key=Source,value=Denom,by=c(Control_denom,Model1_denom,Model2_denom,Model3_denom))
# 
# #Create labels for each model and control data type 
# model_control_denom$Source[model_control_denom$Source=='Control_denom']<-'Control'
# model_control_denom$Source[model_control_denom$Source=='Model1_denom']<-'Model1'
# model_control_denom$Source[model_control_denom$Source=='Model2_denom']<-'Model2'
# model_control_denom$Source[model_control_denom$Source=='Model3_denom']<-'Model3'

#Join number, denominator and rate columns together 
Rate_Number <- inner_join(model_control_rate,model_control_number, by=c("Year","Sex","Age","Region","Source"))
#Rate_Number_Denom <- inner_join(Rate_Number,model_control_denom, by=c("Year","Sex","Age","Region","Source"))

#create final skinny table
health_data <- Rate_Number%>%
  select(Year,Sex,Age,Region,Source,Rate,Number)



