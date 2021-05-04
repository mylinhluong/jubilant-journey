library(mlogit)
library(readr)
library(here)
library(dplyr)
library(lubridate)

ParticipantData<-self_report%>%
  mutate_all(list(~na_if(.,"")))%>%
  filter(NICE_age==4 & NICE_pain3mos==1 & Consent==1 & Complete==1)%>%
  select(ResponseId, Complete_time:NICE_stiffness_min,Consent,Study_joint:Averse_block)%>%
  mutate(DCE_block=as.factor(as.numeric(DCE_block)))%>%
  mutate(Averse_block=as.factor(as.character(Averse_block)))%>%
  mutate(DOB=as.character(as.factor(DOB)))%>%
  mutate(Height=as.numeric(as.factor(Height)))%>%
  mutate(Weight=as.numeric(as.factor(Weight)))%>%
  mutate(pain_NRS=as.numeric(as.factor(pain_NRS)))%>%
  mutate(function_NRS=as.numeric(as.factor(function_NRS)))%>%
  mutate(Complete_time=as.numeric(as.factor(Complete_time)))

str(ParticipantData)
summary(ParticipantData)

summary(self_report_wide1)
#Participant Selection_1_1  Selection_1_2  Selection_1_3     Selection_2_1  Selection_2_2  Selection_2_3    Selection_3_1  Selection_3_2 
#R_0fIeZQLpYeksyEp: 1   Min.   :0.00   Min.   :0.00   Min.   :0.00000   Min.   :0.00   Min.   :0.00   Min.   :0.0000   Min.   :0.00   Min.   :0.00  
#R_10GzC0XXUaKyIVo: 1   1st Qu.:0.00   1st Qu.:1.00   1st Qu.:0.00000   1st Qu.:0.00   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.00  
#R_11gpXdnfYR7gCnV: 1   Median :0.00   Median :1.00   Median :0.00000   Median :1.00   Median :0.00   Median :0.0000   Median :0.00   Median :1.00  
#R_1C3D6dwxRAocTna: 1   Mean   :0.02   Mean   :0.91   Mean   :0.04124   Mean   :0.64   Mean   :0.16   Mean   :0.1753   Mean   :0.14   Mean   :0.66  
#R_1dtcYXhwX459zkb: 1   3rd Qu.:0.00   3rd Qu.:1.00   3rd Qu.:0.00000   3rd Qu.:1.00   3rd Qu.:0.00   3rd Qu.:0.0000   3rd Qu.:0.00   3rd Qu.:1.00  
#R_1ewzXXpKiOxCJ2t: 1   Max.   :1.00   Max.   :1.00   Max.   :1.00000   Max.   :1.00   Max.   :1.00   Max.   :1.0000   Max.   :1.00   Max.   :1.00  
#(Other)          :94                                 NA's   :3                                       NA's   :3                                     

#Selection_3_3    Selection_4_1  Selection_4_2  Selection_4_3    Selection_5_1 Selection_5_2  Selection_5_3     Selection_6_1  Selection_6_2 
#Min.   :0.0000   Min.   :0.00   Min.   :0.00   Min.   :0.0000   Min.   :0.0   Min.   :0.00   Min.   :0.00000   Min.   :0.00   Min.   :0.00  
#1st Qu.:0.0000   1st Qu.:1.00   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:0.0   1st Qu.:1.00   1st Qu.:0.00000   1st Qu.:1.00   1st Qu.:0.00  
#Median :0.0000   Median :1.00   Median :0.00   Median :0.0000   Median :0.0   Median :1.00   Median :0.00000   Median :1.00   Median :0.00  
#Mean   :0.1837   Mean   :0.85   Mean   :0.11   Mean   :0.0303   Mean   :0.1   Mean   :0.83   Mean   :0.05102   Mean   :0.82   Mean   :0.13  
#3rd Qu.:0.0000   3rd Qu.:1.00   3rd Qu.:0.00   3rd Qu.:0.0000   3rd Qu.:0.0   3rd Qu.:1.00   3rd Qu.:0.00000   3rd Qu.:1.00   3rd Qu.:0.00  
#Max.   :1.0000   Max.   :1.00   Max.   :1.00   Max.   :1.0000   Max.   :1.0   Max.   :1.00   Max.   :1.00000   Max.   :1.00   Max.   :1.00  
#NA's   :2                                      NA's   :1                                     NA's   :2                                      

#Selection_6_3    Selection_7_1  Selection_7_2  Selection_7_3 Selection_8_1  Selection_8_2  Selection_8_3       Version 
#Min.   :0.0000   Min.   :0.00   Min.   :0.00   Min.   :0.0   Min.   :0.00   Min.   :0.00   Min.   :0.0000   Min.   :1  
#1st Qu.:0.0000   1st Qu.:0.00   1st Qu.:0.00   1st Qu.:0.0   1st Qu.:0.00   1st Qu.:0.00   1st Qu.:0.0000   1st Qu.:1  
#Median :0.0000   Median :0.00   Median :1.00   Median :0.0   Median :0.00   Median :1.00   Median :0.0000   Median :1  
#Mean   :0.0404   Mean   :0.28   Mean   :0.52   Mean   :0.2   Mean   :0.12   Mean   :0.67   Mean   :0.1939   Mean   :1  
#3rd Qu.:0.0000   3rd Qu.:1.00   3rd Qu.:1.00   3rd Qu.:0.0   3rd Qu.:0.00   3rd Qu.:1.00   3rd Qu.:0.0000   3rd Qu.:1  
#Max.   :1.0000   Max.   :1.00   Max.   :1.00   Max.   :1.0   Max.   :1.00   Max.   :1.00   Max.   :1.0000   Max.   :1  
#NA's   :1                                                                                  NA's   :2                   

summary(self_report_wide2)
#Participant Selection_1_1    Selection_1_2 Selection_1_3     Selection_2_1    Selection_2_2    Selection_2_3    Selection_3_1   
#R_0DGDSZ4Kr1XUZRn: 1   Min.   :0.0000   Min.   :0.0   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#R_0HembqJRSKvr7cl: 1   1st Qu.:0.0000   1st Qu.:0.0   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:1.0000  
#R_0ktSYOHRtQ62qqJ: 1   Median :0.0000   Median :0.5   Median :0.00000   Median :1.0000   Median :0.0000   Median :0.0000   Median :1.0000  
#R_10peO0wd15jhTtC: 1   Mean   :0.4778   Mean   :0.5   Mean   :0.02222   Mean   :0.7222   Mean   :0.1667   Mean   :0.1011   Mean   :0.8111  
#R_10YSiJthBKMaPAV: 1   3rd Qu.:1.0000   3rd Qu.:1.0   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:1.0000  
#R_1207Ib6sNrXiUo7: 1   Max.   :1.0000   Max.   :1.0   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
#(Other)          :84                                                                                      NA's   :1                        

#Selection_3_2    Selection_3_3     Selection_4_1    Selection_4_2     Selection_4_3     Selection_5_1 Selection_5_2    Selection_5_3   
#Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.00000   Min.   :0.00000   Min.   :0.0   Min.   :0.0000   Min.   :0.0000  
#1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:1.0000   1st Qu.:0.00000   1st Qu.:0.00000   1st Qu.:0.0   1st Qu.:0.0000   1st Qu.:0.0000  
#Median :0.0000   Median :0.00000   Median :1.0000   Median :0.00000   Median :0.00000   Median :0.0   Median :1.0000   Median :0.0000  
#Mean   :0.1556   Mean   :0.02247   Mean   :0.8667   Mean   :0.08889   Mean   :0.04444   Mean   :0.3   Mean   :0.5222   Mean   :0.1685  
#3rd Qu.:0.0000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:0.00000   3rd Qu.:1.0   3rd Qu.:1.0000   3rd Qu.:0.0000  
#Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.00000   Max.   :1.00000   Max.   :1.0   Max.   :1.0000   Max.   :1.0000  
#NA's   :1                                                                                             NA's   :1       

#Selection_6_1    Selection_6_2    Selection_6_3     Selection_7_1    Selection_7_2    Selection_7_3     Selection_8_1    Selection_8_2   
#Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000   Min.   :0.0000   Min.   :0.0000  
#1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000   1st Qu.:0.0000   1st Qu.:0.0000  
#Median :0.0000   Median :1.0000   Median :0.00000   Median :0.0000   Median :0.0000   Median :0.00000   Median :0.0000   Median :0.0000  
#Mean   :0.2333   Mean   :0.7333   Mean   :0.02247   Mean   :0.4778   Mean   :0.4444   Mean   :0.06742   Mean   :0.4556   Mean   :0.4444  
#3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000   3rd Qu.:1.0000   3rd Qu.:1.0000  
#Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000   Max.   :1.0000   Max.   :1.0000  
#NA's   :1                                           NA's   :1                                          
#Selection_8_3    Version 
#Min.   :0.0   Min.   :2  
#1st Qu.:0.0   1st Qu.:2  
#Median :0.0   Median :2  
#Mean   :0.1   Mean   :2  
#3rd Qu.:0.0   3rd Qu.:2  
#Max.   :1.0   Max.   :2  

NICE_participants<-ParticipantData%>%
  filter(Consent==1, NICE_age==4 & NICE_pain3mos==1 & NICE_activity==1 & (NICE_stiffnessYN==3|(NICE_stiffnessYN==1&NICE_stiffness_min!=5)))



View(NICE_participants)

View(ParticipantData)

summary(ParticipantData)

colnames(self_report)

#want to change to complete_data_processed to DCE data, since that is all it includes atm

#import data
data <- read_csv(here("01_data","02_processed","DCE_data_processed.csv"))
#View(data)

data<-data%>%
  select(Participant,Version, Trial, Selection, Alt, Goal, Form, Magnitude, Direction)

xtabs(~Goal+Selection, data=data)
#      Selection
#Goal   0    1
#30min  729  909
#60min  843  752
#90min 1072  543
#SQ    2204  186

plot(xtabs(~Goal+Selection, data=data))

xtabs(~Form+Selection, data=data)
#       Selection
#Form     0    1
#Cash     532 1083
#Donate  1359  256
#SQ      2204  186
#Voucher  753  865

plot(xtabs(~Form+Selection, data=data))

xtabs(~Magnitude+Selection, data=data)
#            Selection
#Magnitude    0    1
#160         1055  573
#300         876  749
#500          713  882
#SQ          2204  186
plot(xtabs(~Magnitude+Selection, data=data))

xtabs(~Direction+Selection, data=data)
#         Selection
#Direction    0    1
#neg        1328 1096
#pos        1316 1108
#SQ         2204  186
plot(xtabs(~Direction+Selection, data=data))


describe(data)
#             vars    n   mean    sd median trimmed    mad min max range  skew kurtosis   se
#Participant*    1 7272 152.00 87.47  152.0  152.00 112.68   1 303   302  0.00    -1.20 1.03
#Version         2 7272   2.04  0.84    2.0    2.05   1.48   1   3     2 -0.08    -1.57 0.01
#Trial           3 7272   4.50  2.29    4.5    4.50   2.97   1   8     7  0.00    -1.24 0.03
#Selection       4 7238   0.33  0.47    0.0    0.29   0.00   0   1     1  0.72    -1.48 0.01
#Alt*            5 7272   2.00  0.82    2.0    2.00   1.48   1   3     2  0.00    -1.50 0.01
#Goal*           6 7272   2.66  1.16    3.0    2.70   1.48   1   4     3 -0.19    -1.42 0.01
#Form*           7 7272   2.56  1.07    3.0    2.57   1.48   1   4     3 -0.15    -1.22 0.01
#Magnitude*      8 7272   2.66  1.16    3.0    2.70   1.48   1   4     3 -0.18    -1.42 0.01
#Direction*      9 7272   2.00  0.82    2.0    2.00   1.48   1   3     2  0.00    -1.50 0.01