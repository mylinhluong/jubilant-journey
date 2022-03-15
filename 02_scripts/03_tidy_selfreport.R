#NEXT STEPS
#look into local v global environmentment [new.env()]to clean up global environment and leave only dataset for analysis
#This script sources several scripts to creates a new DF that includes tidy data for non-choice set data:  
  # monotonicity (for sensitivity analysis)
  # IPAQ-SF 
  # the BREQ-3 
  # aversion (loss and risk) 
  # OA characteristics (incl NICE OA criteria, pain NRS and function NRS)
  # sociodemographics

#CHECK PACKRAT TO DELETE
#install.packages("summarytools") #https://statsandr.com/blog/descriptive-statistics-in-r/
#install.packages("gtsummary")
#transform mutate_at to across https://dplyr.tidyverse.org/dev/articles/colwise.html
#https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-colwise/
#https://stackoverflow.com/questions/49396267/dplyr-rowwise-sum-and-other-functions-like-max 
#summing across many columns: https://github.com/tidyverse/dplyr/issues/4544
#row-wise operations: https://dplyr.tidyverse.org/articles/rowwise.html

######LIBRARIES
library(here)
library(readr)
library(dplyr)
library(lubridate)

# #Create a .txt file within the errors folder
# tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_selfreport.txt"), open = "wt")
# sink(tidy_selfreport_03, type = "message")

#import renamed dataset into environment
data<- read_csv(here("01_data","02_processed", "data_rename.csv"))


#View(data)
############SCRIPTS############
#this script scores risk aversion, loss aversion, and notes which version they received
source(here("02_scripts","03_tidy_selfreport_aversion.R"))

#this script scores the BREQ-3 motivation scales
source(here("02_scripts", "03_tidy_selfreport_BREQ3.R"))

#this scripts scores the IPAQ-Short Form
source(here("02_scripts", "03_tidy_selfreport_IPAQ.R"))

#this script compiles information about the participant's joint pain and function, including whether or not the participant
#has a NICE diagnosis of osteoarthritis
source(here("02_scripts", "03_tidy_selfreport_OA_chars.R"))

#this script scores data checks for mononiticity and duration in secs the participant took to complete the study
source(here("02_scripts", "03_tidy_selfreport_sensitivity.R"))

#this script tidies sociodemographic data, including 
# calculating age from date of birth, cleaning height & weight data, calculating BMI
source(here("02_scripts", "03_tidy_selfreport_sociodems.R"))



###### Creating tidy self-report data#####
data_SR_processed<-data_aversion %>% 
  left_join(data_breq, by="ID") %>% 
  left_join(data_OA_chars, by="ID") %>% 
  left_join(data_ipaq, by="ID") %>% 
  left_join(data_sociodems, by="ID") %>% 
  left_join(data_sensitivity, by="ID") %>%
  mutate(across(c(ID, aversion_version, joint_study, joint_multi,
  NICE_diagnosis,pain_recode, hipL:PA_clinical_cutpoint,FiveDays:sex,BMI_recode: DCE_mono),as.factor)) %>%
  mutate(across(c(VigDays:METs_Total),as.numeric)) %>% 
  filter(!is.na(DCE_mono))
#View(data_SR_processed)

#these data exclude participants for whom choice monontonicity is assumed to hold
data_SR_processed_sensitivity<-data_aversion %>%
  left_join(data_breq, by="ID") %>%
  left_join(data_OA_chars, by="ID") %>%
  left_join(data_ipaq, by="ID") %>%
  left_join(data_sociodems, by="ID") %>%
  left_join(data_sensitivity, by="ID") %>%
  mutate(across(c(ID, aversion_version, joint_study, joint_multi,
              NICE_diagnosis, pain_recode, hipL:PA_clinical_cutpoint,FiveDays:sex,BMI_recode: DCE_mono),as.factor)) %>%
  mutate(across(c(VigDays:METs_Total),as.numeric)) %>%
  filter(!is.na(DCE_mono))%>%
  filter(DCE_mono==1)
#View(data_SR_processed_sensitivity)


#Save object to an rds file to preserve column data types
# saveRDS(data_SR_processed,"01_data/02_processed/data_SR_processed.rds")
# saveRDS(data_SR_processed_sensitivity,"01_data/02_processed/data_SR_processed_sensitivity.rds")
# 
# #Write to CSV file
# write.csv(data_SR_processed,"01_data/02_processed/data_SR_processed.csv", row.names=FALSE)
# write.csv(data_SR_processed_sensitivity,"01_data/02_processed/data_SR_processed_sensitivity.csv", row.names=FALSE)

# #end of script
# #close the error message catching script and save the file
# sink(type = "message")
# close(tidy_selfreport_03)
# 
# #Open the .txt file for inspection
# readLines(here("02_scripts","Errors", "03_tidy_selfreport.txt"))

# str(data_SR_processed)
# # tibble [310 x 51] (S3: tbl_df/tbl/data.frame)
# $ ID                   : Factor w/ 387 levels "00528be0-0b09-4a3f-24ac-bc796b309825",..: 111 164 218 244 327 361 385 160 321 333 ...
# $ aversion_version     : Factor w/ 2 levels "1","2": 2 1 1 2 1 2 2 1 1 1 ...
# $ risk_score           : num [1:310] 1 6 12 12 8 12 12 6 12 9 ...
# $ loss_score           : num [1:310] 1 1 2 12 1 1 1 1 12 1 ...
# $ identified           : num [1:310] 3 3.5 1 2.75 1.75 4 1.75 2 2 4 ...
# $ amotivation          : num [1:310] 3 0 0 2 1.75 0 0.25 2 0 3 ...
# $ intrinsic            : num [1:310] 3 3.75 0 3 1.25 3 2.25 2 2.5 3.75 ...
# $ integrated           : num [1:310] 3.25 3 0 2.75 1.25 4 1 2 2.75 4 ...
# $ external             : num [1:310] 3 0.25 0 2.25 2 0 0.25 2 0 4 ...
# $ joint_study          : Factor w/ 8 levels "1","2","3","4",..: 1 5 4 6 7 3 6 4 4 6 ...
# $ joint_number         : num [1:310] 1 3 2 2 4 2 2 2 2 2 ...
# $ joint_multi          : Factor w/ 2 levels "0","1": 1 2 2 2 2 2 2 2 2 2 ...
# $ NICE_diagnosis       : Factor w/ 2 levels "0","1": 2 2 2 1 1 1 2 2 1 2 ...
# $ pain_NRS             : num [1:310] 7 5 7 5 7 6 5 7 3 7 ...
# $ pain_recode          : Factor w/ 3 levels "mild","moderate",..: 2 1 2 1 2 2 1 2 1 2 ...
# $ function_NRS         : num [1:310] 7 3 8 5 7 8 2 7 4 7 ...
# $ hipL                 : Factor w/ 2 levels "0","1": 2 1 1 1 1 1 1 1 1 1 ...
# $ hipR                 : Factor w/ 2 levels "0","1": 1 1 1 1 1 2 1 1 2 1 ...
# $ ankleL               : Factor w/ 2 levels "0","1": 1 2 1 1 2 1 1 1 1 1 ...
# $ ankleR               : Factor w/ 2 levels "0","1": 1 2 1 1 2 1 1 1 1 1 ...
# $ kneeL                : Factor w/ 2 levels "0","1": 1 1 2 1 1 2 2 2 1 1 ...
# $ kneeR                : Factor w/ 2 levels "0","1": 1 1 2 2 1 1 1 2 2 1 ...
# $ footL                : Factor w/ 2 levels "0","1": 1 2 1 2 2 1 2 1 1 2 ...
# $ footR                : Factor w/ 2 levels "0","1": 1 1 1 1 2 1 1 1 1 2 ...
# $ PA_typical           : Factor w/ 3 levels "1","2","3": 3 1 1 1 1 1 1 2 1 2 ...
# $ VigDays              : num [1:310] 5 1 1 6 1 4 2 1 1 5 ...
# $ DailyVigMin_trunc    : num [1:310] 21 1 1 6 1 4 6 NA 1 21 ...
# $ ModDays              : num [1:310] 5 1 1 6 1 8 3 1 1 1 ...
# $ DailyModMin_trunc    : num [1:310] 19 1 1 6 1 10 10 NA 1 1 ...
# $ WalkDays             : num [1:310] 4 8 2 8 7 8 6 1 8 8 ...
# $ DailyWalkMin_trunc   : num [1:310] 22 1 1 6 7 5 5 NA 7 12 ...
# $ TotalDailyPAMin_trunc: num [1:310] 56 1 1 17 6 20 22 NA 6 40 ...
# $ METs_Vig             : num [1:310] 34 1 1 14 1 9 5 NA 1 34 ...
# $ METs_Mod             : num [1:310] 34 1 1 16 1 28 12 NA 1 1 ...
# $ METs_Walk            : num [1:310] 38 1 1 19 20 17 13 NA 22 34 ...
# $ METs_Total           : num [1:310] 208 1 1 105 23 114 46 NA 28 199 ...
# $ FiveDays             : Factor w/ 2 levels "0","1": 2 2 1 2 2 2 2 1 2 2 ...
# $ SevenDays            : Factor w/ 2 levels "0","1": 2 2 1 2 1 2 2 1 2 2 ...
# $ IPAQ_cat             : Factor w/ 3 levels "1","2","3": 3 1 1 3 2 3 2 1 2 3 ...
# $ PA_clinical_cutpoint : Factor w/ 2 levels "0","1": 2 1 1 2 1 2 2 NA 1 2 ...
# $ sex               : Factor w/ 2 levels "1","2": 2 1 2 1 1 1 1 1 2 2 ...
# $ age                  : num [1:310] 51 70 51 50 64 50 55 47 60 54 ...
# $ BMI_calc             : num [1:310] NA 23 28 24 24 18 32 22 26 NA ...
# $ BMI_recode           : Factor w/ 4 levels "healthy","obese",..: NA 1 3 1 1 4 2 1 3 NA ...
# $ income               : Factor w/ 6 levels "1","2","3","4",..: 1 2 3 5 3 6 1 3 5 2 ...
# $ income_recode        : Factor w/ 3 levels "high","low","medium": 2 2 3 1 3 NA 2 3 1 2 ...
# $ employed             : Factor w/ 2 levels "1","2": 2 2 2 1 2 2 2 2 1 2 ...
# $ household            : Factor w/ 3 levels "1","2","4": 1 1 2 2 2 2 2 2 2 2 ...
# $ state                : Factor w/ 8 levels "1","2","3","4",..: 6 6 4 4 1 1 1 6 5 1 ...
# $ DCE_mono             : Factor w/ 2 levels "0","1": 2 2 2 2 2 2 2 2 2 2 ...
#$ duration             : num [1:310] 439 721 495 708 914 583 995 960 831 808 ...

#summary(data_SR_processed)
#                                    ID      aversion_version   risk_score       loss_score       identified   
# 00528be0-0b09-4a3f-24ac-bc796b309825:  1   1:154            Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
# 01394623-b527-bfb2-486f-711230d5bfe3:  1   2:156            1st Qu.: 5.000   1st Qu.: 1.000   1st Qu.:2.250  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:  1                    Median :11.000   Median : 1.000   Median :3.000  
# 028a7395-1463-aeb0-fbd9-44a167323f51:  1                    Mean   : 8.223   Mean   : 3.439   Mean   :2.859  
# 046f49fe-186f-e943-1eb2-7e44c9ce4236:  1                    3rd Qu.:12.000   3rd Qu.: 5.000   3rd Qu.:3.500  
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:  1                    Max.   :12.000   Max.   :12.000   Max.   :4.000  
# (Other)                             :304                                                      NA's   :3      
#   amotivation       intrinsic       integrated       external      joint_study  joint_number   joint_multi NICE_diagnosis
#  Min.   :0.0000   Min.   :0.000   Min.   :0.000   Min.   :0.000   4      :70   Min.   :0.000   0:101       0   :131      
#  1st Qu.:0.0000   1st Qu.:1.500   1st Qu.:1.500   1st Qu.:0.000   3      :52   1st Qu.:1.000   1:209       1   :173      
#  Median :0.0000   Median :2.500   Median :2.250   Median :1.000   2      :51   Median :2.000               NA's:  6      
# Mean   :0.5448   Mean   :2.352   Mean   :2.341   Mean   :1.061   1      :39   Mean   :2.403                             
# 3rd Qu.:1.0000   3rd Qu.:3.250   3rd Qu.:3.250   3rd Qu.:1.750   8      :27   3rd Qu.:3.000                             
# Max.   :4.0000   Max.   :4.000   Max.   :4.000   Max.   :4.000   (Other):58   Max.   :8.000                             
# NA's   :3        NA's   :3       NA's   :3       NA's   :3       NA's   :13                                             
#     pain_NRS        pain_recode   function_NRS    hipL    hipR    ankleL  ankleR  kneeL   kneeR   footL   footR   PA_typical
#  Min.   : 1.000   mild    :166   Min.   : 0.000   0:221   0:206   0:257   0:250   0:183   0:154   0:235   0:229   1   :239  
#  1st Qu.: 4.000   moderate: 99   1st Qu.: 3.000   1: 89   1:104   1: 53   1: 60   1:127   1:156   1: 75   1: 81   2   : 26  
#  Median : 5.000   severe  : 39   Median : 5.000                                                                   3   : 37  
#  Mean   : 5.276   NA's    :  6   Mean   : 5.234                                                                   NA's:  8  
#  3rd Qu.: 7.000                  3rd Qu.: 7.000                                                                             
#  Max.   :10.000                  Max.   :10.000                                                                             
#  NA's   :6                       NA's   :6                                                                                  
#     VigDays      DailyVigMin_trunc    ModDays      DailyModMin_trunc    WalkDays     DailyWalkMin_trunc TotalDailyPAMin_trunc
#  Min.   :1.000   Min.   : 1.000    Min.   :1.000   Min.   : 1.000    Min.   :1.000   Min.   : 1.00      Min.   : 1.00        
#  1st Qu.:1.000   1st Qu.: 1.000    1st Qu.:1.000   1st Qu.: 1.000    1st Qu.:4.000   1st Qu.: 5.00      1st Qu.:12.00        
#  Median :2.000   Median : 6.000    Median :2.500   Median : 6.000    Median :6.000   Median :10.00      Median :23.00        
#  Mean   :3.066   Mean   : 6.748    Mean   :3.178   Mean   : 6.633    Mean   :5.694   Mean   :10.23      Mean   :23.48        
#  3rd Qu.:5.000   3rd Qu.:10.000    3rd Qu.:5.000   3rd Qu.:10.000    3rd Qu.:8.000   3rd Qu.:14.75      3rd Qu.:32.00        
#  Max.   :8.000   Max.   :21.000    Max.   :8.000   Max.   :19.000    Max.   :8.000   Max.   :22.00      Max.   :56.00        
#  NA's   :5       NA's   :24        NA's   :6       NA's   :24        NA's   :6       NA's   :24         NA's   :24           
# METs_Vig        METs_Mod       METs_Walk       METs_Total     FiveDays   SevenDays  IPAQ_cat   PA_clinical_cutpoint
# Min.   : 1.00   Min.   : 1.00   Min.   : 1.00   Min.   :  1.00   0   : 60   0   : 99   1   : 82   0   :122            
# 1st Qu.: 1.00   1st Qu.: 1.00   1st Qu.:12.00   1st Qu.: 33.25   1   :244   1   :205   2   : 92   1   :164            
# Median : 8.50   Median : 8.00   Median :22.00   Median : 90.50   NA's:  6   NA's:  6   3   :131   NA's: 24            
#  Mean   :11.05   Mean   :11.27   Mean   :22.82   Mean   : 96.34                         NA's:  5                       
# 3rd Qu.:19.00   3rd Qu.:19.00   3rd Qu.:34.00   3rd Qu.:153.75                                                        
# Max.   :39.00   Max.   :39.00   Max.   :49.00   Max.   :222.00                                                        
# NA's   :24      NA's   :24      NA's   :24      NA's   :24                                                            
# sex         age           BMI_calc           BMI_recode   income    income_recode employed   household      state   
# 1   :180   Min.   :44.00   Min.   :16.00   healthy    : 55   1   : 15   high  : 20    1   :116   1   : 60   4      :86  
# 2   :123   1st Qu.:52.00   1st Qu.:25.00   obese      :116   2   : 69   low   : 84    2   :187   2   :241   6      :80  
# NA's:  7   Median :61.00   Median :28.00   overweight :108   3   :117   medium:174    NA's:  7   4   :  1   5      :67  
# Mean   :60.15   Mean   :29.02   underweight:  5   4   : 57   NA's  : 32               NA's:  8   3      :33  
# 3rd Qu.:67.00   3rd Qu.:32.00   NA's       : 26   5   : 20                                       1      :24  
#             Max.   :86.00   Max.   :50.00                     6   : 25                                       (Other):13  
#             NA's   :11      NA's   :26                        NA's:  7                                       NA's   : 7  
#  DCE_mono    duration      
#  0: 22    Min.   :  293.0  
#  1:288    1st Qu.:  846.2  
#           Median : 1060.5  
#           Mean   : 1464.2  
#           3rd Qu.: 1431.2  
#           Max.   :20424.0  

#summary(data_SR_processed_sensitivity)
# #                                     ID      aversion_version   risk_score       loss_score      identified   
# 00528be0-0b09-4a3f-24ac-bc796b309825:  1   1:143            Min.   : 0.000   Min.   : 0.00   Min.   :0.000  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:  1   2:145            1st Qu.: 5.000   1st Qu.: 1.00   1st Qu.:2.250  
# 028a7395-1463-aeb0-fbd9-44a167323f51:  1                    Median :11.000   Median : 1.00   Median :3.000  
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:  1                    Mean   : 8.253   Mean   : 3.51   Mean   :2.826  
# 05df3ec6-f594-6343-a98a-09f4804aa700:  1                    3rd Qu.:12.000   3rd Qu.: 6.00   3rd Qu.:3.500  
# 0691c602-3175-2b96-9b4e-f1bafe7b24cc:  1                    Max.   :12.000   Max.   :12.00   Max.   :4.000  
# (Other)                             :282                                                     NA's   :3      
#   amotivation       intrinsic       integrated       external      joint_study  joint_number   joint_multi NICE_diagnosis
#  Min.   :0.0000   Min.   :0.000   Min.   :0.000   Min.   :0.000   4      :62   Min.   :0.000   0: 89       0   :121      
#  1st Qu.:0.0000   1st Qu.:1.500   1st Qu.:1.500   1st Qu.:0.000   2      :48   1st Qu.:1.000   1:199       1   :162      
#  Median :0.0000   Median :2.250   Median :2.250   Median :1.000   3      :48   Median :2.000               NA's:  5      
# Mean   :0.5132   Mean   :2.303   Mean   :2.284   Mean   :1.016   1      :38   Mean   :2.451                             
# 3rd Qu.:1.0000   3rd Qu.:3.250   3rd Qu.:3.000   3rd Qu.:1.750   6      :25   3rd Qu.:3.000                             
# Max.   :3.7500   Max.   :4.000   Max.   :4.000   Max.   :4.000   (Other):55   Max.   :8.000                             
# NA's   :3        NA's   :3       NA's   :3       NA's   :3       NA's   :12                                             
#     pain_NRS        pain_recode   function_NRS   hipL    hipR    ankleL  ankleR  kneeL   kneeR   footL   footR   PA_typical
#  Min.   : 1.000   mild    :158   Min.   : 0.00   0:202   0:191   0:235   0:234   0:167   0:143   0:215   0:211   1   :223  
#  1st Qu.: 4.000   moderate: 93   1st Qu.: 3.00   1: 86   1: 97   1: 53   1: 54   1:121   1:145   1: 73   1: 77   2   : 24  
#  Median : 5.000   severe  : 32   Median : 5.00                                                                   3   : 35  
#  Mean   : 5.184   NA's    :  5   Mean   : 5.12                                                                   NA's:  6  
#  3rd Qu.: 7.000                  3rd Qu.: 7.00                                                                             
#  Max.   :10.000                  Max.   :10.00                                                                             
#  NA's   :5                       NA's   :5                                                                                 
#     VigDays      DailyVigMin_trunc    ModDays      DailyModMin_trunc    WalkDays     DailyWalkMin_trunc TotalDailyPAMin_trunc
#  Min.   :1.000   Min.   : 1.000    Min.   :1.000   Min.   : 1.000    Min.   :1.000   Min.   : 1.00      Min.   : 1.00        
#  1st Qu.:1.000   1st Qu.: 1.000    1st Qu.:1.000   1st Qu.: 1.000    1st Qu.:4.000   1st Qu.: 5.00      1st Qu.:12.00        
#  Median :2.000   Median : 6.000    Median :2.000   Median : 6.000    Median :6.000   Median :10.00      Median :23.00        
#  Mean   :3.004   Mean   : 6.711    Mean   :3.113   Mean   : 6.564    Mean   :5.657   Mean   :10.25      Mean   :23.38        
#  3rd Qu.:5.000   3rd Qu.:10.000    3rd Qu.:4.500   3rd Qu.:10.000    3rd Qu.:8.000   3rd Qu.:14.75      3rd Qu.:32.00        
#  Max.   :8.000   Max.   :21.000    Max.   :8.000   Max.   :19.000    Max.   :8.000   Max.   :22.00      Max.   :56.00        
#  NA's   :5       NA's   :22        NA's   :5       NA's   :22        NA's   :5       NA's   :22         NA's   :22           
# METs_Vig        METs_Mod       METs_Walk       METs_Total     FiveDays   SevenDays  IPAQ_cat   PA_clinical_cutpoint
# Min.   : 1.00   Min.   : 1.00   Min.   : 1.00   Min.   :  1.00   0   : 58   0   : 94   1   : 78   0   :115            
# 1st Qu.: 1.00   1st Qu.: 1.00   1st Qu.:12.00   1st Qu.: 32.00   1   :225   1   :189   2   : 85   1   :151            
# Median : 7.50   Median : 8.00   Median :22.00   Median : 90.00   NA's:  5   NA's:  5   3   :120   NA's: 22            
#  Mean   :10.86   Mean   :11.08   Mean   :22.82   Mean   : 95.41                         NA's:  5                       
# 3rd Qu.:19.00   3rd Qu.:19.00   3rd Qu.:34.00   3rd Qu.:152.75                                                        
# Max.   :39.00   Max.   :39.00   Max.   :49.00   Max.   :222.00                                                        
# NA's   :22      NA's   :22      NA's   :22      NA's   :22                                                            
# sex         age           BMI_calc           BMI_recode   income    income_recode employed   household      state   
# 1   :165   Min.   :45.00   Min.   :16.00   healthy    : 50   1   : 14   high  : 19    1   :108   1   : 55   4      :80  
# 2   :117   1st Qu.:52.00   1st Qu.:25.00   obese      :110   2   : 66   low   : 80    2   :174   2   :225   6      :78  
# NA's:  6   Median :60.00   Median :28.00   overweight :102   3   :108   medium:161    NA's:  6   4   :  1   5      :60  
# Mean   :59.97   Mean   :29.15   underweight:  4   4   : 53   NA's  : 28               NA's:  7   3      :30  
# 3rd Qu.:66.00   3rd Qu.:32.00   NA's       : 22   5   : 19                                       1      :22  
#             Max.   :86.00   Max.   :50.00                     6   : 22                                       (Other):12  
#             NA's   :9       NA's   :22                        NA's:  6                                       NA's   : 6  
#  DCE_mono    duration      
#  0:  0    Min.   :  293.0  
#  1:288    1st Qu.:  846.8  
#           Median : 1059.0  
#           Mean   : 1481.8  
#           3rd Qu.: 1436.0  
#           Max.   :20424.0  
#                            

##################
#CORRELATIONS    #
##################
summary(data_SR_processed)
data_corr<-select(data_SR_processed, aversion_version=aversion_version,
                  risk_score=risk_score, loss_score=loss_score, intrinsic=intrinsic, external=external, 
                  joint_study=joint_study, NICE_diagnosis=NICE_diagnosis, pain_NRS=pain_NRS, pain_recode=pain_recode, 
                  function_NRS=function_NRS, METs_Total=METs_Total, IPAQ_cat=IPAQ_cat,
                  sex=sex, age=age, BMI_calc=BMI_calc, BMI_recode=BMI_recode, income=income, income_recode=income_recode, 
                  employed=employed, state=state, DCE_mono=DCE_mono)
                  
                

# summary(data_corr)
# # 
# 
# corr_matrix<-rcorr(as.matrix(data_corr),
#                    type="pearson")
# 
# corr_matrix
# #  
# 
# flattenCorrMatrix(corr_matrix$r, corr_matrix$P)
# #
# 
