######TIDYING DCE DATA######
#next steps, working on block1_best to recode data into 1 for A, 2 for B and 0 for NoTx
#update remaining design scenario blocks
#update rename to include :3.27 to re-upload
#install.packages into another R script
#figure out where to put tidy_DCE, as part of or not as part of self-report
#check to make sure that the design scenarios are right

######LIBRARIES
library(here)
library(readr)
library(dplyr)
library(tidyr)

# #Create a .txt file within the errors folder
# tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_DCE.txt"), open = "wt")
# sink(tidy_selfreport_03, type = "message")

#import renamed dataset into environment
data_DCE<- read_rds(here("01_data","02_processed", "data_DCE_processed.rds"))
data_SR<- read_rds(here("01_data","02_processed", "data_SR_processed.rds")) %>% 
  mutate(across(c(ID, aversion_version, joint_study, joint_multi,
               NICE_diagnosis, pain_recode, hipL:PA_clinical_cutpoint,FiveDays:sex,BMI_recode: DCE_mono),as.factor)) 

data_SR_sensitivity<-read_rds(here("01_data","02_processed", "data_SR_processed_sensitivity.rds")) %>%  
 mutate(across(c(ID, aversion_version, joint_study, joint_multi,
              NICE_diagnosis,pain_recode, hipL:PA_clinical_cutpoint,FiveDays:sex,BMI_recode: DCE_mono),as.factor))

# ############SCRIPTS############
# #this script tidies DCE stated preference data for all participants
# source(here("02_scripts","03_tidy_DCE.R"))
# 
# #this script tidies self-report data for all participants
# source(here("02_scripts","03_tidy_selfreport.R"))

###### Creating tidy processed data#####
data_processed<-data_DCE %>% 
  left_join(data_SR, by="ID") %>% 
  arrange(desc(ID))

data_processed_sensitivity<-data_DCE %>% 
  left_join(data_SR_sensitivity, by="ID") %>% 
  arrange(desc(ID))

# #Save object to an rds file to preserve column data types
# saveRDS(data_processed,"01_data/02_processed/00_data_processed.rds")
# saveRDS(data_processed_sensitivity,"01_data/02_processed/00_data_processed_sensitivity.rds")

# #Write to CSV file
# write.csv(data_processed,"01_data/02_processed/00_data_processed.csv", row.names=FALSE)
# write.csv(data_processed_sensitivity,"01_data/02_processed/00_data_processed_sensitivity.csv", row.names=FALSE)

#summary(data_SR)
#                                   ID      aversion_version   risk_score       loss_score       identified   
# 00528be0-0b09-4a3f-24ac-bc796b309825:  1   1:154            Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
# 01394623-b527-bfb2-486f-711230d5bfe3:  1   2:156            1st Qu.: 5.000   1st Qu.: 1.000   1st Qu.:2.250  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:  1                    Median :11.000   Median : 1.000   Median :3.000  
# 028a7395-1463-aeb0-fbd9-44a167323f51:  1                    Mean   : 8.223   Mean   : 3.439   Mean   :2.859  
# 046f49fe-186f-e943-1eb2-7e44c9ce4236:  1                    3rd Qu.:12.000   3rd Qu.: 5.000   3rd Qu.:3.500  
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:  1                    Max.   :12.000   Max.   :12.000   Max.   :4.000  
# (Other)                             :304                                                      NA's   :3      
#   amotivation       intrinsic       integrated      extrinsic      joint_study  joint_number   joint_multi NICE_diagnosis
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
#     VigDays    DailyVigMin_trunc    ModDays    DailyModMin_trunc    WalkDays   DailyWalkMin_trunc TotalDailyPAMin_trunc
#  1      :135   1      :126       1      :117   1      :115       8      :116   12     :59         12     : 25          
#  6      : 34   10     : 41       3      : 38   10     : 43       6      : 39   7      :53         23     : 23          
#  3      : 31   6      : 31       2      : 35   6      : 38       4      : 37   1      :30         32     : 19          
#  4      : 31   21     : 16       4      : 35   19     : 21       5      : 30   5      :25         6      : 18          
#  5      : 25   17     : 14       8      : 31   16     : 17       1      : 26   22     :22         1      : 16          
#  (Other): 49   (Other): 58       (Other): 48   (Other): 52       (Other): 56   (Other):97         (Other):185          
#  NA's   :  5   NA's   : 24       NA's   :  6   NA's   : 24       NA's   :  6   NA's   :24         NA's   : 24          
# METs_Vig      METs_Mod     METs_Walk     METs_Total  FiveDays   SevenDays  IPAQ_cat   PA_clinical_cutpoint  sex   
# 1      :126   1      :115   34     : 31   1      : 16   0   : 60   0   : 99   1   : 82   0   :122             1   :180  
# 16     : 15   8      : 23   1      : 30   65     :  9   1   :244   1   :205   2   : 92   1   :164             2   :123  
# 25     : 14   12     : 16   20     : 19   8      :  6   NA's:  6   NA's:  6   3   :131   NA's: 24             NA's:  7  
# 9      : 13   18     : 14   22     : 19   23     :  5                         NA's:  5                                  
#  11     : 11   5      : 11   15     : 15   28     :  4                                                                   
#  (Other):107   (Other):107   (Other):172   (Other):246                                                                   
#  NA's   : 24   NA's   : 24   NA's   : 24   NA's   : 24                                                                   
#       age           BMI_calc           BMI_recode   income    income_recode employed   household      state    DCE_mono
#  Min.   :44.00   Min.   :16.00   healthy    : 55   1   : 15   high  : 20    1   :116   1   : 60   4      :86   0: 22   
#  1st Qu.:52.00   1st Qu.:25.00   obese      :116   2   : 69   low   : 84    2   :187   2   :241   6      :80   1:288   
#  Median :61.00   Median :28.00   overweight :108   3   :117   medium:174    NA's:  7   4   :  1   5      :67           
# Mean   :60.15   Mean   :29.02   underweight:  5   4   : 57   NA's  : 32               NA's:  8   3      :33           
# 3rd Qu.:67.00   3rd Qu.:32.00   NA's       : 26   5   : 20                                       1      :24           
#  Max.   :86.00   Max.   :50.00                     6   : 25                                       (Other):13           
#  NA's   :11      NA's   :26                        NA's:  7                                       NA's   : 7           
#     duration      
#  Min.   :  293.0  
#  1st Qu.:  846.2  
#  Median : 1060.5  
#  Mean   : 1464.2  
#  3rd Qu.: 1431.2  
#  Max.   :20424.0 

#summary(data_SR_sensitivity)
# #                                    ID      aversion_version   risk_score       loss_score      identified   
# 00528be0-0b09-4a3f-24ac-bc796b309825:  1   1:143            Min.   : 0.000   Min.   : 0.00   Min.   :0.000  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:  1   2:145            1st Qu.: 5.000   1st Qu.: 1.00   1st Qu.:2.250  
# 028a7395-1463-aeb0-fbd9-44a167323f51:  1                    Median :11.000   Median : 1.00   Median :3.000  
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:  1                    Mean   : 8.253   Mean   : 3.51   Mean   :2.826  
# 05df3ec6-f594-6343-a98a-09f4804aa700:  1                    3rd Qu.:12.000   3rd Qu.: 6.00   3rd Qu.:3.500  
# 0691c602-3175-2b96-9b4e-f1bafe7b24cc:  1                    Max.   :12.000   Max.   :12.00   Max.   :4.000  
# (Other)                             :282                                                     NA's   :3      
#   amotivation       intrinsic       integrated      extrinsic      joint_study  joint_number   joint_multi NICE_diagnosis
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
#     VigDays    DailyVigMin_trunc    ModDays    DailyModMin_trunc    WalkDays   DailyWalkMin_trunc TotalDailyPAMin_trunc
#  1      :128   1      :119       1      :114   1      :110       8      :107   12     :54         12     : 24          
#  6      : 31   10     : 38       3      : 36   10     : 40       6      : 38   7      :49         23     : 22          
#  4      : 29   6      : 30       2      : 32   6      : 33       4      : 34   1      :27         6      : 17          
#  3      : 28   21     : 15       4      : 30   19     : 21       5      : 29   5      :24         32     : 16          
#  2      : 21   17     : 13       8      : 29   16     : 14       1      : 25   22     :20         1      : 14          
#  (Other): 46   (Other): 51       (Other): 42   (Other): 48       (Other): 50   (Other):92         (Other):173          
#  NA's   :  5   NA's   : 22       NA's   :  5   NA's   : 22       NA's   :  5   NA's   :22         NA's   : 22          
# METs_Vig      METs_Mod     METs_Walk     METs_Total  FiveDays   SevenDays  IPAQ_cat   PA_clinical_cutpoint  sex   
# 1      :119   1      :110   34     : 29   1      : 14   0   : 58   0   : 94   1   : 78   0   :115             1   :165  
# 16     : 15   8      : 21   1      : 27   65     :  9   1   :225   1   :189   2   : 85   1   :151             2   :117  
# 9      : 13   12     : 15   20     : 16   8      :  6   NA's:  5   NA's:  5   3   :120   NA's: 22             NA's:  6  
# 25     : 12   18     : 11   22     : 16   23     :  4                         NA's:  5                                  
#  11     : 11   5      :  9   15     : 14   28     :  4                                                                   
#  (Other): 96   (Other):100   (Other):164   (Other):229                                                                   
#  NA's   : 22   NA's   : 22   NA's   : 22   NA's   : 22                                                                   
#       age           BMI_calc           BMI_recode   income    income_recode employed   household      state    DCE_mono
#  Min.   :45.00   Min.   :16.00   healthy    : 50   1   : 14   high  : 19    1   :108   1   : 55   4      :80   0:  0   
#  1st Qu.:52.00   1st Qu.:25.00   obese      :110   2   : 66   low   : 80    2   :174   2   :225   6      :78   1:288   
#  Median :60.00   Median :28.00   overweight :102   3   :108   medium:161    NA's:  6   4   :  1   5      :60           
# Mean   :59.97   Mean   :29.15   underweight:  4   4   : 53   NA's  : 28               NA's:  7   3      :30           
# 3rd Qu.:66.00   3rd Qu.:32.00   NA's       : 22   5   : 19                                       1      :22           
#  Max.   :86.00   Max.   :50.00                     6   : 22                                       (Other):12           
#  NA's   :9       NA's   :22                        NA's:  6                                       NA's   : 6           
#     duration      
#  Min.   :  293.0  
#  1st Qu.:  846.8  
#  Median : 1059.0  
#  Mean   : 1481.8  
#  3rd Qu.: 1436.0  
#  Max.   :20424.0  