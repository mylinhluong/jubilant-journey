#This script visually inspects self-report data
library(here)
library(readr)
library(dplyr)

#Create a .txt file within the errors folder
tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_selfreport.txt"), open = "wt")
sink(tidy_selfreport_03, type = "message")


data_inspect_SR<-read_rds(here("01_data/02_processed/data_SR_processed.rds"))
data_inspect_DCE<-read_rds(here("01_data/02_processed/data_DCE_processed.rds"))
data_inspect_full<-read_rds(here("01_data/02_processed/00_data_processed.rds"))

#data_inspect_SR %>% 
#  mutate(across(c(ID, aversion_version, joint_study, joint_multi,
#                #NICE_diagnosis, hipL:PA_typical,FiveDays:gender,income: DCE_mono),as.factor))

str(data_inspect_SR)
summary(data_inspect_SR)
# #                                   ID      aversion_version   risk_score       loss_score       identified   
# 00528be0-0b09-4a3f-24ac-bc796b309825:  1   1:154            Min.   : 0.000   Min.   : 0.000   Min.   :0.000  
# 01394623-b527-bfb2-486f-711230d5bfe3:  1   2:156            1st Qu.: 5.000   1st Qu.: 1.000   1st Qu.:2.250  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:  1                    Median :11.000   Median : 1.000   Median :3.000  
# 028a7395-1463-aeb0-fbd9-44a167323f51:  1                    Mean   : 8.223   Mean   : 3.439   Mean   :2.859  
# 046f49fe-186f-e943-1eb2-7e44c9ce4236:  1                    3rd Qu.:12.000   3rd Qu.: 5.000   3rd Qu.:3.500  
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:  1                    Max.   :12.000   Max.   :12.000   Max.   :4.000  
# (Other)                             :304                                                      NA's   :3      

# amotivation       intrinsic       integrated      extrinsic      joint_study  joint_number   joint_multi NICE_diagnosis
# Min.   :0.0000   Min.   :0.000   Min.   :0.000   Min.   :0.000   4      :70   Min.   :0.000   0:101       0   :131      
# 1st Qu.:0.0000   1st Qu.:1.500   1st Qu.:1.500   1st Qu.:0.000   3      :52   1st Qu.:1.000   1:209       1   :173      
# Median :0.0000   Median :2.500   Median :2.250   Median :1.000   2      :51   Median :2.000               NA's:  6      
# Mean   :0.5448   Mean   :2.352   Mean   :2.341   Mean   :1.061   1      :39   Mean   :2.403                             
# 3rd Qu.:1.0000   3rd Qu.:3.250   3rd Qu.:3.250   3rd Qu.:1.750   8      :27   3rd Qu.:3.000                             
# Max.   :4.0000   Max.   :4.000   Max.   :4.000   Max.   :4.000   (Other):58   Max.   :8.000                             
# NA's   :3        NA's   :3       NA's   :3       NA's   :3       NA's   :13                                             

# pain_NRS          function_NRS    hipL    hipR    ankleL  ankleR  kneeL   kneeR   footL   footR   PA_typical    VigDays     
# Min.   : 1.000   Min.   : 0.000   0:221   0:206   0:257   0:250   0:183   0:154   0:235   0:229   1   :227   Min.   :1.000  
# 1st Qu.: 4.000   1st Qu.: 3.000   1: 89   1:104   1: 53   1: 60   1:127   1:156   1: 75   1: 81   2   : 24   1st Qu.:1.000  
# Median : 5.000   Median : 5.000                                                                   3   : 33   Median :3.000  
# Mean   : 5.276   Mean   : 5.234                                                                   NA's: 26   Mean   :3.179  
#  3rd Qu.: 7.000   3rd Qu.: 7.000                                                                              3rd Qu.:5.000  
#  Max.   :10.000   Max.   :10.000                                                                              Max.   :8.000  
#  NA's   :6        NA's   :6                                                                                   NA's   :25     

#  TotalVigMin_trunc    ModDays      TotalModMin_trunc    WalkDays     TotalWalkMin_trunc TotalPAMin_trunc    METs_Vig    
#  Min.   : 1.00     Min.   :1.000   Min.   : 1.00     Min.   :1.000   Min.   : 1.00      Min.   :  1.00   Min.   : 1.00  
#  1st Qu.: 1.00     1st Qu.:1.000   1st Qu.: 1.00     1st Qu.:4.000   1st Qu.:12.00      1st Qu.: 23.00   1st Qu.: 1.00  
#  Median : 8.00     Median :3.000   Median : 8.00     Median :6.000   Median :22.00      Median : 45.00   Median : 8.00  
#  Mean   :11.02     Mean   :3.298   Mean   :11.17     Mean   :5.982   Mean   :22.73      Mean   : 53.47   Mean   :11.02  
#  3rd Qu.:19.00     3rd Qu.:5.000   3rd Qu.:19.00     3rd Qu.:8.000   3rd Qu.:34.00      3rd Qu.: 86.00   3rd Qu.:19.00  
#  Max.   :40.00     Max.   :8.000   Max.   :39.00     Max.   :8.000   Max.   :49.00      Max.   :126.00   Max.   :40.00  
#  NA's   :25        NA's   :25      NA's   :25        NA's   :25      NA's   :25         NA's   :25       NA's   :25     

#     METs_Mod       METs_Walk       METs_Total    FiveDays   SevenDays  IPAQ_cat    gender         age           BMI_calc    
#  Min.   : 1.00   Min.   : 1.00   Min.   :  1.0   0   : 42   0   : 81   1   : 46   1   :180   Min.   :44.00   Min.   :16.00  
#  1st Qu.: 1.00   1st Qu.:12.00   1st Qu.: 33.0   1   :243   1   :204   2   : 93   2   :123   1st Qu.:52.00   1st Qu.:25.00  
#  Median : 8.00   Median :22.00   Median : 90.0   NA's: 25   NA's: 25   3   :130   NA's:  7   Median :61.00   Median :28.00  
#  Mean   :11.17   Mean   :22.73   Mean   : 95.9                         NA's: 41              Mean   :60.15   Mean   :28.99  
#  3rd Qu.:19.00   3rd Qu.:34.00   3rd Qu.:153.0                                               3rd Qu.:67.00   3rd Qu.:32.00  
#  Max.   :39.00   Max.   :49.00   Max.   :221.0                                               Max.   :86.00   Max.   :50.00  
#  NA's   :25      NA's   :25      NA's   :25                                                  NA's   :11      NA's   :27     

#  income    employed   household      state    DCE_mono    duration      
#  1   : 15   1   :116   1   : 60   4      :86   0: 22    Min.   :  293.0  
#  2   : 69   2   :187   2   :241   6      :80   1:288    1st Qu.:  846.2  
#  3   :117   NA's:  7   4   :  1   5      :67            Median : 1060.5  
#  4   : 57              NA's:  8   3      :33            Mean   : 1464.2  
#  5   : 20                         1      :24            3rd Qu.: 1431.2  
#  6   : 25                         (Other):13            Max.   :20424.0  
#  NA's:  7                         NA's   : 7                             



summary(data_inspect_DCE)
# #                                 ID       DCE_version      task     choice_forced choice_tx   choice_best   goal1    
# 00528be0-0b09-4a3f-24ac-bc796b309825:   8   1:830       1      :310   1:1157        0   : 188   0   : 188   30min:845  
# 01394623-b527-bfb2-486f-711230d5bfe3:   8   2:736       2      :310   2:1313        1   :2246   1   :1066   60min:778  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:   8   3:904       3      :309                 NA's:  36   2   :1180   90min:847  
# 028a7395-1463-aeb0-fbd9-44a167323f51:   8               4      :309                             NA's:  36              
# 046f49fe-186f-e943-1eb2-7e44c9ce4236:   8               5      :308                                                    
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:   8               6      :308                                                    
# (Other)                             :2422               (Other):616                                                    
# form1      mag1      dir1        goal2         form2      mag2      dir2      goal3     form3     mag3      dir3     
# cash   :814   160:835   neg:1236   30min:821   cash   :835   160:821   neg:1234   SQ:2470   SQ:2470   SQ:2470   SQ:2470  
# donate :846   300:800   pos:1234   60min:848   donate :803   300:858   pos:1236                                          
# voucher:810   500:835              90min:801   voucher:832   500:791                                                     

summary(data_inspect_full)


# ID                                          DCE_version      task     choice_forced choice_tx   choice_best   goal1    
# 00528be0-0b09-4a3f-24ac-bc796b309825:   8   1:830       1      :310   1:1157        0   : 188   0   : 188   30min:845  
# 01394623-b527-bfb2-486f-711230d5bfe3:   8   2:736       2      :310   2:1313        1   :2246   1   :1066   60min:778  
# 01aa1bbe-843c-c35c-dcf2-2c816964ce6f:   8   3:904       3      :309                 NA's:  36   2   :1180   90min:847  
# 028a7395-1463-aeb0-fbd9-44a167323f51:   8               4      :309                             NA's:  36              
# 046f49fe-186f-e943-1eb2-7e44c9ce4236:   8               5      :308                                                    
# 04b04ad4-f56c-72b6-c698-7d9b2a546f11:   8               6      :308                                                    
# (Other)                             :2422               (Other):616                                                    

# form1         mag1      dir1        goal2         form2      mag2      dir2      goal3     form3     mag3      dir3     
# cash   :814   160:835   neg:1236   30min:821   cash   :835   160:821   neg:1234   SQ:2470   SQ:2470   SQ:2470   SQ:2470  
# donate :846   300:800   pos:1234   60min:848   donate :803   300:858   pos:1236                                          
# voucher:810   500:835              90min:801   voucher:832   500:791                                                     
# 
# aversion_version   risk_score       loss_score       identified     amotivation       intrinsic       integrated   
# 1:1232           Min.   : 0.000   Min.   : 0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.000   Min.   :0.000  
# 2:1238           1st Qu.: 5.000   1st Qu.: 1.000   1st Qu.:2.250   1st Qu.:0.0000   1st Qu.:1.500   1st Qu.:1.500  
# Median :11.000   Median : 1.000   Median :3.000   Median :0.0000   Median :2.500   Median :2.250  
# Mean   : 8.256   Mean   : 3.453   Mean   :2.859   Mean   :0.5448   Mean   :2.352   Mean   :2.341  
# 3rd Qu.:12.000   3rd Qu.: 5.000   3rd Qu.:3.500   3rd Qu.:1.0000   3rd Qu.:3.250   3rd Qu.:3.250  
# Max.   :12.000   Max.   :12.000   Max.   :4.000   Max.   :4.0000   Max.   :4.000   Max.   :4.000  
# NA's   :14      NA's   :14       NA's   :14      NA's   :14     

# extrinsic      joint_study   joint_number   joint_multi NICE_diagnosis    pain_NRS       function_NRS    hipL     hipR    
# Min.   :0.000   4      :560   Min.   :0.000   0: 798      0   :1048      Min.   : 1.000   Min.   : 0.000   0:1758   0:1638  
# 1st Qu.:0.000   3      :416   1st Qu.:1.000   1:1672      1   :1384      1st Qu.: 4.000   1st Qu.: 3.000   1: 712   1: 832  
# Median :1.000   2      :408   Median :2.000               NA's:  38      Median : 5.000   Median : 5.000                    
# Mean   :1.061   1      :312   Mean   :2.413                              Mean   : 5.276   Mean   : 5.234                    
# 3rd Qu.:1.750   8      :216   3rd Qu.:3.000                              3rd Qu.: 7.000   3rd Qu.: 7.000                    
# Max.   :4.000   (Other):464   Max.   :8.000                              Max.   :10.000   Max.   :10.000                    
# NA's   :14      NA's   : 94                                              NA's   :38       NA's   :38                        

# ankleL   ankleR   kneeL    kneeR    footL    footR    PA_typical     VigDays      TotalVigMin_trunc    ModDays     
# 0:2046   0:1990   0:1454   0:1222   0:1870   0:1822   1   :1816   Min.   :1.000   Min.   : 1.00     Min.   :1.000  
# 1: 424   1: 480   1:1016   1:1248   1: 600   1: 648   2   : 192   1st Qu.:1.000   1st Qu.: 1.00     1st Qu.:1.000  
# 3   : 264   Median :3.000   Median : 8.00     Median :3.000  
# NA's: 198   Mean   :3.179   Mean   :11.02     Mean   :3.298  
# 3rd Qu.:5.000   3rd Qu.:19.00     3rd Qu.:5.000  
# Max.   :8.000   Max.   :40.00     Max.   :8.000  
# NA's   :190     NA's   :190       NA's   :190    

# TotalModMin_trunc    WalkDays     TotalWalkMin_trunc TotalPAMin_trunc    METs_Vig        METs_Mod       METs_Walk    
# Min.   : 1.00     Min.   :1.000   Min.   : 1.00      Min.   :  1.00   Min.   : 1.00   Min.   : 1.00   Min.   : 1.00  
# 1st Qu.: 1.00     1st Qu.:4.000   1st Qu.:12.00      1st Qu.: 23.00   1st Qu.: 1.00   1st Qu.: 1.00   1st Qu.:12.00  
# Median : 8.00     Median :6.000   Median :22.00      Median : 45.00   Median : 8.00   Median : 8.00   Median :22.00  
# Mean   :11.17     Mean   :5.982   Mean   :22.73      Mean   : 53.47   Mean   :11.02   Mean   :11.17   Mean   :22.73  
# 3rd Qu.:19.00     3rd Qu.:8.000   3rd Qu.:34.00      3rd Qu.: 86.00   3rd Qu.:19.00   3rd Qu.:19.00   3rd Qu.:34.00  
# Max.   :39.00     Max.   :8.000   Max.   :49.00      Max.   :126.00   Max.   :40.00   Max.   :39.00   Max.   :49.00  
# NA's   :190       NA's   :190     NA's   :190        NA's   :190      NA's   :190     NA's   :190     NA's   :190    

# METs_Total    FiveDays    SevenDays   IPAQ_cat     gender          age           BMI_calc      income    employed   
# Min.   :  1.0   0   : 336   0   : 648   1   : 368   1   :1440   Min.   :44.00   Min.   :16.00   1   :120   1   : 928  
# 1st Qu.: 33.0   1   :1944   1   :1632   2   : 744   2   : 984   1st Qu.:52.00   1st Qu.:25.00   2   :552   2   :1496  
# Median : 90.0   NA's: 190   NA's: 190   3   :1040   NA's:  46   Median :61.00   Median :28.00   3   :936   NA's:  46  
# Mean   : 95.9                           NA's: 318               Mean   :60.15   Mean   :28.99   4   :456              
# 3rd Qu.:153.0                                                   3rd Qu.:67.00   3rd Qu.:32.00   5   :160              
# Max.   :221.0                                                   Max.   :86.00   Max.   :50.00   6   :200              
# NA's   :190                                                     NA's   :78      NA's   :206     NA's: 46              

# household       state     DCE_mono    duration    
# 1   : 480   4      :688   0: 176   Min.   :  293  
# 2   :1928   6      :640   1:2294   1st Qu.:  847  
# 4   :   8   5      :536            Median : 1061  
# NA's:  54   3      :264            Mean   : 1469  
            # 1      :192            3rd Qu.: 1433  
            # (Other):104            Max.   :20424  
            # NA's   : 46              


str(data_inspect_full)


#to do
#look into local v global environmentment [new.env()]to clean up global environment and leave only dataset for analysis

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(tidy_selfreport_03)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "03_tidy_selfreport.txt"))
