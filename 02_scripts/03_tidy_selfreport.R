#NEXT STEPS
#look into local v global environmentment [new.env()]to clean up global environment and leave only dataset for analysis
#This script sources several scripts to creates a new DF that includes tidy data for non-choice set data:  
  # monotonicity (for sensitivity analysis)
  # IPAQ-SF **in progress**
  # the BREQ-3, **in progress: draft complete**
  # aversion (loss and risk) 
  # OA characteristics (incl NICE OA criteria, pain NRS and function NRS)
  # sociodemographics


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

#Create a .txt file within the errors folder
tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_selfreport.txt"), open = "wt")
sink(tidy_selfreport_03, type = "message")

#import renamed dataset into environment
data<- read_csv(here("01_data","02_processed", "data_rename.csv"))

rm(data_BREQ)
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
data_processed<-data_aversion %>% 
  left_join(data_BREQ, by="ID") %>% 
  left_join(data_OA_chars, by="ID") %>% 
  left_join(data_sociodems, by="ID") %>% 
  left_join(data_sensitivity) %>% 
  mutate(across(c(ID, aversion_version, joint_study, joint_multi,
                  NICE_diagnosis, hipL:gender, income: DCE_mono),as.factor))


# for final dataset: add 
#duration sensitivity
filter(!is.na(mono)) %>%
  



#Save object to an rds file to preserve column data types
#saveRDS(self_report_processed,"01_data/02_processed/data_processed.rds")

#Write to CSV file
#write.csv(self_report_processed,"01_data/02_processed/data_processed.csv", row.names=FALSE)

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(tidy_selfreport_03)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "03_tidy_selfreport.txt"))




	mutate(BLOCK2_DCE=recode(BLOCK2_DCE,'1'='2'))%>%
  mutate(BLOCK3_DCE=recode(BLOCK3_DCE,'1'='3'))%>%
  mutate(BLOCK1_DCE=as.character(as.factor(BLOCK1_DCE)))%>%
  mutate(BLOCK2_DCE=as.character(as.factor(BLOCK2_DCE)))%>%
  mutate(BLOCK3_DCE=as.character(as.factor(BLOCK3_DCE)))%>%
  mutate(DCE_block=coalesce(BLOCK1_DCE,BLOCK2_DCE ,BLOCK3_DCE))%>%
  mutate(DCE_block=as.numeric(as.character(DCE_block)))%>%
  mutate(BLOCK2_Averse=recode(BLOCK2_Averse,'1'='2'))%>%
  mutate(BLOCK1_Averse=as.character(as.factor(BLOCK1_Averse)))%>%
  mutate(BLOCK2_Averse=as.character(as.factor(BLOCK2_Averse)))%>%
  mutate(Averse_block=coalesce(BLOCK1_Averse,BLOCK2_Averse))



Data Dictionary 
"breq3_id1","breq3_id2","breq3_id3","breq3_id4"
"breq3_amov1","breq3_amov2","breq3_amov3","breq3_amov4"
"breq3_intrins1","breq3_intrins2","breq3_intrins3","breq3_intrins4"
"breq3_intro1","breq3_intro2","breq3_intro3","breq3_intro4"
"breq3_integ1","breq3_integ2","breq3_integ3","breq3_integ4"
"breq3_ext1","breq3_ext2","breq3_ext3","breq3_ext4"
sociodems_gender
sociodems_weight
sociodems_height

VigDays: Number of days doing vigorous physical activity per week
VigHours: Number of hours in vigorous physical activity per day
VigMin: Number of minutes in vigorous physical activity per day
ModDays: Number of days doing moderate physical activity per week
ModHours: Number of hours in moderate physical activity per day
ModMin: Number of minutes in moderate physical activity per day
WalkDays: Number of days walking per week
WalkHours: Number of hours walking per day
WalkMin: Number of minutes in walking per day
SitHours: Number of hours sitting per day
SitMin: Number of minutes sitting per day


#group=unique participant ID
#sociodems_gender: 1=F, 2=M
#sociodems_weight=in kg
#sociodems_height=in cm

##FOR BREQ: (group=c("group"),
                      identified = c("breq3_id1","breq3_id2","breq3_id3","breq3_id4"),
                      amotivation = c("breq3_amov1","breq3_amov2","breq3_amov3","breq3_amov4"),
                      intrinsic = c("breq3_intrins1","breq3_intrins2","breq3_intrins3","breq3_intrins4"),
                      introjected = c("breq3_intro1","breq3_intro2","breq3_intro3","breq3_intro4"),
                      integrated = c("breq3_integ1","breq3_integ2","breq3_integ3","breq3_integ4"),
                      extrinsic = c("breq3_ext1","breq3_ext2","breq3_ext3","breq3_ext4"),

#ipaqdata
#Column 1: ID
#Column 2: Weight
#Column 3: VigDays: Number of days doing vigorous physical activity per week
#Column 4: VigHours: Number of hours in vigorous physical activity per day
#Column 5: VigMin: Number of minutes in vigorous physical activity per day
#Column 6: ModDays: Number of days doing moderate physical activity per week
#Column 7: ModHours: Number of hours in moderate physical activity per day
#Column 8: ModMin: Number of minutes in moderate physical activity per day
#Column 9: WalkDays: Number of days walking per week
#Column 10: WalkHours: Number of hours walking per day
#Column 11: WalkMin: Number of minutes in walking per day
#Column 12: SitHours: Number of hours sitting per day
#Column 13: SitMin: Number of minutes sitting per day




#####SOCIODEMOGRAPHICS
#Data cleaning for height, weight and BMI
#Used data editing/cleaning from https://melbourneinstitute.unimelb.edu.au/assets/documents/hilda-bibliography/hilda-technical-papers/htec108.pdf
#inspected for cases that met one of the following criteria:
#• height less than 120cms or more than 210 cms;
#• weight less than 40 kgs or more than 200 kgs; or
#• a height and weight combination that led to a BMI of less than 15 or more than 50. 
#extremely unlikely values replaced with NA_integer_. 
#Cutoff points:
#       Men             Women
#    L       U         L         U
#H  130cms  229cms   110cms    210cms
#W  35kgs   300kgs    25kgs   300kgs

#group=unique participant ID
#sociodems_gender: 1=F, 2=M
#sociodems_weight=in kg
#sociodems_height=in cm

#check on values
#sociodems_BMI_m<-self_report_baseline%>%
#  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>%
#  filter(sociodems_gender==2)%>%
#  arrange(desc(sociodems_height))%>%
#  #60 773094008                2              116             98.0
#  #61 835779665                2               16             75.0
#  arrange(desc(sociodems_weight))
#  #61       240                2              176             10.0

#sociodems_BMI_f<-self_report_baseline%>%
#  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>%
#  filter(sociodems_gender ==1)%>%
#  arrange(desc(sociodems_height))%>%
#  #157  19665804                1            71.00             65.0
#  #158 970289459                1             5.60             77.0
#  arrange(desc(sociodems_weight))%>%
  

#HEIGHT AND WEIGHT DATA CLEANING & BMI CALC 
#select cols, only if gender is reported
sociodems_BMI<-self_report_baseline %>% 
  select(group, sociodems_gender, sociodems_height,sociodems_weight)%>% 
  filter(!is.na(sociodems_gender))

#parameters in for loop
num_row = 1

#screen the input data table line by line. Add the row to the tidy table if the line meets the criteria
for (r in 1:nrow(sociodems_BMI)) {
  print(r)
  
  #1st layer: gender-males
  if (sociodems_BMI[r,'sociodems_gender'] == 2){
    #2nd layer: height and weight  
    if (!(sociodems_BMI[r,'sociodems_height']%in% 130:229)){
      sociodems_BMI[r,'sociodems_height']= NA_integer_
    }
    if (!(sociodems_BMI[r,'sociodems_weight']%in% 35:300)){
      sociodems_BMI[r,'sociodems_weight']= NA_integer_}
  }
  
  #1st layer: gender-females
  if (sociodems_BMI[r,'sociodems_gender'] == 1){
    #2nd layer: height and weight
    if (!(sociodems_BMI[r,'sociodems_height']%in% 110:210)){
      sociodems_BMI[r,'sociodems_height']= NA_integer_
    }
    if (!(sociodems_BMI[r,'sociodems_weight']%in% 25:300)){
      sociodems_BMI[r,'sociodems_weight']= NA_integer_}
  }
  
  
  num_row = num_row +1
} 

#add col of BMI_calc
sociodems_BMI<-sociodems_BMI%>%
  mutate(BMI_calc=(sociodems_weight/sociodems_height/sociodems_height)*10000)%>% 
  select(group=group, gender=sociodems_gender, weight=sociodems_weight,height=sociodems_height, BMI_calc=BMI_calc)

#View(sociodems_BMI)


#DEALING WITH REMAINING SOCIODEMOGRAPHICS + MERGING WITH CLEANED HEIGHT, WEIGHT & BMI DATA
#*convert year of birth to age in years
#*converting str of data, variables of income, education level, state and post code to factors
#*rename variables
#*merge sociodemographic datasets
#*replace extremely unlikely values of height and weight combination that led to a BMI of 
#*...less than 15 or more than 50 with NA_integer_.
sociodems<-self_report_baseline%>%
  select(group, age_year, sociodems_income1:sociodems_post)%>%
  arrange(desc(age_year))%>%
  mutate(age_year=sub("5/5/1905","1905",age_year))%>% #is there really somebody who is 114?
  mutate(age_year=as.numeric(age_year))%>%
  mutate(age=2019-age_year)%>% #converting year of both to age, study ended in 2019
  mutate_at(vars(sociodems_loc, 
                 sociodems_post),
            list(factor)) %>% 
  select(group, 
         age, 
         income=sociodems_income1, 
         educ_lvl=sociodems_educ, 
         educ_years=sociodems_educ2, 
         state=sociodems_loc, 
         pcd=sociodems_post)%>% 
  left_join(sociodems_BMI,by="group")%>% 
  arrange(desc(BMI_calc))%>%
  #       group age income educ_lvl educ_years state  pcd gender weight height BMI_calc
  #1  844956205  59      3        3       12.0     6 3803      1  150.0 164.00 55.77037
  #2  301739772  52      2        3       12.0     5 2580      1   99.0 135.00 54.32099
  mutate(BMI_calc=ifelse(BMI_calc>50,NA_integer_,BMI_calc))%>%
  mutate(BMI_calc=ifelse(BMI_calc<15,NA_integer_,BMI_calc))



	mutate(BLOCK2_DCE=recode(BLOCK2_DCE,'1'='2'))%>%
  mutate(BLOCK3_DCE=recode(BLOCK3_DCE,'1'='3'))%>%
  mutate(BLOCK1_DCE=as.character(as.factor(BLOCK1_DCE)))%>%
  mutate(BLOCK2_DCE=as.character(as.factor(BLOCK2_DCE)))%>%
  mutate(BLOCK3_DCE=as.character(as.factor(BLOCK3_DCE)))%>%
  mutate(DCE_block=coalesce(BLOCK1_DCE,BLOCK2_DCE ,BLOCK3_DCE))%>%
  mutate(DCE_block=as.numeric(as.character(DCE_block)))%>%
  mutate(BLOCK2_Averse=recode(BLOCK2_Averse,'1'='2'))%>%
  mutate(BLOCK1_Averse=as.character(as.factor(BLOCK1_Averse)))%>%
  mutate(BLOCK2_Averse=as.character(as.factor(BLOCK2_Averse)))%>%
  mutate(Averse_block=coalesce(BLOCK1_Averse,BLOCK2_Averse))
  
  #%>%mutate_at(vars(ends_with(block)=as.numeric))

##FOR BREQ: (group=c("group"),
                      identified = c("breq3_id1","breq3_id2","breq3_id3","breq3_id4"),
                      amotivation = c("breq3_amov1","breq3_amov2","breq3_amov3","breq3_amov4"),
                      intrinsic = c("breq3_intrins1","breq3_intrins2","breq3_intrins3","breq3_intrins4"),
                      introjected = c("breq3_intro1","breq3_intro2","breq3_intro3","breq3_intro4"),
                      integrated = c("breq3_integ1","breq3_integ2","breq3_integ3","breq3_integ4"),
                      extrinsic = c("breq3_ext1","breq3_ext2","breq3_ext3","breq3_ext4"),


###Creating program selection variables for SR wide
self_report_wide1<-self_report%>%
  filter(BLOCK1_DCE==1)%>%
  select(-(DCE2.1:DCE2.27), -(DCE3.1:DCE3.27), -BLOCK2_DCE,-BLOCK3_DCE)%>%
  mutate(Dominant_1=case_when(DCE1.2==1~1,is.na(DCE1.2)~0))%>%
  mutate(Dominant_2=case_when(DCE1.3==1~1,is.na(DCE1.3)~0))%>%
  mutate(Dominant_3=case_when(DCE1.2!=2 |DCE1.3!=2~0))%>%
  mutate(Selection_1_1=case_when(DCE1.5==1~1,DCE1.5==2~0,is.na(DCE1.5)~0))%>%
  mutate(Selection_1_2=case_when(DCE1.6==1~1,DCE1.6==2~0,is.na(DCE1.6)~0))%>%
  mutate(Selection_1_3=case_when((DCE1.5==2 |DCE1.6==2~1),(DCE1.5!=2|DCE1.6!=2~0)))%>%
  mutate(Selection_2_1=case_when(DCE1.8==1~1,DCE1.8==2~0,is.na(DCE1.8)~0))%>%
  mutate(Selection_2_2=case_when(DCE1.9==1~1,DCE1.9==2~0,is.na(DCE1.9)~0))%>%
  mutate(Selection_2_3=case_when((DCE1.8==2 |DCE1.9==2~1),(DCE1.8!=2|DCE1.9!=2~0)))%>%
  mutate(Selection_3_1=case_when(DCE1.11==1~1,DCE1.11==2~0,is.na(DCE1.11)~0))%>%
  mutate(Selection_3_2=case_when(DCE1.12==1~1,DCE1.12==2~0,is.na(DCE1.12)~0))%>%
  mutate(Selection_3_3=case_when((DCE1.11==2 |DCE1.12==2~1),(DCE1.11!=2|DCE1.12!=2~0)))%>%
  mutate(Selection_4_1=case_when(DCE1.14==1~1,DCE1.14==2~0,is.na(DCE1.14)~0))%>%
  mutate(Selection_4_2=case_when(DCE1.15==1~1,DCE1.15==2~0,is.na(DCE1.15)~0))%>%
  mutate(Selection_4_3=case_when((DCE1.14==2 |DCE1.15==2~1),(DCE1.14!=2|DCE1.15!=2~0)))%>%
  mutate(Selection_5_1=case_when(DCE1.17==1~1,DCE1.17==2~0,is.na(DCE1.17)~0))%>%
  mutate(Selection_5_2=case_when(DCE1.18==1~1,DCE1.18==2~0,is.na(DCE1.18)~0))%>%
  mutate(Selection_5_3=case_when((DCE1.17==2 |DCE1.18==2~1),(DCE1.17!=2|DCE1.18!=2~0)))%>%
  mutate(Selection_6_1=case_when(DCE1.20==1~1,DCE1.20==2~0,is.na(DCE1.20)~0))%>%
  mutate(Selection_6_2=case_when(DCE1.21==1~1,DCE1.21==2~0,is.na(DCE1.21)~0))%>%
  mutate(Selection_6_3=case_when((DCE1.20==2 |DCE1.21==2~1),(DCE1.20!=2|DCE1.21!=2~0)))%>%
  mutate(Selection_7_1=case_when(DCE1.23==1~1,DCE1.23==2~0,is.na(DCE1.23)~0))%>%
  mutate(Selection_7_2=case_when(DCE1.24==1~1,DCE1.24==2~0,is.na(DCE1.24)~0))%>%
  mutate(Selection_7_3=case_when((DCE1.23==2 |DCE1.24==2~1),(DCE1.23!=2|DCE1.24!=2~0)))%>%
  mutate(Selection_8_1=case_when(DCE1.26==1~1,DCE1.26==2~0,is.na(DCE1.26)~0))%>%
  mutate(Selection_8_2=case_when(DCE1.27==1~1,DCE1.27==2~0,is.na(DCE1.27)~0))%>%
  mutate(Selection_8_3=case_when((DCE1.26==2 |DCE1.27==2~1),(DCE1.26!=2|DCE1.27!=2~0)))%>%
  mutate(BLOCK1_DCE=as.numeric(as.character(BLOCK1_DCE)))%>%
  select(Participant=ResponseId,Selection_1_1:Selection_8_3, Version=BLOCK1_DCE)

#View(self_report_wide1)
#str(self_report_wide1)
#colnames(self_report_wide1, Dominant_1:Selection_8_3, names_to=")

self_report_wide2<-self_report%>%
  filter(BLOCK2_DCE==2)%>%
  select(-(DCE1.1:DCE1.27), -(DCE3.1:DCE3.27), -BLOCK1_DCE,-BLOCK3_DCE)%>%
  mutate(Dominant_1=case_when(DCE2.2==1~1,is.na(DCE2.2)~0))%>%
  mutate(Dominant_2=case_when(DCE2.3==1~1,is.na(DCE2.3)~0))%>%
  mutate(Dominant_3=case_when(DCE2.2!=2 |DCE2.3!=2~0))%>%
  mutate(Selection_1_1=case_when(DCE2.5==1~1,DCE2.5==2~0,is.na(DCE2.5)~0))%>%
  mutate(Selection_1_2=case_when(DCE2.6==1~1,DCE2.6==2~0,is.na(DCE2.6)~0))%>%
  mutate(Selection_1_3=case_when((DCE2.5==2 |DCE2.6==2~1),(DCE2.5!=2|DCE2.6!=2~0)))%>%
  mutate(Selection_2_1=case_when(DCE2.8==1~1,DCE2.8==2~0,is.na(DCE2.8)~0))%>%
  mutate(Selection_2_2=case_when(DCE2.9==1~1,DCE2.9==2~0,is.na(DCE2.9)~0))%>%
  mutate(Selection_2_3=case_when((DCE2.8==2 |DCE2.9==2~1),(DCE2.8!=2|DCE2.9!=2~0)))%>%
  mutate(Selection_3_1=case_when(DCE2.11==1~1,DCE2.11==2~0,is.na(DCE2.11)~0))%>%
  mutate(Selection_3_2=case_when(DCE2.12==1~1,DCE2.12==2~0,is.na(DCE2.12)~0))%>%
  mutate(Selection_3_3=case_when((DCE2.11==2 |DCE2.12==2~1),(DCE2.11!=2|DCE2.12!=2~0)))%>%
  mutate(Selection_4_1=case_when(DCE2.14==1~1,DCE2.14==2~0,is.na(DCE2.14)~0))%>%
  mutate(Selection_4_2=case_when(DCE2.15==1~1,DCE2.15==2~0,is.na(DCE2.15)~0))%>%
  mutate(Selection_4_3=case_when((DCE2.14==2 |DCE2.15==2~1),(DCE2.14!=2|DCE2.15!=2~0)))%>%
  mutate(Selection_5_1=case_when(DCE2.17==1~1,DCE2.17==2~0,is.na(DCE2.17)~0))%>%
  mutate(Selection_5_2=case_when(DCE2.18==1~1,DCE2.18==2~0,is.na(DCE2.18)~0))%>%
  mutate(Selection_5_3=case_when((DCE2.17==2 |DCE2.18==2~1),(DCE2.17!=2|DCE2.18!=2~0)))%>%
  mutate(Selection_6_1=case_when(DCE2.20==1~1,DCE2.20==2~0,is.na(DCE2.20)~0))%>%
  mutate(Selection_6_2=case_when(DCE2.21==1~1,DCE2.21==2~0,is.na(DCE2.21)~0))%>%
  mutate(Selection_6_3=case_when((DCE2.20==2 |DCE2.21==2~1),(DCE2.20!=2|DCE2.21!=2~0)))%>%
  mutate(Selection_7_1=case_when(DCE2.23==1~1,DCE2.23==2~0,is.na(DCE2.23)~0))%>%
  mutate(Selection_7_2=case_when(DCE2.24==1~1,DCE2.24==2~0,is.na(DCE2.24)~0))%>%
  mutate(Selection_7_3=case_when((DCE2.23==2 |DCE2.24==2~1),(DCE2.23!=2|DCE2.24!=2~0)))%>%
  mutate(Selection_8_1=case_when(DCE2.26==1~1,DCE2.26==2~0,is.na(DCE2.26)~0))%>%
  mutate(Selection_8_2=case_when(DCE2.27==1~1,DCE2.27==2~0,is.na(DCE2.27)~0))%>%
  mutate(Selection_8_3=case_when((DCE2.26==2 |DCE2.27==2~1),(DCE2.26!=2|DCE2.27!=2~0)))%>%
  mutate(BLOCK2_DCE=as.numeric(as.character(BLOCK2_DCE)))%>%
  select(Participant=ResponseId,Selection_1_1:Selection_8_3, Version=BLOCK2_DCE)

#View(self_report_wide2)

self_report_wide3<-self_report%>%
  filter(BLOCK3_DCE==3)%>%
  select(-(DCE1.1:DCE1.27), -(DCE2.1:DCE2.27), -BLOCK1_DCE,-BLOCK2_DCE)%>%
  mutate(Dominant_1=case_when(DCE3.2==1~1,is.na(DCE3.2)~0))%>%
  mutate(Dominant_2=case_when(DCE3.3==1~1,is.na(DCE3.3)~0))%>%
  mutate(Dominant_3=case_when(DCE3.2!=2 |DCE3.3!=2~0))%>%
  mutate(Selection_1_1=case_when(DCE3.5==1~1,DCE3.5==2~0,is.na(DCE3.5)~0))%>%
  mutate(Selection_1_2=case_when(DCE3.6==1~1,DCE3.6==2~0,is.na(DCE3.6)~0))%>%
  mutate(Selection_1_3=case_when((DCE3.5==2 |DCE3.6==2~1),(DCE3.5!=2|DCE3.6!=2~0)))%>%
  mutate(Selection_2_1=case_when(DCE3.8==1~1,DCE3.8==2~0,is.na(DCE3.8)~0))%>%
  mutate(Selection_2_2=case_when(DCE3.9==1~1,DCE3.9==2~0,is.na(DCE3.9)~0))%>%
  mutate(Selection_2_3=case_when((DCE3.8==2 |DCE3.9==2~1),(DCE3.8!=2|DCE3.9!=2~0)))%>%
  mutate(Selection_3_1=case_when(DCE3.11==1~1,DCE3.11==2~0,is.na(DCE3.11)~0))%>%
  mutate(Selection_3_2=case_when(DCE3.12==1~1,DCE3.12==2~0,is.na(DCE3.12)~0))%>%
  mutate(Selection_3_3=case_when((DCE3.11==2 |DCE3.12==2~1),(DCE3.11!=2|DCE3.12!=2~0)))%>%
  mutate(Selection_4_1=case_when(DCE3.14==1~1,DCE3.14==2~0,is.na(DCE3.14)~0))%>%
  mutate(Selection_4_2=case_when(DCE3.15==1~1,DCE3.15==2~0,is.na(DCE3.15)~0))%>%
  mutate(Selection_4_3=case_when((DCE3.14==2 |DCE3.15==2~1),(DCE3.14!=2|DCE3.15!=2~0)))%>%
  mutate(Selection_5_1=case_when(DCE3.17==1~1,DCE3.17==2~0,is.na(DCE3.17)~0))%>%
  mutate(Selection_5_2=case_when(DCE3.18==1~1,DCE3.18==2~0,is.na(DCE3.18)~0))%>%
  mutate(Selection_5_3=case_when((DCE3.17==2 |DCE3.18==2~1),(DCE3.17!=2|DCE3.18!=2~0)))%>%
  mutate(Selection_6_1=case_when(DCE3.20==1~1,DCE3.20==2~0,is.na(DCE3.20)~0))%>%
  mutate(Selection_6_2=case_when(DCE3.21==1~1,DCE3.21==2~0,is.na(DCE3.21)~0))%>%
  mutate(Selection_6_3=case_when((DCE3.20==2 |DCE3.21==2~1),(DCE3.20!=2|DCE3.21!=2~0)))%>%
  mutate(Selection_7_1=case_when(DCE3.23==1~1,DCE3.23==2~0,is.na(DCE3.23)~0))%>%
  mutate(Selection_7_2=case_when(DCE3.24==1~1,DCE3.24==2~0,is.na(DCE3.24)~0))%>%
  mutate(Selection_7_3=case_when((DCE3.23==2 |DCE3.24==2~1),(DCE3.23!=2|DCE3.24!=2~0)))%>%
  mutate(Selection_8_1=case_when(DCE3.26==1~1,DCE3.26==2~0,is.na(DCE3.26)~0))%>%
  mutate(Selection_8_2=case_when(DCE3.27==1~1,DCE3.27==2~0,is.na(DCE3.27)~0))%>%
  mutate(Selection_8_3=case_when((DCE3.26==2 |DCE3.27==2~1),(DCE3.26!=2|DCE3.27!=2~0)))%>%
  mutate(BLOCK3_DCE=as.numeric(as.character(BLOCK3_DCE)))%>%
  select(Participant=ResponseId,Selection_1_1:Selection_8_3, Version=BLOCK3_DCE)

#View(self_report_wide3)

####PIVOT DATA FROM WIDE TO LONG
#values_drop_na=TRUE will drop rows that contain only NAs in the value_to column?
self_report_long1<-self_report_wide1%>%
  pivot_longer(
    cols=starts_with("Selection_"),
    names_to="Alt",
    values_to="Selection",
    values_drop_na=FALSE)%>%
  mutate(Trial=Alt)%>%
  select(Participant, Selection, Version, Trial, Alt)%>%
  mutate(Trial=str_remove(Trial,"Selection_"))%>%
  mutate(Trial=str_extract(Trial,"^.{1}"))%>%
  mutate(Alt=str_remove(Alt,"Selection_"))%>%
  mutate(Alt=str_sub(Alt,3))

#View(self_report_long1)  

self_report_long2<-self_report_wide2%>%
  pivot_longer(
    cols=starts_with("Selection_"),
    names_to="Alt",
    values_to="Selection",
    values_drop_na=FALSE)%>%
  mutate(Trial=Alt)%>%
  select(Participant, Selection, Version, Trial, Alt)%>%
  mutate(Trial=str_remove(Trial,"Selection_"))%>%
  mutate(Trial=str_extract(Trial,"^.{1}"))%>%
  mutate(Alt=str_remove(Alt,"Selection_"))%>%
  mutate(Alt=str_sub(Alt,3))
#View(self_report_long2)


self_report_long3<-self_report_wide3%>%
  pivot_longer(
    cols=starts_with("Selection_"),
    names_to="Alt",
    values_to="Selection",
    values_drop_na=FALSE)%>%
  mutate(Trial=Alt)%>%
  select(Participant, Selection, Version, Trial, Alt)%>%
  mutate(Trial=str_remove(Trial,"Selection_"))%>%
  mutate(Trial=str_extract(Trial,"^.{1}"))%>%
  mutate(Alt=str_remove(Alt,"Selection_"))%>%
  mutate(Alt=str_sub(Alt,3))

#View(self_report_long3)


####COMBINE DCE w/ choice set
DCE_long1<-merge(self_report_long1, Version1, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_long1)

#write.csv(DCE_long1,"01_data/02_processed/DCE_long1.csv")

DCE_long2<-merge(self_report_long2, Version2, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_long2)

#write.csv(DCE_long2,"01_data/02_processed/DCE_long1.csv")
DCE_long3<-merge(self_report_long3, Version3, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_long3)

#write.csv(DCE_long3,"01_data/02_processed/DCE_long1.csv")

DCE_data_processed<-bind_rows(DCE_long1,DCE_long2,DCE_long3)

#write.csv(DCE_data_processed,"01_data/02_processed/DCE_data_processed.csv", row.names=FALSE)
View(DCE_data_processed)





mutate(Selection_1_1=ifelse(DCE1.2==1,1,0))%>%
  mutate(Selection_1_2=ifelse(DCE1.3==1,1,0))%>%
  mutate(Selection_1_3=ifelse(DCE1.2==2 |DCE1.3==2,1,0)) 

View(self_report_wide1)



str(self_report)


mutate(intention_min=as.numeric(as.character(intention_min, na.rm=FALSE)))%>% #coercing the data into numeric
  mutate(intention_min=replace(intention_min, which(is.na(intention_min)& intention==0),0))%>%
  
  mutate(DCE_block=ifelse(BLOCK1_DCE=1,ifelse(BLOCK2_DCE(ifelse(BLOCK3_DCE)))))
  mutate(BLOCK1_DCE=coalesce(BLOCK1_DCE,BLOCK2_DCE))
  
self_report%>%mutate(BLOCK2_DCE=coalesce(BLOCK2_DCE,BLOCK3_DCE))
  mutate(BLOCK1_DCE=coalesce(BLOCK1_DCE,BLOCK2_DCE))

View(self_report)
  
head(self_report)

str(self_report)
  mutate()
        
colnames(self_report)
#colnames(self_report)
#View(self_report)



#####re-naming variables for IPAQ-Short Form#####
##baseline
IPAQ<-select(self_report_baseline,group=group,
                                  PA_JOB_UNPAID_WRK=ipaq_sl_job.x,
                                  PA_WRK_VIG_FREQ= ipaq_sl_ovday.x, PA_WRK_VIG_TIME_HR=ipaq_sl_ovdhrs.x, PA_WRK_VIG_TIME_MIN=ipaq_sl_ovdmin.x,
                                  PA_WRK_MOD_FREQ=ipaq_sl_omday.x, PA_WRK_MOD_TIME_HR=ipaq_sl_omdhrs.x, PA_WRK_MOD_TIME_MIN=ipaq_sl_omdmin.x,
                                  PA_WRK_WALK_FREQ=ipaq_sl_owday.x, PA_WRK_WALK_TIME_HR=ipaq_sl_owdhrs.x, PA_WRK_WALK_TIME_MIN=ipaq_sl_owdmin.x,
                                  PA_TRANS_FREQ=ipaq_sl_tmday.x, PA_TRANS_TIME_HOUR=ipaq_sl_tmdhrs.x, PA_TRANS_TIME_MIN=ipaq_sl_tmdmin.x,
                                  PA_CYCLING_FREQ=ipaq_sl_tbday.x, PA_CYCLING_TIME_HR=ipaq_sl_tbwhrs.x, PA_CYCLING_TIME_MIN=ipaq_sl_tbwmin.x,
                                  PA_TRANS_WALK_FREQ=ipaq_sl_twday.x, PA_TRANS_WALK_TIME_HR=ipaq_sl_twdhrs.x, PA_TRANS_WALK_TIME_MIN=ipaq_sl_twdmin.x, 
                                  PA_GARDEN_VIG_FREQ=ipaq_sl_gvday.x, PA_GARDEN_VIG_TIME_HR=ipaq_sl_gvdhrs.x, PA_GARDEN_VIG_TIME_MIN=ipaq_sl_gvmin.x,
                                  PA_GARDEN_MOD_FREQ=ipaq_sl_gmday.x, PA_GARDEN_MOD_TIME_HR=ipaq_sl_gmdhrs.x, PA_GARDEN_MOD_TIME_MIN=ipaq_sl_gmdmin.x,
                                  PA_INSIDE_MOD_FREQ=ipaq_sl_hmday.x, PA_INSIDE_MOD_HR=ipaq_sl_hmdhrs.x, PA_INSIDE_MOD_MIN=ipaq_sl_hmdmin.x,
                                  PA_LEISURE_WALK_FREQ=ipaq_sl_lwday.x, PA_LEISURE_WALK_TIME_HR=ipaq_sl_lwdhrs.x, PA_LEISURE_WALK_TIME_MIN=ipaq_sl_lwdmin.x,
                                  PA_LEISURE_VIG_FREQ=ipaq_sl_lvday.x, PA_LEISURE_VIG_TIME_HR=ipaq_sl_lvdhrs.x, PA_LEISURE_VIG_TIME_MIN=ipaq_sl_lvdmin.x, 
                                  PA_LEISURE_MOD_FREQ=ipaq_sl_lmday.x, PA_LEISURE_MOD_TIME_HR=ipaq_sl_lmdhrs.x, PA_LEISURE_MOD_TIME_MIN=ipaq_sl_lmdmin.x) 



#going from gather/spread to pivot_longer, pivot_wider: https://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html
# vignette on pivot https://tidyr.tidyverse.org/reference/pivot_longer.html
#extracting characters from a string: https://stackoverflow.com/questions/38750535/extract-the-first-2-characters-in-a-string
#stringr cheatsheet: http://edrub.in/CheatSheets/cheatSheetStringr.pdf
#??melt

##Resources
#Age calculations 
##://jenrichmond.rbind.io/post/calculating-age/
##Working with dates and time in R using the lubridate package: https://data.library.virginia.edu/working-with-dates-and-time-in-r-using-the-lubridate-package/
##https://stackoverflow.com/questions/41668795/using-mutate-with-dates-gives-numerical-values