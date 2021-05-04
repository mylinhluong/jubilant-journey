#NEXT STEPS
#(describe) What's happening in this data clean?
#?? Do you have a question after DCE to see if they omitted Any attribute when making decisions? 
#What does that variable tell you? [include in future iterations]

#FUTURE
#update to replace rename_if() with rename_with() to reflect new dplyr funs


#create full dataset filtering only with eligibility criteria
#self_report
#self_report_choice
#this script selects specific variables for self-report, and renames variables
#need to get block1/block2/block3 design scenarios into environment
#need to make self_report_wide1 into self_report_long1
#make sure they are joined by trial and alt
#repeat for self_report_wide2/3
#stack the dfs
#figure out if/how to remove the participants with NAs? or if need to deal w/ them in the analysis
#pull up analysis for multinomial logit
#include only relevant rows for analysis
#if cell is empty, list as NA
#deal with eligibility: remove if CintID = na
#deal with eligibility: remove if Q2.1 eligibility age <45 [4= yes, 5= no]
#deal with eligibility: remove if Q2.2 eligibility pain != Yes [1= yes, 4= no]
#deal with eligibility: remove if Q4.3 eligibility reviewed != Yes[1= yes, 3= no]
#rename choice sets, aversion sets, breq-3, physical activity, sociodemographics, OA characteristics, NICE eligibility 
#should end up with a list of n=303

#LIBRARIES
library(here)
library(dplyr)
library(stringr)
library(readr)


#Create a .txt file within the errors folder
clean_names_02 <- file(here("02_scripts","Errors", "02_clean_names.txt"), open = "wt")
sink(clean_names_02, type = "message")

#View(data_raw)
#View(data)

#97fc7aff-45ad-3863-0ab2-4dd73f0c9721
#44423d2b-fbea-7f88-b466-0a69eb4b29f5

#Early screenout: 163d307d-84e7-72e2-cb30-0716bd4656a3
View(data_rename)

View(data_processed)
data_rename<-data_raw%>%
  slice(-1,-2)%>%
  mutate_all(list(~na_if(.,"")))%>%
  select(ID=CintID, NICE_age=Q2.1, NICE_pain=Q2.2, consent=Q4.3,Q5.1:Q10.4_8,Q10.8_1:Q12.8,Q_TotalDuration, EndDate)%>%
  filter(!is.na(ID)) %>% 
  filter(NICE_age==4& NICE_pain==1 & consent==1)%>%
  filter(ID!="97fc7aff-45ad-3863-0ab2-4dd73f0c9721") %>% 
  filter(ID!="44423d2b-fbea-7f88-b466-0a69eb4b29f5") %>% 
  rename_at(vars(starts_with("Q5.")),
            funs(str_replace(.,"Q5.", "DCE1.")))%>%
  rename_at(vars(starts_with("Q6.")),
            funs(str_replace(.,"Q6.", "DCE2.")))%>%
  rename_at(vars(starts_with("Q7.")),
            funs(str_replace(.,"Q7.", "DCE3.")))%>%
  rename_at(vars(starts_with("Q8.")),
            funs(str_replace(.,"Q8.", "averse1.")))%>%
  rename_at(vars(starts_with("Q9.")),
            funs(str_replace(.,"Q9.", "averse2.")))%>%
  select(ID,
         #SENSITIVITY_MONOTONICITY#
         DCE1.mono= DCE1.1, DCE2.mono=DCE2.1, DCE3.mono=DCE3.1,
         #SENSITIVITY_DURATION#
         duration=Q_TotalDuration,   
         #CHOICE SET# 
         DCE1.4:DCE1.27, DCE2.4:DCE2.27, DCE3.4:DCE3.27,
         #RISKAVERSIONv1#
         risk1_1=averse1.2,risk1_2=averse1.3,risk1_3=averse1.4,risk1_4=averse1.5,risk1_5=averse1.6,risk1_6=averse1.7,
         risk1_7=averse1.8,risk1_8=averse1.9,risk1_9=averse1.10,risk1_10=averse1.11,risk1_11=averse1.12,risk1_12=averse1.13,
         #LOSSAVERSIONv1#
         loss1_1=averse1.15,loss1_2=averse1.16,loss1_3=averse1.17,loss1_4=averse1.18,loss1_5=averse1.19,loss1_6=averse1.20,
         loss1_7=averse1.21,loss1_8=averse1.22,loss1_9=averse1.23,loss1_10=averse1.24,loss1_11=averse1.25,loss1_12=averse1.26,
         #RISKAVERSIONv2#
         risk2_1=averse2.2,risk2_2=averse2.3,risk2_3=averse2.4,risk2_4=averse2.5,risk2_5=averse2.6,risk2_6=averse2.7,
         risk2_7=averse2.8,risk2_8=averse2.9,risk2_9=averse2.10,risk2_10=averse2.11,risk2_11=averse2.12,risk2_12=averse2.13,
         #LOSSAVERSIONv2#
         loss2_1=averse2.15,loss2_2=averse2.16,loss2_3=averse2.17,loss2_4=averse2.18,loss2_5=averse2.19,loss2_6=averse2.20,
         loss2_7=averse2.21,loss2_8=averse2.22,loss2_9=averse2.23,loss2_10=averse2.24,loss2_11=averse2.25,loss2_12=averse2.26,
         #MOTIVATION# 
         breq3_id1=Q10.2_1,breq3_id2=Q10.2_7,breq3_id3=Q10.3_5,breq3_id4=Q10.4_3,
         breq3_amov1=Q10.2_2,breq3_amov2=Q10.2_8,breq3_amov3=Q10.3_6,breq3_amov4=Q10.4_4,
         breq3_intrins1=Q10.2_3,breq3_intrins2=Q10.3_1,breq3_intrins3=Q10.3_7,breq3_intrins4=Q10.4_5,
         breq3_intro1=Q10.2_4,breq3_intro2=Q10.3_2,breq3_intro3=Q10.3_8,breq3_intro4=Q10.4_6,
         breq3_integ1=Q10.2_5,breq3_integ2=Q10.3_3,breq3_integ3=Q10.4_1,breq3_integ4=Q10.4_7,
         breq3_ext1=Q10.2_6,breq3_ext2=Q10.3_4,breq3_ext3=Q10.4_2,breq3_ext4=Q10.4_8,
         #PHYSICAL ACTIVITY#
         VigDays=Q10.8_1,VigHours=Q10.9_1_TEXT,VigMin=Q10.9_2_TEXT,VigDK=Q10.9_6,
         ModDays=Q10.11_1,ModHours=Q10.12_1_TEXT,ModMin=Q10.12_2_TEXT,ModDK=Q10.12_4,
         WalkDays=Q10.14_1,WalkHours=Q10.15_1_TEXT,WalkMin=Q10.15_2_TEXT,WalkDK=Q10.15_4,
         SitHours=Q10.17_1_TEXT,SitMin=Q10.17_2_TEXT,SitDK=Q10.17_4,
         PA_typical=Q10.18,
         #OA CHARACTERISTICS# 
         joint_study=Q11.3, 
         NICE_age, NICE_pain, NICE_activity=Q11.4, NICE_stiffness=Q11.7, NICE_stiffness_min=Q11.8,
         pain_NRS=Q11.5_1, function_NRS=Q11.6_1, 
         hipL=Q11.2_1, hipR=Q11.2_3, ankleL=Q11.2_5, ankleR=Q11.2_7, kneeL=Q11.2_2, kneeR=Q11.2_4, 
         footL=Q11.2_6, footR=Q11.2_8, 
         #SOCIODEMS 
         DOB=Q12.1, enddate=EndDate, gender=Q12.2, height=Q12.3, weight=Q12.4, 
         employed=Q12.5, income=Q12.6, household=Q12.7, state= Q12.8)	
                             
#Save object to an rds file to preserve column data types
saveRDS(data_rename,"01_data/02_processed/data_rename.rds")

#Write to CSV file
write.csv(data_rename,"01_data/02_processed/data_rename.csv", row.names=FALSE)


#end of script
#close the error message catching script and save the file
sink(type = "message")
close(clean_names_02)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "02_clean_names.txt"))

#RESOURCES
#renaming multiple vars @once https://honingds.com/blog/dplyr-rename/