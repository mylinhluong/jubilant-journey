######OSTEOARTHRITIS CHARACTERISTICS######
#####NICE Diagnosis#####
#Participants were coded with a National Institute for Health and Care Excellence (NICE) clinical classification consistent with a diagnosis of knee OA (National Institute for Health and Clinical Excellence, 2014) 
# if he/she/they were age ≥ 45 years; joint pain ≥ three months; joint pain with everyday movements/activity; and morning joint stiffness < 30 minutes.

# NICE_age=age ≥ 45 years: recode 4 as 1
# NICE_pain=joint pain ≥ three months
# NICE_activity=joint pain with everyday movements/activity | 1= Yes, 4= No |recode >> 1= Yes, 0 = No
# NICE_stiffness= morning joint stiffness |1 = Yes, 3= No | recode>> 1= Yes, 0= No
# NICE_stiffness_min= < 30 minutes | 4= <= 30 min, 5 = >30 min | recode >> 0 = 0 min, 1= <=30 min, 3= >30min

# added variable for number of joints with joint pain
# added variable for multiple joints Yes/No

data_OA_chars<-data%>%
  select(ID, joint_study,
         NICE_age, NICE_pain, NICE_activity, NICE_stiffness, NICE_stiffness_min, 
         pain_NRS, function_NRS,
         hipL, hipR, ankleL, ankleR, kneeL, kneeR, footL, footR)%>%
  mutate(across(hipL:footR, ~ifelse(is.na(.),0,.))) %>%
  mutate(joint_number=rowSums(across(hipL:footR))) %>% 
  mutate(joint_multi=case_when(joint_number>1~1,
                               TRUE ~ 0)) %>% 
  mutate(NICE_age=case_when(NICE_age==4~1)) %>% 
  mutate(NICE_activity=case_when(NICE_activity==1~1, 
                                 NICE_activity==4~0,
                                 TRUE ~ NA_real_)) %>% 
  mutate(NICE_stiffness=case_when(NICE_stiffness==1~1,
                                  NICE_stiffness==3~0,
                                  TRUE ~ NA_real_)) %>% 
  mutate(NICE_stiffness_min=case_when(NICE_stiffness_min==4~1,
                                      NICE_stiffness_min==5~2,
                                      TRUE ~ 0)) %>% 
  mutate(NICE_diagnosis= case_when(
    NICE_age==1 & NICE_activity==1 & (NICE_stiffness==1 & NICE_stiffness_min==1 |NICE_stiffness==0) ~1, 
    NICE_age !=1 | NICE_activity !=1|NICE_stiffness_min==2 ~0, 
    TRUE ~ NA_real_)) %>% 
  select(ID, joint_study, joint_number, joint_multi, NICE_diagnosis, pain_NRS, function_NRS, hipL:footR)