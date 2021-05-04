#this script selects specific variables for self-report and renames variables

#todo
#should any of these dfs be saved into processed data? if so, as csv or as rdata?
#colnames(self_report_raw)
#Warning message:In x:y : numerical expression has 6 elements: only the first used

#need to get block1/block2/block3 design scenarios into environment
#need to make self_report_wide1 into self_report_long1
#make sure they are joined by trial and alt
#repeat for self_report_wide2/3
#stack the dfs
#figure out if/how to remove the participants with NAs? or if need to deal w/ them in the analysis
#pull up analysis for multinomial logit

#Resources
#renaming multiple vars @once https://honingds.com/blog/dplyr-rename/
#going from gather/spread to pivot_longer, pivot_wider: https://www.computerworld.com/article/2486425/business-intelligence-4-data-wrangling-tasks-in-r-for-advanced-beginners.html
# vignette on pivot https://tidyr.tidyverse.org/reference/pivot_longer.html
#extracting characters from a string: https://stackoverflow.com/questions/38750535/extract-the-first-2-characters-in-a-string
#stringr cheatsheet: http://edrub.in/CheatSheets/cheatSheetStringr.pdf


#View(self_report)

#####selecting columns & renaming variables for self-report data#####
self_report<-(self_report_raw[-c(1,2),])%>%
  mutate_all(list(~na_if(.,"")))%>%
  select(ResponseId,Duration..in.seconds.,Q2.1,Q2.2,Q4.3,Q5.1:Q5.6:Q10.4_8,Q10.8_1:Q12.8,
         Finished.1:FL_29_DO_SECTION2AND3RiskAversion.LossAversionB2)%>%
  rename_at(vars(starts_with("Q5.")),
            funs(str_replace(.,"Q5.","DCE1.")))%>%
  rename_at(vars(starts_with("Q6.")),
            funs(str_replace(.,"Q6.","DCE2.")))%>%
  rename_at(vars(starts_with("Q7.")),
            funs(str_replace(.,"Q7.","DCE3.")))%>%
  rename_at(vars(starts_with("Q8.")),
            funs(str_replace(.,"Q8.","Averse1.")))%>%
  rename_at(vars(starts_with("Q9.")),
            funs(str_replace(.,"Q9.","Averse2.")))%>%
  rename_at(vars(starts_with("Q10.")),
            funs(str_replace(.,"Q10.2_","BREQ1.")))%>%
  rename_at(vars(starts_with("Q10.")),
            funs(str_replace(.,"Q10.3_","BREQ2.")))%>%
  rename_at(vars(starts_with("Q10.")),
            funs(str_replace(.,"Q10.4_","BREQ3.")))%>%
  rename_at(vars(starts_with("Q10.")),
            funs(str_replace(.,"Q10.","IPAQ.")))%>%
  select(ResponseId, Complete=Finished.1, Complete_time=Duration..in.seconds.,
        NICE_age=Q2.1,NICE_pain3mos=Q2.2, NICE_activity=Q11.4,NICE_stiffnessYN=Q11.7,NICE_stiffness_min=Q11.8,
        Consent=Q4.3,
        BLOCK1_DCE=FL_19_DO_SECTION1.BLOCK1, BLOCK2_DCE=FL_19_DO_SECTION1.BLOCK2,BLOCK3_DCE=FL_19_DO_SECTION1.BLOCK3,
        DCE1.1:DCE3.27,
        BLOCK1_Averse=FL_29_DO_SECTION2AND3RiskAversion.LossAversionB1,BLOCK2_Averse=FL_29_DO_SECTION2AND3RiskAversion.LossAversionB2,
        Averse1.2:IPAQ.18,
        Study_joint=Q11.3,pain_NRS=Q11.5_1, function_NRS=Q11.6_1,
        joint_hipL=Q11.2_1,joint_hipR=Q11.2_3, joint_ankleL=Q11.2_5, joint_ankleR=Q11.2_7,
        joint_kneeL=Q11.2_2, joint_kneeR_=Q11.2_4,joint_footL=Q11.2_6, joint_footR=Q11.2_8,
        DOB=Q12.1,Gender=Q12.2, Height=Q12.3, Weight=Q12.4, Employed=Q12.5, Income=Q12.6,Household=Q12.7,Location=Q12.8)%>%
  filter(NICE_age==4 & NICE_pain3mos==1 & Consent==1 & Complete==1)%>%
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
#View(self_report)



###Creating program selection variables for SR wide (2 alt choices (forced choice) for each randomized block)
self_report_forced_wide1<-self_report%>%
  filter(DCE_block==1)%>%
  select(-(DCE2.1:DCE2.27), -(DCE3.1:DCE3.27), -BLOCK2_DCE,-BLOCK3_DCE)%>%
  mutate(Dominant_1=case_when(DCE1.1==1~1,DCE1.1==2~0))%>%
  mutate(Dominant_2=case_when(DCE1.1==2~1, DCE1.1==1~0))%>%
  mutate(Selection_1_1=case_when(DCE1.4==1~1,DCE1.4==2~0))%>%
  mutate(Selection_1_2=case_when(DCE1.4==2~1, DCE1.4==1~0))%>%
  mutate(Selection_2_1=case_when(DCE1.7==1~1,DCE1.7==2~0))%>%
  mutate(Selection_2_2=case_when(DCE1.7==2~1,DCE1.7==1~0))%>%
  mutate(Selection_3_1=case_when(DCE1.10==1~1,DCE1.10==2~0))%>%
  mutate(Selection_3_2=case_when(DCE1.10==2~1,DCE1.10==1~0))%>%
  mutate(Selection_4_1=case_when(DCE1.13==1~1,DCE1.13==2~0))%>%
  mutate(Selection_4_2=case_when(DCE1.13==2~1,DCE1.13==1~0))%>%
  mutate(Selection_5_1=case_when(DCE1.16==1~1,DCE1.16==2~0))%>%
  mutate(Selection_5_2=case_when(DCE1.16==2~1,DCE1.16==1~0))%>%
  mutate(Selection_6_1=case_when(DCE1.19==1~1,DCE1.19==2~0))%>%
  mutate(Selection_6_2=case_when(DCE1.19==2~1,DCE1.19==1~0))%>%
  mutate(Selection_7_1=case_when(DCE1.22==1~1,DCE1.22==2~0))%>%
  mutate(Selection_7_2=case_when(DCE1.22==2~1,DCE1.22==1~0))%>%
  mutate(Selection_8_1=case_when(DCE1.25==1~1,DCE1.25==2~0))%>%
  mutate(Selection_8_2=case_when(DCE1.25==2~1,DCE1.25==1~0))%>%
  mutate(DCE_block=as.numeric(as.character(DCE_block)))%>%
  select(Participant=ResponseId,Version=DCE_block,Selection_1_1:Selection_8_2)
summar
#View(self_report_forced_wide1)
#str(self_report_wide1)
#colnames(self_report_wide1, Dominant_1:Selection_8_3, names_to=")

self_report_forced_wide2<-self_report%>%
  filter(DCE_block==2)%>%
  select(-(DCE1.1:DCE1.27), -(DCE3.1:DCE3.27), -BLOCK1_DCE,-BLOCK3_DCE)%>%
  mutate(Dominant_1=case_when(DCE2.1==1~1,DCE2.1==2~0))%>%
  mutate(Dominant_2=case_when(DCE2.1==2~1,DCE2.1==1~0))%>%
  mutate(Selection_1_1=case_when(DCE2.4==1~1,DCE2.4==2~0))%>%
  mutate(Selection_1_2=case_when(DCE2.4==2~1, DCE2.4==1~0))%>%
  mutate(Selection_2_1=case_when(DCE2.7==1~1,DCE2.7==2~0))%>%
  mutate(Selection_2_2=case_when(DCE2.7==2~1,DCE2.7==1~0))%>%
  mutate(Selection_3_1=case_when(DCE2.10==1~1,DCE2.10==2~0))%>%
  mutate(Selection_3_2=case_when(DCE2.10==2~1,DCE2.10==1~0))%>%
  mutate(Selection_4_1=case_when(DCE2.13==1~1,DCE2.13==2~0))%>%
  mutate(Selection_4_2=case_when(DCE2.13==2~1,DCE2.13==1~0))%>%
  mutate(Selection_5_1=case_when(DCE2.16==1~1,DCE2.16==2~0))%>%
  mutate(Selection_5_2=case_when(DCE2.16==2~1,DCE2.16==1~0))%>%
  mutate(Selection_6_1=case_when(DCE2.19==1~1,DCE2.19==2~0))%>%
  mutate(Selection_6_2=case_when(DCE2.19==2~1,DCE2.19==1~0))%>%
  mutate(Selection_7_1=case_when(DCE2.22==1~1,DCE2.22==2~0))%>%
  mutate(Selection_7_2=case_when(DCE2.22==2~1,DCE2.22==1~0))%>%
  mutate(Selection_8_1=case_when(DCE2.25==1~1,DCE2.25==2~0))%>%
  mutate(Selection_8_2=case_when(DCE2.25==2~1,DCE2.25==1~0))%>%
  mutate(DCE_block=as.numeric(as.character(DCE_block)))%>%
  select(Participant=ResponseId,Version=DCE_block,Selection_1_1:Selection_8_2)

#View(self_report__forced_wide2)

self_report_forced_wide3<-self_report%>%
  filter(DCE_block==3)%>%
  select(-(DCE1.1:DCE1.27), -(DCE2.1:DCE2.27), -BLOCK1_DCE,-BLOCK2_DCE)%>%
  mutate(Dominant_1=case_when(DCE3.1==1~1,DCE3.1==2~0))%>%
  mutate(Dominant_2=case_when(DCE3.1==2~1,DCE3.1==1~0))%>%
  mutate(Selection_1_1=case_when(DCE3.4==1~1,DCE3.4==2~0))%>%
  mutate(Selection_1_2=case_when(DCE3.4==2~1, DCE3.4==1~0))%>%
  mutate(Selection_2_1=case_when(DCE3.7==1~1,DCE3.7==2~0))%>%
  mutate(Selection_2_2=case_when(DCE3.7==2~1,DCE3.7==1~0))%>%
  mutate(Selection_3_1=case_when(DCE3.10==1~1,DCE3.10==2~0))%>%
  mutate(Selection_3_2=case_when(DCE3.10==2~1,DCE3.10==1~0))%>%
  mutate(Selection_4_1=case_when(DCE3.13==1~1,DCE3.13==2~0))%>%
  mutate(Selection_4_2=case_when(DCE3.13==2~1,DCE3.13==1~0))%>%
  mutate(Selection_5_1=case_when(DCE3.16==1~1,DCE3.16==2~0))%>%
  mutate(Selection_5_2=case_when(DCE3.16==2~1,DCE3.16==1~0))%>%
  mutate(Selection_6_1=case_when(DCE3.19==1~1,DCE3.19==2~0))%>%
  mutate(Selection_6_2=case_when(DCE3.19==2~1,DCE3.19==1~0))%>%
  mutate(Selection_7_1=case_when(DCE3.22==1~1,DCE3.22==2~0))%>%
  mutate(Selection_7_2=case_when(DCE3.22==2~1,DCE3.22==1~0))%>%
  mutate(Selection_8_1=case_when(DCE3.25==1~1,DCE3.25==2~0))%>%
  mutate(Selection_8_2=case_when(DCE3.25==2~1,DCE3.25==1~0))%>%
  mutate(DCE_block=as.numeric(as.character(DCE_block)))%>%
  select(Participant=ResponseId,Version=DCE_block,Selection_1_1:Selection_8_2)

#View(self_report_forced_wide3)

####PIVOT DATA FROM WIDE TO LONG
#values_drop_na=TRUE will drop rows that contain only NAs in the value_to column?
self_report_forced_long1<-self_report_forced_wide1%>%
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

#View(self_report_forced_long1)  

self_report_forced_long2<-self_report_forced_wide2%>%
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
#View(self_report_forced_long2)


self_report_forced_long3<-self_report_forced_wide3%>%
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
DCE_forced_long1<-merge(self_report_forced_long1, Version1, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_forced_long1)

#write.csv(DCE_forced_long1,"01_data/02_processed/DCE_forced_long1.csv")

DCE_forced_long2<-merge(self_report_forced_long2, Version2, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_forced_long2)

#write.csv(DCE_forced_long2,"01_data/02_processed/DCE_forced_long2.csv")

DCE_forced_long3<-merge(self_report_forced_long3, Version3, by=c("Trial","Alt","Version"))%>%
  arrange(Participant)
#View(DCE_long3)

#write.csv(DCE_forced_long3,"01_data/02_processed/DCE_forced_long3.csv")

DCE_forced_data_processed<-bind_rows(DCE_forced_long1,DCE_forced_long2,DCE_forced_long3)

#write.csv(DCE_forced_data_processed,"01_data/02_processed/DCE_forced_data_processed.csv", row.names=FALSE)






#mutate(Selection_1_1=ifelse(DCE1.2==1,1,0))%>%
#  mutate(Selection_1_2=ifelse(DCE1.3==1,1,0))%>%
#  mutate(Selection_1_3=ifelse(DCE1.2==2 |DCE1.3==2,1,0)) 

#View(self_report_wide1)



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



#####re-naming variables for IPAQ#####
##baseline
self_report_baseline_IPAQ<-select(self_report_baseline,group=group,
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

##follow-up
self_report_followup_IPAQ<-select(self_report_followup,group=group,
                                  PA_JOB_UNPAID_WRK=ipaq_sl_job.y,
                                  PA_WRK_VIG_FREQ= ipaq_sl_ovday.y, PA_WRK_VIG_TIME_HR=ipaq_sl_ovdhrs.y, PA_WRK_VIG_TIME_MIN=ipaq_sl_ovdmin.y,
                                  PA_WRK_MOD_FREQ=ipaq_sl_omday.y, PA_WRK_MOD_TIME_HR=ipaq_sl_omdhrs.y, PA_WRK_MOD_TIME_MIN=ipaq_sl_omdmin.y,
                                  PA_WRK_WALK_FREQ=ipaq_sl_owday.y, PA_WRK_WALK_TIME_HR=ipaq_sl_owdhrs.y, PA_WRK_WALK_TIME_MIN=ipaq_sl_owdmin.y,
                                  PA_TRANS_FREQ=ipaq_sl_tmday.y, PA_TRANS_TIME_HOUR=ipaq_sl_tmdhrs.y, PA_TRANS_TIME_MIN=ipaq_sl_tmdmin.y,
                                  PA_CYCLING_FREQ=ipaq_sl_tbday.y, PA_CYCLING_TIME_HR=ipaq_sl_tbwhrs.y, PA_CYCLING_TIME_MIN=ipaq_sl_tbwmin.y,
                                  PA_TRANS_WALK_FREQ=ipaq_sl_twday.y, PA_TRANS_WALK_TIME_HR=ipaq_sl_twdhrs.y, PA_TRANS_WALK_TIME_MIN=ipaq_sl_twdmin.y, 
                                  PA_GARDEN_VIG_FREQ=ipaq_sl_gvday.y, PA_GARDEN_VIG_TIME_HR=ipaq_sl_gvdhrs.y, PA_GARDEN_VIG_TIME_MIN=ipaq_sl_gvmin.y,
                                  PA_GARDEN_MOD_FREQ=ipaq_sl_gmday.y, PA_GARDEN_MOD_TIME_HR=ipaq_sl_gmdhrs.y, PA_GARDEN_MOD_TIME_MIN=ipaq_sl_gmdmin.y,
                                  PA_INSIDE_MOD_FREQ=ipaq_sl_hmday.y, PA_INSIDE_MOD_HR=ipaq_sl_hmdhrs.y, PA_INSIDE_MOD_MIN=ipaq_sl_hmdmin.y,
                                  PA_LEISURE_WALK_FREQ=ipaq_sl_lwday.y, PA_LEISURE_WALK_TIME_HR=ipaq_sl_lwdhrs.y, PA_LEISURE_WALK_TIME_MIN=ipaq_sl_lwdmin.y,
                                  PA_LEISURE_VIG_FREQ=ipaq_sl_lvday.y, PA_LEISURE_VIG_TIME_HR=ipaq_sl_lvdhrs.y, PA_LEISURE_VIG_TIME_MIN=ipaq_sl_lvdmin.y, 
                                  PA_LEISURE_MOD_FREQ=ipaq_sl_lmday.y, PA_LEISURE_MOD_TIME_HR=ipaq_sl_lmdhrs.y, PA_LEISURE_MOD_TIME_MIN=ipaq_sl_lmdmin.y) 
                          

#####selecting columns for IATs & renaming a variable#####
IAT<-IAT%>%
  select(group:expressions.percentcorrect)%>%
  rename(subject=group)