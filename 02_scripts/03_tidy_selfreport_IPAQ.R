#this script cleans & scores IPAQ-SF data
#CLEAN DATA based on Guidelines for Data Processing and Analysis of IPAQ
######IPAQ Scoring Protocol##### 
#link to Scoring Protocol here: https://sites.google.com/site/theipaq/scoring-protocol
#https://docs.google.com/viewer?a=v&pid=sites&srcid=ZGVmYXVsdGRvbWFpbnx0aGVpcGFxfGd4OjE0NDgxMDk3NDU1YWRlZTM
#More details available below script

data_ipaq<-data%>%
  select(ID, VigDays, VigHours, VigMin, 
         ModDays, ModHours, ModMin,
         WalkDays, WalkHours, WalkMin, 
         SitHours, SitMin, PA_typical) %>%
  arrange(desc(VigHours)) %>%
  #1 c7de6087-7eeb-0090-a8c~       4       40#
  mutate(VigMin= case_when(VigHours>=10~VigHours,
                           VigHours==0.5~30,
                           VigMin<10~0,
                           (VigDays+ModDays+WalkDays>=1 & is.na(VigMin))~0,
                           TRUE~VigMin))%>%
  mutate(VigHours=case_when(VigHours>=10 | VigHours==0.5~0,
                           (VigDays+ModDays+WalkDays>=1 & is.na(VigHours))~0,
                           VigMin>=60~0,
                            TRUE~VigHours))%>%
  arrange(desc(ModHours)) %>%
  #1 c7de6087-7eeb-0090-a8c~       4       40      0       1       30#
  mutate(ModMin=case_when(ModHours>=10~ModHours,
                          ModHours==0.5~30,
                          ModMin<10~0,
                         (VigDays+ModDays+WalkDays>=1 & is.na(ModMin))~0,
                          TRUE~ModMin))%>%
  mutate(ModHours=case_when(ModHours>=10 |ModHours==0.5~0,
                           (VigDays+ModDays+WalkDays>=1 & is.na(ModHours))~0,
                           ModMin>=60~0,
                            TRUE~ModHours))%>%
  arrange(desc(WalkHours)) %>%
  #1 918fc53a-f83c-1bae-fe7~       0       NA     NA       1       NA     NA        7        60
#2 ed5d4339-7a81-af97-577~       0       NA     NA       0       NA     NA        7        30
#3 0c2c713d-c0a0-84a7-5d7~       0       NA     NA       0       NA     NA        3        30
#4 2b6164fa-bfca-115d-17b~       0       NA     NA       0       NA     NA        7        20
  mutate(WalkMin=case_when(WalkHours>=10~WalkHours,
                           WalkHours==0.5~30,
                           WalkMin<10~0,
                           (VigDays+ModDays+WalkDays>=1 & is.na(WalkMin))~0,
                          TRUE~WalkMin))%>%
  mutate(WalkHours=case_when(WalkHours>=10|WalkHours==0.5~0,
                             (VigDays+ModDays+WalkDays>=1 & is.na(WalkHours))~0,
                             WalkMin>=60~0,
                             TRUE~WalkHours)) %>%
  mutate(DailyVigMin=VigHours*60+VigMin) %>%
  mutate(DailyModMin=ModHours*60+ModMin) %>% 
  mutate(DailyWalkMin=WalkHours*60+WalkMin) %>% 
  mutate(TotalDailyPAMin=DailyVigMin+DailyModMin+DailyWalkMin) %>% 
  mutate(DailyVigMin_trunc=case_when(DailyVigMin>180~180,
                                     TRUE~DailyVigMin)) %>% 
  mutate(DailyModMin_trunc=case_when(DailyModMin>180~180,
                                     TRUE~DailyModMin)) %>% 
  mutate(DailyWalkMin_trunc=case_when(DailyWalkMin>180~180,
                                     TRUE~DailyWalkMin)) %>%
  mutate(TotalDailyPAMin_trunc=DailyVigMin_trunc+DailyModMin_trunc+DailyWalkMin_trunc) %>% 
  mutate(METs_Vig=8.0*VigDays*DailyVigMin_trunc) %>% 
  mutate(METs_Mod=4.0*ModDays*DailyModMin_trunc) %>% 
  mutate(METs_Walk=3.3*WalkDays*DailyWalkMin_trunc) %>% 
  mutate(METs_Total=METs_Vig+METs_Mod+METs_Walk) %>%
  mutate(FiveDays=case_when(VigDays+ModDays+WalkDays>=5~1,
                            VigDays+ModDays+WalkDays<5~0,
                            TRUE~NA_real_)) %>%
  mutate(SevenDays=case_when(VigDays+ModDays+WalkDays>=7~1,
                             VigDays+ModDays+WalkDays<7~0,
                             TRUE~NA_real_)) %>% 
  mutate(IPAQ_cat=case_when((VigDays>=3 & METs_Total>=1500) |
                              (SevenDays=1 & METs_Total>=3000)~3,
                            ((VigDays>=3 & DailyVigMin>=20)| 
                              (FiveDays==1 & ((DailyModMin + DailyWalkMin)>=30))|
                              (FiveDays==1 & (METs_Total>=600)))~2,
                            is.na(VigDays)~NA_real_,
                            TRUE~1)) %>% 
  mutate(PA_clinical_cutpoint=case_when(((VigDays*DailyVigMin_trunc)+(ModDays*DailyModMin_trunc))>=131~1,
                                        ((VigDays*DailyVigMin_trunc)+(ModDays*DailyModMin_trunc))<131~0,
                                         TRUE~NA_real_)) %>% 
  select(ID, PA_typical, 
         VigDays, DailyVigMin_trunc,
         ModDays, DailyModMin_trunc,
         WalkDays, DailyWalkMin_trunc,
         TotalDailyPAMin_trunc, 
         METs_Vig, METs_Mod, METs_Walk, 
         METs_Total,
         FiveDays, SevenDays,IPAQ_cat, PA_clinical_cutpoint) %>% 
  mutate(PA_typical=case_when(PA_typical==5~2,
                              PA_typical==NA_real_~NA_real_,
                              TRUE ~ PA_typical))


# #Save object to an rds file to preserve column data types
# saveRDS(data_ipaq,"01_data/02_processed/data_ipaq.rds")

# #Write to CSV file
# write.csv(data_ipaq,"01_data/02_processed/data_ipaq.csv", row.names=FALSE)


# %>% 
  # mutate(across(c(ID, PA_typical, FiveDays, SevenDays,IPAQ_cat),as.factor))


#Data Processing Rules
#Any responses to duration (time) provided in the hours and minutes response
#option should be converted from hours and minutes into minutes.
#II. To ensure that responses in ‘minutes’ were not entered in the ‘hours’ column by
#mistake during self-completion or during data entry process, values of ‘15’, ‘30’,
#‘45’, ‘60’ and ‘90’ in the ‘hours’ column should be converted to ‘15’, ‘30’, ‘45’, ‘60’
#and ‘90’ minutes, respectively, in the minutes column.
#III. In some cases duration (time) will be reported as weekly (not daily) e.g.,
#VWHRS, VWMINS. These data should be converted into an average daily time
#by dividing by 7.
#IV. If ‘don’t know’ or ‘refused ‘ or data are missing for time or days then that case is
#removed from analysis.
#Note: Both the number of days and daily time are required for the creation of categorical and
#continuous summary variables

#7.2 Maximum Values for Excluding Outliers
#This rule is to exclude data which are unreasonably high; these data are to be
#considered outliers and thus are excluded from analysis. All cases in which the sum
#total of all Walking, Moderate and Vigorous time variables is greater than 960
#minutes (16 hours) should be excluded from the analysis. This assumes that on
#average an individual of 8 hours per day is spent sleeping.
#The ‘days’ variables can take the range 0-7 days, or 8, 9 (don’t know or refused);
#values greater than 9 should not be allowed and those cases excluded from analysis.

#7.3 Minimum Values for Duration of Activity
#Only values of 10 or more minutes of activity should be included in the calculation of
#summary scores. The rationale being that the scientific evidence indicates that
#episodes or bouts of at least 10 minutes are required to achieve health benefits.
#Responses of less than 10 minutes [and their associated days] should be re-coded to
#‘zero’.

#7.4 Truncation of Data Rules
#This rule attempts to normalize the distribution of levels of activity which are usually
#skewed in national or large population data sets.
#In IPAQ short - it is recommended that all Walking, Moderate and Vigorous time
#variables exceeding ‘ 3 hours’ or ‘180 minutes’ are truncated (that is re-coded) to be
#equal to ‘180 minutes’ in a new variable. This rule permits a maximum of 21 hours of
#activity in a week to be reported for each category (3 hours * 7 days).

#As there are no established thresholds for presenting MET-minutes, the IPAQ
#Research Committee proposes that these data are reported as comparisons of
#median values and interquartile ranges for different populations.

#HOWEVER, in 7.4 Truncation of data rules
#In IPAQ long – the truncation process is more complicated, but to be consistent with
#the approach for IPAQ short requires that the variables total Walking, total Moderate intensity
#and total Vigorous-intensity activity are calculated and then, for each of
#these summed behaviours, the total value should be truncated to 3 hours (180 minutes).

#----
# relocate(TotalPAMin_trunc,.after=TotalPAMin_Daily)
# relocate(TotalVigMin_trunc,.after=TotalVigMin) %>%
#   relocate(TotalModMin_trunc,.after=TotalModMin) %>% 
#   relocate(TotalWalkMin_trunc,.after=TotalWalkMin) %>%   
#   relocate(VigHours_trunc,.after=VigMin) %>% 
#   relocate(VigMin_trunc,.after=VigHours_trunc) %>% 
#   relocate(ModHours_trunc,.after=ModMin) %>% 
#   relocate(ModMin_trunc,.after=ModHours_trunc) %>% 
#   relocate(WalkHours_trunc,.after=WalkMin) %>% 
#   relocate(WalkMin_trunc,.after=WalkHours_trunc) %>% 


# #Create a .txt file within the errors folder
# tidy_self_report_IPAQ_03 <- file(here("02_scripts","Errors", "01_tidy_selfreport_IPAQ.txt"), open = "wt")
# sink(tide_self_report_IPAQ_03, type = "message")



#end of script
#close the error message catching script and save the file
# sink(type = "message")
# close(01_IPAQ_03)
# 
# #Open the .txt file for inspection
# readLines(here("02_scripts","Errors", "01_tidy_selfreport_IPAQ.txt"))

#code that didn't work
#mutate(across(where(is.numeric),~na_if(., TotalPAMin_Daily>=960)))
#mutate(across(c(VigDays:TotalWalkMin)),~ifelse(TotalPAMin_Daily>=960, NA, .x))
#if_else(TotalPAMin_Daily>=960,as.numeric(NA_integer_), . )
# mutate(across(c("VigDays", "VigHours", "VigMin"),~na_if(.,TotalPAMin_Daily>=960)))
#The dot . is a placeholder for each column listed in the vars() function. Also, note the ~ operator in lieu of . =
#deprecated: mutate_at(vars(VigDays:TotalWalkMin),~ifelse(TotalPAMin_Daily>=960, NA, .x))
# mutate(across(VigDays:TotalPAMin_Daily),na_if(.,TotalPAMin_Daily>=960))
# mutate(across(c("VigHours", "ModHours", "WalkHours")),
#          ~(case_when(.>=10 | VigHours==0.5 ~0,
#                      .>3~3,
#                      TRUE~.)))
# 
# mutate(across(VigHours:WalkHours(contains("Hours")),
#                 ~(case_when(.>=10 ~ 0)
#                 )))