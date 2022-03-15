# this script creates new DF that includes tidy data for scoring self-report data for
######BREQ-3,
#Scoring Protocol: http://exercise-motivation.bangor.ac.uk/breq/brqscore.php
#Multidimensional scoring
#In order to use the BREQ as multidimensional scales, simply calculate the mean scores for each set of items as 
#indicated below (the original BREQ scoring key is given for anyone still using that version).
	
#BREQ-3
#Amotivation	
#2,8,14,20
 	 
#External regulation	
#6,12,18,24
 	 
#Introjected regulation	
#4,10,16,22*	 

#Identified regulation	
# 1,7,13, 19
 	 
#Integrated regulation	
# 5,11,17,23
 	 
#Intrinsic regulation	
# 3,9,15,21
 
#What was done to tidy these data:
# -recoded of Qualtrics values into a 0-4 scale
# -calculated the mean score of each set of items (using psych package)

data_breq=data %>% 
  select(ID,breq3_id1:breq3_ext4) %>% 
  mutate(across(starts_with("breq3"),
    ~(case_when(.== 1 ~ 0,
                .== 3 ~ 1, 
                .== 4 ~ 2,
                .== 5 ~ 3,
                .== 6 ~ 4
                ))))%>%
	mutate(identified= rowMeans(across(breq3_id1:breq3_id4)))%>%
  mutate(amotivation= rowMeans(across(breq3_amov1:breq3_amov4)))%>%
  mutate(intrinsic= rowMeans(across(breq3_intrins1:breq3_intrins4)))%>%
  mutate(integrated= rowMeans(across(breq3_integ1:breq3_integ4)))%>%
  mutate(external= rowMeans(across(breq3_ext1:breq3_ext4)))%>%
  select(ID, identified, amotivation, intrinsic, integrated, external)

#RESOURCES
#https://jmgirard.com/rowwise-means/