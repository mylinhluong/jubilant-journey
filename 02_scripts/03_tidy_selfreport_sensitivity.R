######SENSITIVITY ANALYSIS
#####monotonicity
#choice monotonicity is assessed by including a choice task that involves a dominated choice alternative in a choice set.
data_sensitivity<-data%>% 
  select(ID, DCE1.mono, DCE2.mono, DCE3.mono, duration)%>% 
  mutate(DCE_mono=coalesce(DCE1.mono, DCE2.mono, DCE3.mono)) %>%
  mutate(DCE_mono=case_when(DCE_mono==2~1,
                            DCE_mono==1~0,
                            TRUE ~ NA_real_)) %>%
  select(ID, DCE_mono, duration)

#REFERENCES
#Mattmann et al., 2019 Choice Certainty, consistency, and monotonicity in discrete choice experiments
# ‘choice monotonicity’ is assumed to hold if a
# respondent chooses a non-dominated alternative in a choice task that contains a dominated alternative, which is a hypothetical alternative that is worse than at least one other alternative in a choice
# task with respect to all attributes.