######SENSITIVITY ANALYSIS
#####monotonicity
data_sensitivity<-data%>% 
  select(ID, DCE1.mono, DCE2.mono, DCE3.mono, duration)%>% 
  mutate(DCE_mono=coalesce(DCE1.mono, DCE2.mono, DCE3.mono)) %>%
  mutate(DCE_mono=case_when(DCE_mono==2~1,
                            DCE_mono==1~0,
                            TRUE ~ NA_real_)) %>% 
  select(ID, DCE_mono, duration)