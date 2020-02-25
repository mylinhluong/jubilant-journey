######SCORING RISK & LOSS AVERSION######
#transform multiple columns into one score (1-12, 1= low aversion, 12 = high aversion)

data_aversion<-data%>% 
  select(ID, risk1_1:risk1_12, risk2_1:risk2_12, loss1_1:loss1_12, loss2_1:loss2_12)%>% 
  mutate_at(
    vars(contains("risk")),~replace(., is.na(.),0)) %>% 
  mutate_at(
    vars(contains("loss")),~replace(.,is.na(.),0)) %>% 
  mutate(across(everything(),~replace(.,.==2,1))) %>% 
  mutate(aversion_version=case_when(risk1_1>=1 ~ 1,
                                    TRUE ~ 2)) %>%
  mutate(risk_score=rowSums(across(risk1_1:risk2_12))) %>% 
  mutate(loss_score=rowSums(across(loss1_1:loss2_12))) %>% 
  select(ID, aversion_version, risk_score, loss_score)