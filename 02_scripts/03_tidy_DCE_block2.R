######this script tidies DCE stated preference data for participants who were randomized to block 1 ######

block2<-block2 %>% 
  mutate(across(c(task),as.factor))

#Forced Choice Data
#Participants were forced to select between one of two physical activity reward programs: Program A and Program B
#1=Program A, 2= Program B
block2_forced<-data%>% 
  select(ID, "1"=DCE2.4, "2"=DCE2.7, "3"=DCE2.10, "4"=DCE2.13, "5"=DCE2.16, "6"=DCE2.19, "7"=DCE2.22, "8"=DCE2.25) %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_forced",
               values_drop_na = TRUE) %>% 
  mutate(across(c(task),as.factor))

#Any intervention vs Opt-out/Status Quo Data
#Participants selected between participating in the reward program and the opt-out/status quo (not participating in the program)
#1=Intervention 0=Opt-out/Status Quo
block2_tx<-data %>% 
  select(ID, 
         DCE2.5, DCE2.6, 
         DCE2.8, DCE2.9, 
         DCE2.11, DCE2.12, 
         DCE2.14, DCE2.15, 
         DCE2.17, DCE2.18, 
         DCE2.20, DCE2.21, 
         DCE2.23, DCE2.24,
         DCE2.26, DCE2.27) %>% 
  mutate("1"=coalesce(DCE2.5, DCE2.6)) %>% 
  mutate("2"=coalesce(DCE2.8, DCE2.9)) %>% 
  mutate("3"=coalesce(DCE2.11, DCE2.12)) %>% 
  mutate("4"=coalesce(DCE2.14, DCE2.15)) %>% 
  mutate("5"=coalesce(DCE2.17, DCE2.18)) %>% 
  mutate("6"=coalesce(DCE2.20, DCE2.21)) %>% 
  mutate("7"=coalesce(DCE2.23, DCE2.24)) %>% 
  mutate("8"=coalesce( DCE2.26, DCE2.27)) %>% 
  select(ID,"1":"8") %>% 
  replace(.==2,0) %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_tx",
               values_drop_na = TRUE)

#Preferred Intervention vs Opt-out/Status Quo Data
#Participants selected between participating in their preferred reward program and the opt-out/status quo (not participating in the program)
#These data are distinguished in that the preference for reward program is preserved
#1=Program A, 2=Program B, 3= Opt-out/Status Quo
block2_best<-data %>% 
  select(ID, 
         DCE2.5, DCE2.6, 
         DCE2.8, DCE2.9, 
         DCE2.11, DCE2.12, 
         DCE2.14, DCE2.15, 
         DCE2.17, DCE2.18, 
         DCE2.20, DCE2.21, 
         DCE2.23, DCE2.24,
         DCE2.26, DCE2.27)%>% 
  mutate_at(c("DCE2.5", "DCE2.8", "DCE2.11", "DCE2.14", "DCE2.17", "DCE2.20", "DCE2.23", "DCE2.26"), 
            ~case_when(.==1~1,
                       .==2~0)) %>% 
  mutate_at(c("DCE2.6", "DCE2.9", "DCE2.12", "DCE2.15", "DCE2.18", "DCE2.21", "DCE2.24", "DCE2.27"), 
            ~case_when(.==2~0,
                       .==1~2)) %>% 
  mutate("1"=coalesce(DCE2.5, DCE2.6)) %>% 
  mutate("2"=coalesce(DCE2.8, DCE2.9)) %>% 
  mutate("3"=coalesce(DCE2.11, DCE2.12)) %>% 
  mutate("4"=coalesce(DCE2.14, DCE2.15)) %>% 
  mutate("5"=coalesce(DCE2.17, DCE2.18)) %>% 
  mutate("6"=coalesce(DCE2.20, DCE2.21)) %>% 
  mutate("7"=coalesce(DCE2.23, DCE2.24)) %>% 
  mutate("8"=coalesce( DCE2.26, DCE2.27)) %>% 
  select(ID,"1":"8") %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_best",
               values_drop_na = TRUE)


#Joins the forced choice, opt-out, and best choice stated preference data
block2_processed<-block2_forced %>% 
  left_join(block2, by="task") %>% 
  left_join(block2_tx, by=c("ID","task")) %>% 
  left_join(block2_best, by=c("ID","task")) %>% 
  mutate(DCE_version=2)
