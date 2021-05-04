######this script tidies DCE stated preference data for participants who were randomized to block 1 ######

block1<-block1 %>% 
  mutate(across(c(task),as.factor))

#Forced Choice Data
#Participants were forced to select between one of two physical activity reward programs: Program A and Program B
#1=Program A, 2= Program B
block1_forced<-data%>% 
  select(ID, "1"=DCE1.4, "2"=DCE1.7, "3"=DCE1.10, "4"=DCE1.13, "5"=DCE1.16, "6"=DCE1.19, "7"=DCE1.22, "8"=DCE1.25) %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_forced",
               values_drop_na = TRUE) %>% 
  mutate(across(c(task),as.factor))

#Any intervention vs Opt-out/Status Quo Data
#Participants selected between participating in the reward program and the opt-out/status quo (not participating in the program)
#1=Intervention 0=Opt-out/Status Quo
block1_tx<-data %>% 
  select(ID, 
         DCE1.5, DCE1.6, 
         DCE1.8, DCE1.9, 
         DCE1.11, DCE1.12, 
         DCE1.14, DCE1.15, 
         DCE1.17, DCE1.18, 
         DCE1.20, DCE1.21, 
         DCE1.23, DCE1.24,
         DCE1.26, DCE1.27) %>% 
  mutate("1"=coalesce(DCE1.5, DCE1.6)) %>% 
  mutate("2"=coalesce(DCE1.8, DCE1.9)) %>% 
  mutate("3"=coalesce(DCE1.11, DCE1.12)) %>% 
  mutate("4"=coalesce(DCE1.14, DCE1.15)) %>% 
  mutate("5"=coalesce(DCE1.17, DCE1.18)) %>% 
  mutate("6"=coalesce(DCE1.20, DCE1.21)) %>% 
  mutate("7"=coalesce(DCE1.23, DCE1.24)) %>% 
  mutate("8"=coalesce( DCE1.26, DCE1.27)) %>% 
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
block1_best<-data %>% 
  select(ID, 
         DCE1.5, DCE1.6, 
         DCE1.8, DCE1.9, 
         DCE1.11, DCE1.12, 
         DCE1.14, DCE1.15, 
         DCE1.17, DCE1.18, 
         DCE1.20, DCE1.21, 
         DCE1.23, DCE1.24,
         DCE1.26, DCE1.27)%>% 
  mutate_at(c("DCE1.5", "DCE1.8", "DCE1.11", "DCE1.14", "DCE1.17", "DCE1.20", "DCE1.23", "DCE1.26"), 
            ~case_when(.==1~1,
                       .==2~0)) %>% 
  mutate_at(c("DCE1.6", "DCE1.9", "DCE1.12", "DCE1.15", "DCE1.18", "DCE1.21", "DCE1.24", "DCE1.27"), 
            ~case_when(.==2~0,
                       .==1~2)) %>% 
  mutate("1"=coalesce(DCE1.5, DCE1.6)) %>% 
  mutate("2"=coalesce(DCE1.8, DCE1.9)) %>% 
  mutate("3"=coalesce(DCE1.11, DCE1.12)) %>% 
  mutate("4"=coalesce(DCE1.14, DCE1.15)) %>% 
  mutate("5"=coalesce(DCE1.17, DCE1.18)) %>% 
  mutate("6"=coalesce(DCE1.20, DCE1.21)) %>% 
  mutate("7"=coalesce(DCE1.23, DCE1.24)) %>% 
  mutate("8"=coalesce( DCE1.26, DCE1.27)) %>% 
  select(ID,"1":"8") %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_best",
               values_drop_na = TRUE)


#Joins the forced choice, opt-out, and best choice stated preference data
block1_processed<-block1_forced %>% 
  left_join(block1, by="task") %>% 
  left_join(block1_tx, by=c("ID","task")) %>% 
  left_join(block1_best, by=c("ID","task")) %>% 
  mutate(DCE_version=1)


