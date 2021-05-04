######this script tidies DCE stated preference data for participants who were randomized to block 1 ######

block3<-block3 %>% 
  mutate(across(c(task),as.factor))

#Forced Choice Data
#Participants were forced to select between one of two physical activity reward programs: Program A and Program B
#1=Program A, 2= Program B
block3_forced<-data%>% 
  select(ID, "1"=DCE3.4, "2"=DCE3.7, "3"=DCE3.10, "4"=DCE3.13, "5"=DCE3.16, "6"=DCE3.19, "7"=DCE3.22, "8"=DCE3.25) %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_forced",
               values_drop_na = TRUE) %>% 
  mutate(across(c(task),as.factor))

#Any intervention vs Opt-out/Status Quo Data
#Participants selected between participating in the reward program and the opt-out/status quo (not participating in the program)
#1=Intervention 0=Opt-out/Status Quo
block3_tx<-data %>% 
  select(ID, 
         DCE3.5, DCE3.6, 
         DCE3.8, DCE3.9, 
         DCE3.11, DCE3.12, 
         DCE3.14, DCE3.15, 
         DCE3.17, DCE3.18, 
         DCE3.20, DCE3.21, 
         DCE3.23, DCE3.24,
         DCE3.26, DCE3.27) %>% 
  mutate("1"=coalesce(DCE3.5, DCE3.6)) %>% 
  mutate("2"=coalesce(DCE3.8, DCE3.9)) %>% 
  mutate("3"=coalesce(DCE3.11, DCE3.12)) %>% 
  mutate("4"=coalesce(DCE3.14, DCE3.15)) %>% 
  mutate("5"=coalesce(DCE3.17, DCE3.18)) %>% 
  mutate("6"=coalesce(DCE3.20, DCE3.21)) %>% 
  mutate("7"=coalesce(DCE3.23, DCE3.24)) %>% 
  mutate("8"=coalesce( DCE3.26, DCE3.27)) %>% 
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
block3_best<-data %>% 
  select(ID, 
         DCE3.5, DCE3.6, 
         DCE3.8, DCE3.9, 
         DCE3.11, DCE3.12, 
         DCE3.14, DCE3.15, 
         DCE3.17, DCE3.18, 
         DCE3.20, DCE3.21, 
         DCE3.23, DCE3.24,
         DCE3.26, DCE3.27)%>% 
  mutate_at(c("DCE3.5", "DCE3.8", "DCE3.11", "DCE3.14", "DCE3.17", "DCE3.20", "DCE3.23", "DCE3.26"), 
            ~case_when(.==1~1,
                       .==2~0)) %>% 
  mutate_at(c("DCE3.6", "DCE3.9", "DCE3.12", "DCE3.15", "DCE3.18", "DCE3.21", "DCE3.24", "DCE3.27"), 
            ~case_when(.==2~0,
                       .==1~2)) %>% 
  mutate("1"=coalesce(DCE3.5, DCE3.6)) %>% 
  mutate("2"=coalesce(DCE3.8, DCE3.9)) %>% 
  mutate("3"=coalesce(DCE3.11, DCE3.12)) %>% 
  mutate("4"=coalesce(DCE3.14, DCE3.15)) %>% 
  mutate("5"=coalesce(DCE3.17, DCE3.18)) %>% 
  mutate("6"=coalesce(DCE3.20, DCE3.21)) %>% 
  mutate("7"=coalesce(DCE3.23, DCE3.24)) %>% 
  mutate("8"=coalesce( DCE3.26, DCE3.27)) %>% 
  select(ID,"1":"8") %>% 
  pivot_longer(!ID, 
               names_to="task",
               values_to="choice_best",
               values_drop_na = TRUE)


#Joins the forced choice, opt-out, and best choice stated preference data
block3_processed<-block3_forced %>% 
  left_join(block3, by="task") %>% 
  left_join(block3_tx, by=c("ID","task")) %>% 
  left_join(block3_best, by=c("ID","task")) %>% 
  mutate(DCE_version=3)
