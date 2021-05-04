######TIDYING DCE DATA######
#next steps, working on block1_best to recode data into 1 for A, 2 for B and 0 for NoTx
#update remaining design scenario blocks
#update rename to include :3.27 to re-upload
#install.packages into another R script
#figure out where to put tidy_DCE, as part of or not as part of self-report
#check to make sure that the design scenarios are right

######LIBRARIES
library(here)
library(readr)
library(dplyr)
library(tidyr)

# #Create a .txt file within the errors folder
# tidy_selfreport_03 <- file(here("02_scripts","Errors", "03_tidy_DCE.txt"), open = "wt")
# sink(tidy_selfreport_03, type = "message")

#import renamed dataset into environment
data<- read_csv(here("01_data","02_processed", "data_rename.csv"))
block1<-read_csv(here("01_data", "01_raw", "DesignScenario_Block1.csv"))
block2<-read_csv(here("01_data", "01_raw", "DesignScenario_Block2.csv"))
block3<-read_csv(here("01_data", "01_raw", "DesignScenario_Block3.csv"))


############SCRIPTS############
#this script tidies DCE stated preference data for participants who were randomized to block 1
source(here("02_scripts","03_tidy_DCE_block1.R"))

#this script tidies DCE stated preference data for participants who were randomized to block 2
source(here("02_scripts","03_tidy_DCE_block2.R"))

#this script tidies DCE stated preference data for participants who were randomized to block 3
source(here("02_scripts","03_tidy_DCE_block3.R"))

###### Creating tidy DCE data#####
data_DCE_processed<-bind_rows(block1_processed, block2_processed, block3_processed) %>% 
  relocate(DCE_version,.after=ID) %>% 
  relocate(choice_tx,.after=choice_forced) %>% 
  relocate(choice_best,.after=choice_tx) 
# %>% 
#   mutate(across(where(is.numeric),as.factor)) %>% 
#   mutate(across(where(is.character),as.factor))
# 
# str(data_DCE_processed)

#Save object to an rds file to preserve column data types
saveRDS(data_DCE_processed,"01_data/02_processed/data_DCE_processed.rds")

#Write to CSV file
write.csv(data_DCE_processed,"01_data/02_processed/data_DCE_processed.csv", row.names=FALSE)
