#Next steps
#01_import data
#02_clean_names
#03_tidy_selfreport

#all scripts should be able to run independently
#push to git

#This script sources several scripts
#make sure to launch the R process from the project's top-level directory, where the Rproj is housed  (fictional happiness)
#if you launch R from the shell, cd to the correct folder first

#load library here
library(here)

############SCRIPTS############
##01_import data
#this script imports data
source(here("02_scripts","01_import.R")) 

##02_clean_names
#this script selects specific columnn variables for self-report and renames variables
source(here("02_scripts","02_clean_names.R")) 

#These data exist already in the Rproj as an rds and a csv
#Save object to an rds file to preserve column data types
#saveRDS(data_rename,"01_data/02_processed/data_rename.rds")

#Write to CSV file
#write_csv(data_rename,"01_data/02_processed/data_rename.csv")

##03_tidy_selfreport
# this script creates new DF that includes tidy data for scoring self-report data: 
# IPAQ-SF
# the BREQ-3, 
# aversion (loss and risk)
# NICE OA criteria
# pain numeric rating scale 
# function numeric rating scale
# sociodemographics
source(here("02_scripts","03_tidy_selfreport.R"))


##03_tidy_selfreport_choice
# this script creates DF that converts the dataset from long to wide
source(here("02_scripts","03_tidy_selfreport_choice.R"))

##03 joins tidy_selfreport and tidy_selfreport_choice
#This joins two data sets together to create a complete data set
#complete_data_processed<-tidy_selfreport_choice%>%
#  full_join(tidy_selfreport,by="CINTid")

#View(complete_data_processed)

#These data exist already in the Rproj as an rds and a csv
#Save object to an rds file to preserve column data types
#saveRDS(complete_data_processed,"01_data/02_processed/complete_data_processed.rds")

#Write to CSV file
#write.csv(complete_data_processed,"01_data/02_processed/complete_data_processed.csv", row.names=FALSE)


############OUTPUT############
##01_analyses(currently as a script, eventually as Rmd file)

##02_posthoc (currently as a script, eventually as Rmd file)


##04 join df_1 and df_2
#in main script?
#complete_data_processed<-IAT_processed_dscore%>%
#  full_join(self_report_processed,by="group")%>%
#  mutate_at(vars(study_knee,gender,income,intention,location),
#            funs(factor))
#replace_na(), fix the deprecated replacement

#to write.csv(complete_data_processed,"01_data/02_processed/complete_data_processed.csv", row.names=FALSE)