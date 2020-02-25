##NEXT STEPS

#this script includes all libraries needed for scripts, and imports the self-report data
#and choice sets data for blocks 1-3

#Create a .txt file within the errors folder
import_01<- file(here("02_scripts","Errors", "01_import.txt"), open = "wt")
sink(import_01, type = "message")

#load libraries here
library(here)
library(readr)

#import self-report raw datasets & choice experiments into environment
data_raw <- read_csv(here("01_data","01_raw", "data_raw.csv"))

#View(data_raw)

#choice_sets 
V1<-read.csv(here("01_data","01_raw", "DesignScenario_Block1.csv"))
V2<-read.csv(here("01_data","01_raw", "DesignScenario_Block2.csv"))
V3<-read.csv(here("01_data","01_raw", "DesignScenario_Block3.csv"))

#end of script
#close the error message catching script and save the file
sink(type = "message")
close(import_01)

#Open the .txt file for inspection
readLines(here("02_scripts","Errors", "01_import.txt"))