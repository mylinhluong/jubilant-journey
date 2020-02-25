# this script creates new DF that includes tidy data for self-report data for
#####SOCIODEMOGRAPHICS
#What was done to tidy these data:
# -Age: Date of birth converted to age (use: lubridate package)>>need to deal with unlikely values
# -Data cleaning for height and weight to deal with extremely unlikely value
# -BMI: BMI calculation based on height and weight
# -join
#Gender
#Employment status
#Income level
#Household status
#State

#Convert DOB to age (in years) based on end of study date
####DOB was in DD/MM/YYYY dmy
####enddate in MM/DD/YYYY HH:MM:SS mdy_HMS
data_age<-data%>%
  select(ID, DOB, enddate)%>%
  mutate(DOB=dmy(DOB))%>%
  mutate(enddate=as_date(enddate, tz = NULL))%>%
  mutate(age_interval=DOB%--% enddate) %>% 
  mutate(age_years=age_interval %/% years(1)) %>% 
  select(ID,age=age_years)

#Data cleaning for height, weight and BMI
#Used data editing/cleaning from https://melbourneinstitute.unimelb.edu.au/assets/documents/hilda-bibliography/hilda-technical-papers/htec108.pdf
#inspected for cases that met one of the following criteria:
#• height less than 120cms or more than 210 cms;
#• weight less than 40 kgs or more than 200 kgs; or
#• a height and weight combination that led to a BMI of less than 15 or more than 50. 
#extremely unlikely values replaced with NA_integer_. 
#Cutoff points:
#       Men             Women
#    L       U         L         U
#H  130cms  229cms   110cms    210cms
#W  35kgs   300kgs    25kgs   300kgs

#ID=unique participant ID
#gender: 1=M, 2=F
#weight=in kg
#height=in cm

#ID=unique participant ID
#select cols, only if gender is reported
data_BMI<-data %>%  
  select(ID, gender, height, weight)%>% 
  filter(!is.na(gender))

#parameters in for loop
num_row = 1

#screen the input data table line by line. Add the row to the tidy table if the line meets the criteria
 
for (r in 1:nrow(data_BMI)) {
  print(r)
  
  #1st layer: gender-males
  if (data_BMI[r,'gender'] == 1){
    #2nd layer: height and weight  
    if (!(data_BMI[r,'height']%in% 130:229)){
      data_BMI[r,'height']= NA_integer_
    }
    if (!(data_BMI[r,'weight']%in% 35:300)){
      data_BMI[r,'weight']= NA_integer_}
  }
  
  #1st layer: gender-females
  if (data_BMI[r,'gender'] == 2){
    #2nd layer: height and weight
    if (!(data_BMI[r,'height']%in% 110:210)){
      data_BMI[r,'height']= NA_integer_
    }
    if (!(data_BMI[r,'weight']%in% 25:300)){
      data_BMI[r,'weight']= NA_integer_}
  }
  
  
  num_row = num_row +1
} 

#add col of BMI_calc
data_BMI<-data_BMI%>%
  mutate(BMI_calc=round((weight/height/height)*10000))%>% 
  select(ID, 
         gender, 
         weight, 
         height, 
         BMI_calc)



#DEALING WITH REMAINING SOCIODEMOGRAPHICS + MERGING WITH CLEANED HEIGHT, WEIGHT & BMI DATA
#converting str of data: income, employment status, household status, state and gender to factors
#merge sociodemographic datasets: above + age + BMI
#BMI: replace extremely unlikely values of height and weight combination that led to a BMI of 
#less than 15 or more than 50 with NA_integer_.
#age: replace extremely unlikely values of age with NA_integer_

data_sociodems<-data %>%
  select(ID, income, employed, household, state) %>%
  left_join(data_age, by="ID") %>% 
  left_join(data_BMI, by="ID") %>% 
  arrange(desc(BMI_calc)) %>% 
  mutate(BMI_calc=ifelse(BMI_calc>50,NA_integer_,BMI_calc))%>%
  mutate(BMI_calc=ifelse(BMI_calc<15,NA_integer_,BMI_calc)) %>% 
  select(ID, gender, age, BMI_calc, income, employed, household, state)

# #HEIGHT AND WEIGHT DATA CLEANING & BMI CALC using mutate as opposed to for loop
# #select cols, only if gender is reported
# data_BMI<-data %>% 
#   select(ID, gender, height, weight)%>% 
#   filter(!is.na(gender))
# 
# #check on values
# data_BMI_m<-data%>%
#   select(ID, gender, height, weight)%>%
#   filter(gender==1)%>%
#   arrange(desc(height))%>%
#   #   ID                              gender height weight
#   #1 8195fb32-114a-0e35-9f7e-ccbe0~      1      0      0
#   #2 918fc53a-f83c-1bae-fe76-f2a4b~      1     63     93
#   #3 78c3c5a7-f7d2-6c16-b2eb-f2e88~      1     73     90
#   #4 1d7630f7-aa29-2c78-07bb-075da~      1    121     52
#   arrange(desc(weight))
# #   ID                              gender height weight
# #1 8195fb32-114a-0e35-9f7e-ccbe02~      1      0      0
# #2 b9a3796f-403c-1822-a823-4e8864~      1    158     10
# 
# #head(data_BMI_m)
# #tail(data_BMI_m)
# 
# data_BMI_f<-data%>%
#   select(ID, gender, height, weight)%>%
#   filter(gender==2)%>%
#   arrange(desc(height))%>%
#   #  ID                                   gender height weight
#   #UPPER#
#   #1 42d4da34-0148-27d0-de78-edff2676d6e6      2    511     75
#   #LOWER#
#   #3 d9db3f92-9a2e-ff1e-d1de-08f027154c58      2    101     97
#   #4 8f5b5a41-41e6-a18a-2d0d-6d7038e25498      2     82    120
#   #5 eb8219b4-5e6a-0f37-f3f8-2ea9647ff578      2     80     90
#   #6 26d9a7ee-bd53-baa3-afa2-90f21f4ae426      2     79     65#
#   arrange(desc(weight))
# #UPPER#
# #LOWER#
# #head(data_BMI_f)
# #tail(data_BMI_f)


#View(data_BMI)


#https://blog.usejournal.com/the-ultimate-r-guide-to-process-missing-or-outliers-in-dataset-65e2e59625c1
#https://www.pluralsight.com/guides/cleaning-up-data-from-outliers
