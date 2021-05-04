# resources
# http://lab.agr.hokudai.ac.jp/nmvr/02-dce.html
# https://www.alisonpearce.net/data/practical-resources-for-analysing-your-first-dce/
# https://cran.r-project.org/web/packages/apollo/vignettes/apollofirstexample.html
# https://its.ucdavis.edu/wp-content/uploads/TTP289_W2020_Bunch_syllabus_21Nov2019.pdf
# https://mran.microsoft.com/snapshot/2020-02-28/web/packages/apollo/vignettes/apollofirstexample.html
# https://www.rpubs.com/isiddhartha/example_apollo_MNL
# http://www.apollochoicemodelling.com/files/Apollo_example_8.r
# http://www.apollochoicemodelling.com/files/Apollo_example_10.r
# ####################################################### #
#### 1. Definition of core settings                        
# ####################################################### #
### Clear memory
rm(list = ls())

### Load libraries
library(here)
library(readr)
library(apollo)
library(dplyr)
### Initialise code
apollo_initialise()

### Set core controls
apollo_control<-list(
  modelName= "dce_model1",
  modelDescr="MNL model on SP data",
  indivID= "ID"
)

# ################################################################# #
#### 2. Data loading and apply any transformations               ####
# ################################################################# #
database<-read_rds(here("01_data","02_processed", "00_data_processed.rds")) 
 

# ####################################################### #
#### 3. Parameter definition                           ####
# ####################################################### #

### Vector of parameters, including any that are kept fixed 
### during estimation

apollo_beta = c(
  b_goal_30=0,
  b_goal_60=0,
  b_goal_90=0,
  b_form_cash=0,
  b_form_voucher=0,
  b_form_donate=0,
  b_mag_160=0,
  b_mag_300=0,
  b_mag_500=0,
  b_dir_pos=0,
  b_dir_neg=0
  )

### Vector with names (in quotes) of parameters to be
###  kept fixed at their starting value in apollo_beta.
### Use apollo_beta_fixed = c() for no fixed parameters.
apollo_fixed<-c("b_goal_30","b_form_cash", "b_mag_160", "b_dir_pos")

#apollo_fixed <- c()
# ####################################################### #
#### 4. Input validation                               ####
# ####################################################### #

apollo_inputs = apollo_validateInputs()
# Several observations per individual detected based on the value of ID. Setting panelData in apollo_control set
# to TRUE.
# All checks on apollo_control completed.
# All checks on database completed.

# ####################################################### #
#### 5. Define Model and Likelihood definition                          ####
# ####################################################### #

apollo_probabilities=function(apollo_beta, apollo_inputs, 
                              functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as
  ### in mnl_settings, order is irrelevant.
  V = list()
  V[['alt1']] = b_goal_30*(goal1==30) + b_goal_60*(goal1==60)+ b_goal_90*(goal1==90)+
    b_form_cash*(form1=="cash")+b_form_donate*(form1=="donate")+b_form_voucher*(form1=="voucher")+
    b_mag_160*(mag1==160)+b_mag_300*(mag1==300)+b_mag_500*(mag1==500)+
    b_dir_pos*(dir1=="pos")+b_dir_neg*(dir1=="neg")
  V[['alt2']] = b_goal_30*(goal2==30) + b_goal_60*(goal2==60)+ b_goal_90*(goal2==90)+
    b_form_cash*(form2=="cash")+b_form_donate*(form2=="donate")+b_form_voucher*(form2=="voucher")+
    b_mag_160*(mag2==160)+b_mag_300*(mag2==300)+b_mag_500*(mag2==500)+
    b_dir_pos*(dir2=="pos")+b_dir_neg*(dir2=="neg")
  
  # asc_1 + 
  # asc_2 +
  
  ### Define settings for MNL model component
  mnl_settings = list(
    alternatives  = c(alt1=1, alt2=2), 
    avail         = 1, 
    choiceVar     = choice_forced,
    # explanators  = database[,c("risk_score","loss_score","intrinsic","extrinsic", "pain_NRS", "function_NRS",
    #                            "IPAQ_cat", 
    #                            "gender", "age", "BMI_calc", "income")],
  V=V)
  
    ### Compute probabilities using MNL model
  P[['model']] = apollo_mnl(mnl_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}


# ####################################################### #
#### 6. Model estimation and reporting                 ####
# ####################################################### #

model = apollo_estimate(apollo_beta, apollo_fixed, 
                        apollo_probabilities, 
                        apollo_inputs)

# Testing likelihood function...
# WARNING: Availability not provided (or some elements are NA). Full availability assumed.
# 
# Overview of choices for MNL model component :
#                                    alt1    alt2
# Times available                  2294.00 2294.00
# Times chosen                     1067.00 1227.00
# Percentage chosen overall          46.51   53.49
# Percentage chosen when available   46.51   53.49
# 
# Pre-processing likelihood function...
# Preparing pre-processing report
# 
# Testing influence of parameters.......
# Starting main estimation
# Initial function value: -1590.08 
# Initial gradient value:
#   b_goal_60      b_goal_90 b_form_voucher  b_form_donate      b_mag_300      b_mag_500      b_dir_neg 
#     30.5         -186.5          138.0         -485.0           22.5          152.5            8.0 
# initial  value 1590.079632 
# iter   2 value 1268.441533
# iter   3 value 1202.052010
# iter   4 value 1200.157902
# iter   5 value 1195.722906
# iter   6 value 1191.905540
# iter   7 value 1157.219204
# iter   8 value 1156.673704
# iter   9 value 1139.146684
# iter  10 value 1132.782311
# iter  11 value 1099.779611
# iter  12 value 1099.463659
# iter  13 value 1099.453357
# iter  14 value 1099.451959
# iter  15 value 1099.451820
# iter  15 value 1099.451818
# iter  15 value 1099.451818
# final  value 1099.451818 
# converged
# Estimated parameters:
#   Estimate
# b_goal_30          0.00000
# b_goal_60         -0.19134
# b_goal_90         -0.86738
# b_form_cash        0.00000
# b_form_voucher    -0.51883
# b_form_donate     -1.96009
# b_mag_160          0.00000
# b_mag_300          0.63871
# b_mag_500          0.84113
# b_dir_pos          0.00000
# b_dir_neg         -0.06212
# 
# Computing covariance matrix using analytical gradient.
# 0%....25%....50%....75%....100%
# Negative definite Hessian with maximum eigenvalue: -77.850397
# Computing score matrix...
# Calculating LL(0) for applicable models...
# Calculating LL of each model component...

apollo_modelOutput(model, list (printPVal=TRUE))
# Model run using Apollo for R, version 0.2.5 on Windows by My-Linh 
# www.ApolloChoiceModelling.com
# 
# Model name                       : dce_model1
# Model description                : MNL model on SP data
# Model run at                     : 2021-06-11 16:53:46
# Estimation method                : bfgs
# Model diagnosis                  : successful convergence 
# Number of individuals            : 288
# Number of rows in database       : 2294
# Number of modelled outcomes      : 2294
# 
# Number of cores used             :  1 
# Model without mixing
# 
# LL(start)                        : -1590.08
# LL(0)                            : -1590.08
# LL(final)                        : -1099.452
# Rho-square (0)                   :  0.3086 
# Adj.Rho-square (0)               :  0.3042 
# AIC                              :  2212.9 
# BIC                              :  2253.07 
# 
# 
# Estimated parameters             :  7
# Time taken (hh:mm:ss)            :  00:00:2.07 
# pre-estimation              :  00:00:0.8 
# estimation                  :  00:00:0.71 
# post-estimation             :  00:00:0.56 
# Iterations                       :  17  
# Min abs eigenvalue of Hessian    :  77.8504 
# 
# Estimates:
#                   Estimate        s.e.   t.rat.(0)  p(1-sided)    Rob.s.e. Rob.t.rat.(0)  p(1-sided)
# b_goal_30          0.00000          NA          NA          NA          NA            NA          NA
# b_goal_60         -0.19134     0.07720      -2.478    0.006599     0.08397        -2.279     0.01134
# b_goal_90         -0.86738     0.07617     -11.388    0.000000     0.09436        -9.192     0.00000
# b_form_cash        0.00000          NA          NA          NA          NA            NA          NA
# b_form_voucher    -0.51883     0.07106      -7.301   1.428e-13     0.07595        -6.831   4.206e-12
# b_form_donate     -1.96009     0.08633     -22.705    0.000000     0.11333       -17.295     0.00000
# b_mag_160          0.00000          NA          NA          NA          NA            NA          NA
# b_mag_300          0.63871     0.08125       7.861   1.887e-15     0.07983         8.001   6.661e-16
# b_mag_500          0.84113     0.07464      11.269    0.000000     0.08854         9.500     0.00000
# b_dir_pos          0.00000          NA          NA          NA          NA            NA          NA
# b_dir_neg         -0.06212     0.05372      -1.157    0.123727     0.05476        -1.135     0.12828


#apollo_saveOutput(model)

# apollo_beta = c(
#   +   b_goal_30=0,
#   +   b_goal_60=0,
#   +   b_goal_90=0,
#   +   b_form_cash=0,
#   +   b_form_voucher=0,
#   +   b_form_donate=0,
#   +   b_mag_160=0,
#   +   b_mag_300=0,
#   +   b_mag_500=0,
#   +   b_dir_pos=0,
#   +   b_dir_neg=0
#   +   )
# > apollo_fixed <- c()
# > apollo_inputs = apollo_validateInputs()
# Several observations per individual detected based on the value of ID. Setting
# panelData in apollo_control set to TRUE.
# All checks on apollo_control completed.
# All checks on database completed.
# > apollo_probabilities=function(apollo_beta, apollo_inputs, 
#                                 +                               functionality="estimate"){
#   +   
#     +   ### Attach inputs and detach after function exit
#     +   apollo_attach(apollo_beta, apollo_inputs)
#   +   on.exit(apollo_detach(apollo_beta, apollo_inputs))
#   +   
#     +   ### Create list of probabilities P
#     +   P = list()
#     +   
#       +   ### List of utilities: these must use the same names as
#       +   ### in mnl_settings, order is irrelevant.
#       +   V = list()
#       +   V[['alt1']] = b_goal_30*(goal1==30) + b_goal_60*(goal1==60)+ b_goal_90*(goal1==90)+
#         +     b_form_cash*(form1=="cash")+b_form_donate*(form1=="donate")+b_form_voucher*(form1=="voucher")+
#         +     b_mag_160*(mag1==160)+b_mag_300*(mag1==300)+b_mag_500*(mag1==500)+
#         +     b_dir_pos*(dir1=="pos")+b_dir_neg*(dir1=="neg")
#       +   V[['alt2']] = b_goal_30*(goal2==30) + b_goal_60*(goal2==60)+ b_goal_90*(goal2==90)+
#         +     b_form_cash*(form2=="cash")+b_form_donate*(form2=="donate")+b_form_voucher*(form2=="voucher")+
#         +     b_mag_160*(mag2==160)+b_mag_300*(mag2==300)+b_mag_500*(mag2==500)+
#         +     b_dir_pos*(dir2=="pos")+b_dir_neg*(dir2=="neg")
#       + mnl_settings = list(
#         +     alternatives  = c(alt1=1, alt2=2), 
#         +     avail         = 1, 
#         +     choiceVar     = choice_forced,
#         +     # explanators  = database[,c("risk_score","loss_score","intrinsic","extrinsic", "pain_NRS", "function_NRS",
#           +     #                            "IPAQ_cat", 
#           +     #                            "gender", "age", "BMI_calc", "income")],
#           +   V=V)
#       + 
#         
#         > apollo_probabilities=function(apollo_beta, apollo_inputs, 
#                                         +                               functionality="estimate"){
#           +   
#             +   ### Attach inputs and detach after function exit
#             +   apollo_attach(apollo_beta, apollo_inputs)
#           +   on.exit(apollo_detach(apollo_beta, apollo_inputs))
#           +   
#             +   ### Create list of probabilities P
#             +   P = list()
#             +   
#               +   ### List of utilities: these must use the same names as
#               +   ### in mnl_settings, order is irrelevant.
#               +   V = list()
#               +   V[['alt1']] = b_goal_30*(goal1==30) + b_goal_60*(goal1==60)+ b_goal_90*(goal1==90)+
#                 +     b_form_cash*(form1=="cash")+b_form_donate*(form1=="donate")+b_form_voucher*(form1=="voucher")+
#                 +     b_mag_160*(mag1==160)+b_mag_300*(mag1==300)+b_mag_500*(mag1==500)+
#                 +     b_dir_pos*(dir1=="pos")+b_dir_neg*(dir1=="neg")
#               +   V[['alt2']] = b_goal_30*(goal2==30) + b_goal_60*(goal2==60)+ b_goal_90*(goal2==90)+
#                 +     b_form_cash*(form2=="cash")+b_form_donate*(form2=="donate")+b_form_voucher*(form2=="voucher")+
#                 +     b_mag_160*(mag2==160)+b_mag_300*(mag2==300)+b_mag_500*(mag2==500)+
#                 +     b_dir_pos*(dir2=="pos")+b_dir_neg*(dir2=="neg")
#               +   
#                 +   # asc_1 + 
#                 +   # asc_2 +
#                 +   
#                 +   ### Define settings for MNL model component
#                 +   mnl_settings = list(
#                   +     alternatives  = c(alt1=1, alt2=2), 
#                   +     avail         = 1, 
#                   +     choiceVar     = choice_forced,
#                   +     # explanators  = database[,c("risk_score","loss_score","intrinsic","extrinsic", "pain_NRS", "function_NRS",
#                     +     #                            "IPAQ_cat", 
#                     +     #                            "gender", "age", "BMI_calc", "income")],
#                     +   V=V)
#                 +   
#                   +     ### Compute probabilities using MNL model
#                   +   P[['model']] = apollo_mnl(mnl_settings, functionality)
#                   +   
#                     +   ### Take product across observation for same individual
#                     +   P = apollo_panelProd(P, apollo_inputs, functionality)
#                     +   
#                       +   ### Prepare and return outputs of function
#                       +   P = apollo_prepareProb(P, apollo_inputs, functionality)
#                       +   return(P)
#                       + }
#         > model = apollo_estimate(apollo_beta, apollo_fixed, 
#                                   +                         apollo_probabilities, 
#                                   +                         apollo_inputs)
#         
#         Testing likelihood function...
#         WARNING: Availability not provided (or some elements are NA). Full availability
#         assumed.
#         
#         Overview of choices for MNL model component :
#           alt1    alt2
#         Times available                  2294.00 2294.00
#         Times chosen                     1067.00 1227.00
#         Percentage chosen overall          46.51   53.49
#         Percentage chosen when available   46.51   53.49
#         
#         Pre-processing likelihood function...
#         Preparing pre-processing report
#         
#         Testing influence of parameters...........
#         Starting main estimation
#         Initial function value: -1590.08 
#         Initial gradient value:
#           b_goal_30      b_goal_60      b_goal_90    b_form_cash b_form_voucher  b_form_donate 
#         156.0           30.5         -186.5          347.0          138.0         -485.0 
#         b_mag_160      b_mag_300      b_mag_500      b_dir_pos      b_dir_neg 
#         -175.0           22.5          152.5           -8.0            8.0 
#         initial  value 1590.079632 
#         iter   2 value 1141.187364
#         iter   3 value 1138.333969
#         iter   4 value 1137.753614
#         iter   5 value 1137.425807
#         iter   6 value 1117.662587
#         iter   7 value 1111.401840
#         iter   8 value 1111.333798
#         iter   9 value 1110.421237
#         iter  10 value 1100.804568
#         iter  11 value 1099.461641
#         iter  12 value 1099.451893
#         iter  13 value 1099.451821
#         iter  13 value 1099.451818
#         iter  13 value 1099.451818
#         final  value 1099.451818 
#         converged
#         Estimated parameters:
#           Estimate
#         b_goal_30          0.35291
#         b_goal_60          0.16156
#         b_goal_90         -0.51447
#         b_form_cash        0.82631
#         b_form_voucher     0.30747
#         b_form_donate     -1.13378
#         b_mag_160         -0.49328
#         b_mag_300          0.14544
#         b_mag_500          0.34785
#         b_dir_pos          0.03106
#         b_dir_neg         -0.03106
#         
#         Computing covariance matrix using analytical gradient.
#         0%....25%....50%....75%..100%
#         WARNING: Some eigenvalues of the Hessian are complex, indicating that the Hessian
#         is not symmetrical.
#         Computing score matrix...
#         Calculating LL(0) for applicable models...
#         Calculating LL of each model component...
#         Warning messages:
#           1: In sqrt(diag(varcov)) : NaNs produced
#         2: In sqrt(diag(robvarcov)) : NaNs produced

# ####################################################### #
#### 7. Postprocessing of results                      ####
# ####################################################### #

### Use the estimated model to make predictions
predictions_base = apollo_prediction(model, 
                                     apollo_probabilities, 
                                     apollo_inputs)
#> Running predictions from model using parameter estimates...
#> Predicted aggregated demand at model estimates
#>         car bus  air rail
#> Demand 1946 358 1522 3174
#> 
#> The output from apollo_prediction is a matrix containing the
#>   predictions at the estimated values.

### Now imagine the cost for rail increases by 10% 
### and predict again
database$cost_rail = 1.1*database$cost_rail
apollo_inputs   = apollo_validateInputs()
#> Several observations per individual detected based on the value of ID.
#>   Setting panelData in apollo_control set to TRUE.
#> All checks on apollo_control completed.
#> All checks on database completed.
predictions_new = apollo_prediction(model, 
                                    apollo_probabilities, 
                                    apollo_inputs)
#> Running predictions from model using parameter estimates...
#> Predicted aggregated demand at model estimates
#>            car    bus     air    rail
#> Demand 2132.59 399.33 1645.75 2822.34
#> 
#> The output from apollo_prediction is a matrix containing the
#>   predictions at the estimated values.

### Compare predictions
change=(predictions_new-predictions_base)/predictions_base
### Not interested in chosen alternative now, 
### so drop last column
change=change[,-ncol(change)]
### Summary of changes (possible presence of NAs due to
### unavailable alternatives)
summary(change)
#>        ID     Observation      car              bus              air        
#>  Min.   :0   Min.   :0    Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
#>  1st Qu.:0   1st Qu.:0    1st Qu.:0.0725   1st Qu.:0.0738   1st Qu.:0.0704  
#>  Median :0   Median :0    Median :0.1168   Median :0.1218   Median :0.1100  
#>  Mean   :0   Mean   :0    Mean   :0.1105   Mean   :0.1225   Mean   :0.1121  
#>  3rd Qu.:0   3rd Qu.:0    3rd Qu.:0.1509   3rd Qu.:0.1674   3rd Qu.:0.1517  
#>  Max.   :0   Max.   :0    Max.   :0.2339   Max.   :0.4326   Max.   :0.3677  
#>                           NA's   :1554     NA's   :686      NA's   :1736    
#>       rail        
#>  Min.   :-0.3028  
#>  1st Qu.:-0.2060  
#>  Median :-0.1340  
#>  Mean   :-0.1434  
#>  3rd Qu.:-0.0723  
#>  Max.   :-0.0022  
#>  NA's   :882


# ################################################################# #
#### ANALYSIS OF CHOICES                                         ####
# ################################################################# #

choiceAnalysis_settings <- list(
  alternatives = c(A=1, B=2),
  avail        = 1,
  choiceVar    = database$forced,
  explanators  = database[,c("risk_score","loss_score","intrinsic","extrinsic", "pain_NRS", "function_NRS",
                             "IPAQ_cat", 
                             "gender", "age", "BMI_calc", "income")])


apollo_choiceAnalysis(choiceAnalysis_settings, apollo_control, database)

# ################################################################# #
#### DEFINE MODEL PARAMETERS                                     ####
# ################################################################# #

### Vector of parameters, including any that are kept fixed in estimation
apollo_beta = c(
  asc = 0,
  b_60min=0,
  b_90min=0,
  b_voucher=0,
  b_donate=0,
  b_300=0,
  b_500=0,
  b_neg=0)

apollo_fixed<-c()

# ################################################################# #
#### GROUP AND VALIDATE INPUTS                                   ####
# ################################################################# #

apollo_inputs = apollo_validateInputs()

# ################################################################# #
#### DEFINE MODEL AND LIKELIHOOD FUNCTION                        ####
# ################################################################# #

apollo_probabilities=function(apollo_beta, apollo_inputs, functionality="estimate"){
  
  ### Attach inputs and detach after function exit
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  
  ### Create list of probabilities P
  P = list()
  
  ### List of utilities: these must use the same names as in el_settings, order is irrelevant
  V = list()
  V[['alt1']] = ( b_brand_Artemis*(brand_1=="Artemis") + b_brand_Novum*(brand_1=="Novum") 
                  + b_country_CH*(country_1=="Switzerland") + b_country_DK*(country_1=="Denmark") + b_country_USA*(country_1=="USA") 
                  + b_char_standard*(char_1=="standard") + b_char_fast*(char_1=="fast acting") + b_char_double*(char_1=="double strength") 
                  + b_risk*side_effects_1
                  + b_price*price_1)
  V[['alt2']] = ( b_brand_Artemis*(brand_2=="Artemis") + b_brand_Novum*(brand_2=="Novum") 
                  + b_country_CH*(country_2=="Switzerland") + b_country_DK*(country_2=="Denmark") + b_country_USA*(country_2=="USA") 
                  + b_char_standard*(char_2=="standard") + b_char_fast*(char_2=="fast acting") + b_char_double*(char_2=="double strength") 
                  + b_risk*side_effects_2
                  + b_price*price_2)
  V[['alt3']] = ( b_brand_BestValue*(brand_3=="BestValue") + b_brand_Supermarket*(brand_3=="Supermarket") + b_brand_PainAway*(brand_3=="PainAway") 
                  + b_country_USA*(country_3=="USA") + b_country_IND*(country_3=="India") + b_country_RUS*(country_3=="Russia") + b_country_BRA*(country_3=="Brazil") 
                  + b_char_standard*(char_3=="standard") + b_char_fast*(char_3=="fast acting") 
                  + b_risk*side_effects_3
                  + b_price*price_3 )
  V[['alt4']] = ( b_brand_BestValue*(brand_4=="BestValue") + b_brand_Supermarket*(brand_4=="Supermarket") + b_brand_PainAway*(brand_4=="PainAway") 
                  + b_country_USA*(country_4=="USA") + b_country_IND*(country_4=="India") + b_country_RUS*(country_4=="Russia") + b_country_BRA*(country_4=="Brazil") 
                  + b_char_standard*(char_4=="standard") + b_char_fast*(char_4=="fast acting") 
                  + b_risk*side_effects_4
                  + b_price*price_4 )
  
  ### Define settings for exploded logit
  el_settings = list(
    alternatives = c(alt1=1, alt2=2, alt3=3, alt4=4),
    avail        = list(alt1=1, alt2=1, alt3=1, alt4=1),
    choiceVars   = list(best, second_pref, third_pref),
    V            = V,
    scales       = list(1,scale_2,scale_3)
  )
  
  ### Compute exploded logit probabilities
  P[["model"]]=apollo_el(el_settings, functionality)
  
  ### Take product across observation for same individual
  P = apollo_panelProd(P, apollo_inputs, functionality)
  
  ### Prepare and return outputs of function
  P = apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ################################################################# #
#### MODEL ESTIMATION                                            ####
# ################################################################# #

model = apollo_estimate(apollo_beta, apollo_fixed, apollo_probabilities, apollo_inputs)

# ################################################################# #
#### MODEL OUTPUTS                                               ####
# ################################################################# #

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO SCREEN)                               ----
# ----------------------------------------------------------------- #

apollo_modelOutput(model)

# ----------------------------------------------------------------- #
#---- FORMATTED OUTPUT (TO FILE, using model name)               ----
# ----------------------------------------------------------------- #

apollo_saveOutput(model)


library(mlogit)
library(dplyr)

#import data
data <- read_csv(here("01_data","02_processed","DCE_forced_data_processed.csv"))
#View(data)

#import data from one block
#data1<-read_csv("01_data/02_processed/DCE_long1.csv")


data<-data%>%
  select(Participant,Version, Trial, Selection, Alt, Goal, Form, Magnitude, Direction)


#use mutate to deal w/ this
data$Participant <- factor(as.numeric(as.factor(data$Participant)))
data$Alt <- as.factor(as.character(data$Alt))
data$Goal <- as.factor(data$Goal)
data$Form <- as.factor(data$Form)
data$Magnitude <- as.factor(data$Magnitude)
data$Direction<-as.factor(data$Direction)

#str(data)
#$ Participant: Factor w/ 303 levels "1","2","3","4",..: 2 2 2 2 2 2 2 2 2 2 ...
#$ Version    : num  1 1 1 1 1 1 1 1 1 1 ...
#$ Trial      : num  1 1 2 2 3 3 4 4 5 5 ...
#$ Selection  : num  0 1 1 0 0 1 1 0 0 1 ...
#$ Alt        : Factor w/ 2 levels "1","2": 1 2 1 2 1 2 1 2 1 2 ...
#$ Goal       : Factor w/ 3 levels "30min","60min",..: 3 2 3 2 1 3 1 2 3 1 ...
#$ Form       : Factor w/ 3 levels "Cash","Donate",..: 2 1 1 2 2 1 1 2 3 1 ...
#$ Magnitude  : Factor w/ 3 levels "160","300","500": 1 3 3 2 1 2 3 2 1 2 ...
#$ Direction  : Factor w/ 2 levels "neg","pos": 1 2 1 2 2 1 2 1 2 1 ...

#use m.logit.data() to convert df to to mlogit.data
model1<-mlogit.data(data,
                    choice="Selection", 
                    shape="long",
                    alt.var = "Alt",
                    id.var="Participant",
                    varying=c(6:9))

#View(model1)

m1<-mlogit(Selection~1| Goal+Form+Magnitude+Direction, data=model1)

coef(summary(m1))
#                 Estimate Std. Error    z-value     Pr(>|z|)
#2:(intercept)   1.4232904  0.1423197  10.000656 0.000000e+00
#2:Goal60min    -0.2281006  0.1245599  -1.831253 6.706285e-02
#2:Goal90min    -0.9067820  0.1295098  -7.001648 2.529754e-12
#2:FormDonate   -2.8805924  0.1318348 -21.850012 0.000000e+00
#2:FormVoucher  -1.0003038  0.1302727  -7.678539 1.598721e-14
#2:Magnitude300  0.4700653  0.1375929   3.416348 6.346698e-04
#2:Magnitude500  0.9261114  0.1243945   7.444952 9.703349e-14
#2:Directionpos -0.1712390  0.1028259  -1.665329 9.584709e-02

m2<-mlogit(Selection~Goal+Form+Magnitude+Direction|-1, data=model1)
coef(summary(m2))
#              Estimate Std. Error  z-value  Pr(>|z|)    
#Goal60min    -0.231981   0.073939  -3.1375  0.001704 ** 
#Goal90min    -0.847117   0.072948 -11.6127 < 2.2e-16 ***
#FormDonate   -1.891804   0.081918 -23.0937 < 2.2e-16 ***
#FormVoucher  -0.506060   0.068249  -7.4149 1.217e-13 ***
#Magnitude300  0.550913   0.077137   7.1420 9.199e-13 ***
#Magnitude500  0.763663   0.071109  10.7394 < 2.2e-16 ***
#Directionpos  0.052338   0.051397   1.0183  0.308537    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#Log-Likelihood: -1190.3

#mlogit(formula = Selection ~ Goal + Form + Magnitude + Direction | 
-1, data = model1, method = "nr")

Frequencies of alternatives:
  1     2 
0.467 0.533 

nr method
5 iterations, 0h:0m:0s 
g'(-H)^-1g = 0.00176 
successive function values within tolerance limits 




######################################
#adding dummies
#https://stats.stackexchange.com/questions/24365/why-do-i-get-exact-singularity-when-i-add-a-dummy-variable-to-a-multinomial-logi


coef(m2)[- 1] / coef(m2)[1]
#Goal90min   FormDonate  FormVoucher Magnitude300 Magnitude500 Directionpos 
#3.651668     8.155003     2.181473    -2.374823    -3.291924    -0.225613 

#mixed logit
pilot1_mx1<-mlogit(Selection~Goal+Form+Magnitude|-1, pilot1,
                     rpar=c(Goal85min='n',FormDonate='n',FormVoucher='n',
                            Magnitude160neg='n',Magnitude160pos='n',Magnitude300neg='n',Magnitude300pos='n',
                            Magnitude500neg='n',Magnitude500pos='n'),
                            R=100, halton=NA, panel=TRUE, print.level=1)
#Error in as.data.frame.default(data) : cannot coerce class '"mlogit"' to a data.frame
#Error in mlogit.start(formula = formula, data = data, mf = mf, start = start,  : 
#unknown random parameter

pilot1_mx1<-mlogit(Selection~Goal+Form+Magnitude, pilot1,
                   rpar=c(Goal='n',Form='n'),
                   R=500, halton=NA, panel=TRUE, print.level=1)

Error in relevel.factor(index$alt, reflevel) : 
  'ref' must be an existing level

head(index(pilot1_model))



------
predict.mnl <- function(model, data) {
  # Function for predicting shares from a multinomial logit model 
  # model: mlogit object returned by mlogit()
  # data: a data frame containing the set of designs for which you want to 
  #       predict shares.  Same format at the data used to estimate model. 
  data.model <- model.matrix(update(model$formula, 0 ~ .), data = data)[,-1]
  utility <- data.model%*%model$coef
  share <- exp(utility)/sum(exp(utility))
  cbind(share, data)
}

preference<-predict.mnl(pilot1,pilot_test)

summary(pilot1_model)
#Call:
#  mlogit(formula = Selection ~ Goal + Form + Magnitude, data = pilot1, 
#         method = "nr")

#Frequencies of alternatives:
#  1   2 
#0.5 0.5 

#nr method
#2 iterations, 0h:0m:0s 
#g'(-H)^-1g =  68.8 
#last step couldn't find higher value 

#Coefficients :
#  Estimate Std. Error z-value  Pr(>|z|)    
#2:(intercept)   -0.33021    0.15485 -2.1325 0.0329668 *  
#  Goal60min        0.38602    0.21540  1.7921 0.0731186 .  
#Goal85min        0.38856    0.24622  1.5781 0.1145462    
#FormDonate       0.72419    0.25031  2.8932 0.0038131 ** 
#FormVoucher      0.76996    0.20668  3.7253 0.0001951 ***
#Magnitude160neg  4.39824    0.88121  4.9911 6.003e-07 ***
#Magnitude160pos  2.29407    0.79419  2.8886 0.0038700 ** 
#Magnitude300neg  3.45416    0.83941  4.1150 3.872e-05 ***
#Magnitude300pos  2.81138    0.75343  3.7314 0.0001904 ***
#Magnitude500neg  3.00609    0.83071  3.6187 0.0002961 ***
#Magnitude500pos  4.60666    0.95201  4.8389 1.306e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Log-Likelihood: -183.84
#McFadden R^2:  0.12176 
#Likelihood ratio test : chisq = 50.976 (p.value = 1.7647e-07)


m1=mlogit(Selection~0+Goal+Form+Magnitude+Direction, data=data, print.level=3)
#Error in data.frame(lapply(index, function(x) x[drop = TRUE]), 
#row.names = rownames(mydata)) : 
#row names supplied are of the wrong length
summary(m1)

model1<-mlogit(Selection ~ Goal + Form + Magnitude + Direction, data, 
               chid.var = "Participant", 
               alt.var = "Alt", 
               choice = "Selection", 
               shape = "long")


# # Source: https://cran.r-project.org/web/packages/apollo/vignettes/apollofirstexample.html
#A model file should be structured around the following sections:
# #   
# # 1) Definition of core settings: In this section the Apollo library is loaded, 
# and global variables such as the name of the model are defined.
# # 
# # 2) Data loading: In this section the database is loaded. Any transformation to the data that 
# does not depend on parameters to be estimated (e.g. averages of explanatory variables) 
# should be performed here and stored inside the database object.
# # 
# # 3) Parameter definition: In this section, a vector containing the names and starting 
# values of parameters is defined, as well as details of their mixing distributions if present.
# # 
# # 4) Input validation: In this section, all inputs prepared in the previous sections are validated and stored together.
# # 
# # 5) Likelihood definition: In this section, the user must write a function that calculates the model likelihood.
# # 
# # 6) Model estimation and reporting: In this section, the model is estimated and the results are displayed.
# # 
# # 7) Postprocessing of results: This is an optional section where the user can do 
# forecasting and other post-processing of the estrimated model.

