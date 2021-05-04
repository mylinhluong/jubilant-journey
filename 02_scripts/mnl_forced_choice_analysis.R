library(mlogit)
library(readr)
library(here)
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
