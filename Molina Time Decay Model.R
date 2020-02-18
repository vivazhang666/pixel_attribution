#install necessary packages
if(!"pacman" %in% installed.packages()) install.packages("pacman")

pacman::p_load(dplyr, data.table, fst, 
               tictoc,tidyverse, dummies,corrplot,caret,
               ggplot2,ROCR,Metrics,car,purrr,haven,rpart,rpart.plot,DMwR,ade4,tidyverse,broom,caTools)

#############Molina Pandora##################
all <-read.csv("C:/Users/yzhang/Desktop/Molina/step2_pandora_ready_for_modeling.csv",check.names=F,na.strings=c("",NA),stringsAsFactors = F)

##Data Exploration
#Remove NAs and filter
all_new <- na.omit(all)
#plot three goals for veritone attribution
qplot(time_difference_second/(60*60*24), percent_non_convert, data = all_new,xlab = 'time_difference_in_days')

##Names
# [1] "order_id"               "placement"              "time_difference_second"
# [4] "month"                  "state"                  "type"                  
# [7] "creative_name_clean"    "audience"               "gender"                
# [10] "age_group"              "device"                 "language"              
# [13] "conversion"             "total_conversion"       "percent_convert"       
# [16] "percent_non_convert"    "time_difference_min"    "time_difference_day"  

##Model Fitting
# pick variables
#- predictors:month,state,type,audience,gender,age_group,device,language,time_difference_second
#- target variable: percent_non_convert


#include all variables
model1 <- all_new%>%
  select(month,state,type,audience,gender,age_group,device,language,time_difference_second,percent_non_convert)%>%
  filter(percent_non_convert != 0)%>%
  mutate( month = as.factor(month),
                 state = as.factor(state),
                 type = as.factor(type),
                 audience = as.factor(audience),
                 gender = as.factor(gender),
                 age_group = as.factor(age_group),
                 device = as.factor(device),
                 language = as.factor(language))
model1$month <- relevel(model1$month, ref = "12")
model1$type <- relevel(model1$type, ref = "Display")
model1$language <- relevel(model1$language, ref = "Spanish")

#split train and test 
set.seed(111)
sample1<- sample.split(model1,SplitRatio = 0.8)
train1 <- subset(model1,sample1 ==TRUE)
test1 <- subset(model1,sample1 ==FALSE)

#run regression with all variables in
fit1.1 <-lm(log(percent_non_convert) ~ month+state+type+gender+age_group+device+language+time_difference_second , data = train1)
summary(fit1.1)
#regrouping variables
model1<- model1%>%
         mutate( age_group_new = ifelse(age_group == "55-64", "55-64","18-54"))

# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.05287 -0.01592 -0.00039  0.01529  0.08884 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)            -4.508e-02  4.606e-03   -9.787  < 2e-16 ***
#   month11                -8.621e-03  1.651e-03   -5.220 2.24e-07 ***
#   stateMississippi       -1.822e-02  2.691e-03   -6.771 2.37e-11 ***
#   stateSouth Carolina    -1.130e-02  2.932e-03   -3.854 0.000125 ***
#   typeAudio              -1.588e-02  2.869e-03   -5.534 4.17e-08 ***
#   genderM                -8.893e-04  1.700e-03   -0.523 0.600993    
# age_group25-30          3.355e-03  2.550e-03    1.316 0.188623    
# age_group31-34         -4.493e-04  3.421e-03   -0.131 0.895539    
# age_group35-44         -5.632e-03  2.613e-03   -2.156 0.031395 *  
#   age_group45-54          7.943e-03  2.849e-03    2.788 0.005424 ** 
#   age_group55-64         -3.110e-03  3.125e-03   -0.995 0.319888    
# deviceAV               -1.160e-02  3.923e-03   -2.957 0.003194 ** 
#   deviceiOS               1.204e-02  1.894e-03    6.359 3.29e-10 ***
#   languageEnglish        -4.062e-03  2.219e-03   -1.831 0.067502 .  
# time_difference_second -4.390e-06  3.032e-08 -144.793  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.0233 on 856 degrees of freedom
# Multiple R-squared:  0.964,	Adjusted R-squared:  0.9634 
# F-statistic:  1638 on 14 and 856 DF,  p-value: < 2.2e-16

#perform feature selection to see variable importance
varImp(fit1.1, scale = FALSE)
# varImp(fit1.1, scale = FALSE)
# Overall
# month11                  5.2203294
# stateMississippi         6.7713946
# stateSouth Carolina      3.8539673
# typeAudio                5.5338082
# genderM                  0.5231681
# age_group25-30           1.3157120
# age_group31-34           0.1313382
# age_group35-44           2.1555721
# age_group45-54           2.7878225
# age_group55-64           0.9952663
# deviceAV                 2.9567445
# deviceiOS                6.3593143
# languageEnglish          1.8306377
# time_difference_second 144.7927958

#run model again after removing statistically unsignificant variables 
fit1.2 <-lm(log(percent_non_convert) ~ time_difference_second+month+state+type+language , data = train1)
summary(fit1.2)

# Call:
#   lm(formula = log(percent_non_convert) ~ time_difference_second + 
#        month + state + type + language, data = train1)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.045917 -0.018182 -0.000939  0.017851  0.074403 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)            -4.682e-02  2.996e-03  -15.627  < 2e-16 ***
#   time_difference_second -4.424e-06  3.143e-08 -140.759  < 2e-16 ***
#   month11                -8.863e-03  1.707e-03   -5.192 2.60e-07 ***
#   stateMississippi       -1.474e-02  2.414e-03   -6.105 1.55e-09 ***
#   stateSouth Carolina    -7.444e-03  2.883e-03   -2.582  0.00999 ** 
#   typeAudio              -8.913e-03  2.112e-03   -4.221 2.69e-05 ***
#   languageEnglish        -4.618e-03  2.284e-03   -2.022  0.04346 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.02479 on 864 degrees of freedom
# Multiple R-squared:  0.9589,	Adjusted R-squared:  0.9586 
# F-statistic:  3357 on 6 and 864 DF,  p-value: < 2.2e-16

###apply model on test data
#model 1.1
p1.1 <- predict(fit1.1, test1)
#calculate prediction accuracy and error rates
actual1.1 <- cbind(actuals=test1$percent_non_convert,predicteds = p1.1)
correlation_accuracy1.1 <- cor(actual1.1)
correlation_accuracy1.1 #97.1%
# actuals predicteds
# actuals    1.0000000  0.9706755
# predicteds 0.9706755  1.000000

#model 1.2
p1.2 <- predict(fit1.2, test1)
#calculate prediction accuracy and error rates
actual1.2 <- cbind(actuals=test1$percent_non_convert,predicteds = p1.2)
correlation_accuracy1.2 <- cor(actual1.2)
correlation_accuracy1.2#96.7%
# actuals predicteds
# actuals    1.0000000  0.9661025
# predicteds 0.9661025  1.0000000


#############Molina Audiology##################

all2 <-read.csv("C:/Users/yzhang/Desktop/Molina/step2_audilogy_ready_for_modeling.csv",check.names=F,na.strings=c("",NA),stringsAsFactors = F)

##Data Exploration
#Remove NAs and filter
all_new2 <- na.omit(all2)
#plot three goals for veritone attribution
qplot(time_difference_second/(60*60*24), percent_non_convert, data = all_new2,xlab = 'time_difference_in_days')

##Names
# [1] "audience"               "conversion"             "device"                
# [4] "language"               "month"                  "order_id"              
# [7] "percent_convert"        "percent_non_convert"    "placement"             
# [10] "state"                  "time_difference_day"    "time_difference_min"   
# [13] "time_difference_second" "total_conversion"       "within_15_min" 

##Model Fitting
# two models: 1. time decay model on samples have time_difference_min <=15min
             #2. Linear model on samples have time_difference_min > 15min

# pick variables
#- predictors:month,state,device, language, audience, time_difference_second
#- target variable: percent_non_convert
#include all variables
model2 <- all_new2%>%
  select(month,state,audience,device,language,time_difference_second,percent_non_convert, within_0.25_day)%>%
  filter(percent_non_convert != 0 & within_0.25_day == "True")%>%
  mutate( month = as.factor(month),
          state = as.factor(state),
          audience = as.factor(audience),
          device = as.factor(device),
          language = as.factor(language))
model2$month <- relevel(model2$month, ref = "11")
model2$language <- relevel(model2$language, ref = "English")


#split train and test 
set.seed(123)
sample2<- sample.split(model2,SplitRatio = 0.8)
train2 <- subset(model2,sample2 ==TRUE)
test2 <- subset(model2,sample2 ==FALSE)

#fit_model
fit2.1 <-lm(log(percent_non_convert) ~ time_difference_second+month+state+device+audience+language, data = train2)
summary(fit2.1)

# Call:
#   lm(formula = log(percent_non_convert) ~ time_difference_second + 
#        month + state + device + audience + language, data = train2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.192876 -0.028615 -0.001549  0.043183  0.069383 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)            -1.158e-02  3.886e-03   -2.980  0.00293 ** 
#   time_difference_second -8.595e-05  2.192e-07 -392.117  < 2e-16 ***
#   month11                 3.102e-03  2.716e-03    1.142  0.25371    
# stateWashington        -5.469e-03  3.127e-03   -1.749  0.08055 .  
# deviceDesktop          -5.161e-03  4.016e-03   -1.285  0.19889    
# deviceiOS              -8.623e-04  2.929e-03   -0.294  0.76847    
# audienceInMarket       -1.133e-04  2.663e-03   -0.043  0.96606    
# languageEnglish         5.643e-03  3.399e-03    1.660  0.09718 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04767 on 1294 degrees of freedom
# Multiple R-squared:  0.9922,	Adjusted R-squared:  0.9922 
# F-statistic: 2.364e+04 on 7 and 1294 DF,  p-value: < 2.2e-16

#variable importance
varImp(train2, scale = FALSE)
# Overall
# time_difference_second 392.11664464
# month12                  1.14188382
# stateWashington          1.74886238
# deviceDesktop            1.28536673
# deviceiOS                0.29443247
# audienceInMarket         0.04255636
# languageSpanish          1.65986969

#regrouping variables
train2<- train2%>%
  mutate( device_new = ifelse(device == "iOS" | device == "Android", "mobile", "desktop"))
train2$device_new<- factor(train2$device_new)
train2$device_new <- relevel(train2$device_new, ref = "mobile")

test2<- test2%>%
  mutate( device_new = ifelse(device == "iOS" | device == "Android", "mobile", "desktop"))
test2$device_new<- factor(test2$device_new)

fit2.2 <-lm(log(percent_non_convert) ~ time_difference_second+month+state+device_new+audience+language, data = train2)
summary(fit2.2)
# 
# Call:
#   lm(formula = log(percent_non_convert) ~ time_difference_second + 
#        month + state + device_new + audience + language, data = train2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.193246 -0.028809 -0.001438  0.043265  0.069068 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)            -3.242e-03  3.758e-03   -0.863   0.3885    
# time_difference_second -8.595e-05  2.190e-07 -392.549   <2e-16 ***
#   month12                -3.146e-03  2.711e-03   -1.160   0.2462    
# stateWashington        -5.544e-03  3.116e-03   -1.779   0.0754 .  
# device_newdesktop      -4.723e-03  3.729e-03   -1.267   0.2054    
# audienceInMarket       -7.427e-05  2.659e-03   -0.028   0.9777    
# languageSpanish        -5.610e-03  3.396e-03   -1.652   0.0989 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04765 on 1295 degrees of freedom
# Multiple R-squared:  0.9922,	Adjusted R-squared:  0.9922 
# F-statistic: 2.76e+04 on 6 and 1295 DF,  p-value: < 2.2e-16

#final model after applying model selection
fit2.3 <-lm(log(percent_non_convert) ~ time_difference_second, data = train2)
summary(fit2.3)

# Call:
#   lm(formula = log(percent_non_convert) ~ time_difference_second, 
#      data = train2)
# 
# Residuals:
#   Min        1Q    Median        3Q       Max 
# -0.193429 -0.028901 -0.001511  0.043805  0.066546 
# 
# Coefficients:
#   Estimate Std. Error  t value Pr(>|t|)    
# (Intercept)            -9.541e-03  2.124e-03   -4.492 7.67e-06 ***
#   time_difference_second -8.591e-05  2.112e-07 -406.847  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.04767 on 1300 degrees of freedom
# Multiple R-squared:  0.9922,	Adjusted R-squared:  0.9922 
# F-statistic: 1.655e+05 on 1 and 1300 DF,  p-value: < 2.2e-16

###apply model on test data
#model 2.1 
p2.1 <- predict(fit2.1, test2)
#calculate prediction accuracy and error rates
actual2.1 <- cbind(actuals=test2$percent_non_convert,predicteds = p2.1)
correlation_accuracy2.1 <- cor(actual2.1) #98.17%
# actuals predicteds
# actuals    1.0000000  0.9816505
# predicteds 0.9816505  1.0000000

#model 2.2
p2.2 <- predict(fit2.2, test2)
#calculate prediction accuracy and error rates
actual2.2 <- cbind(actuals=test2$percent_non_convert,predicteds = p2.2)
correlation_accuracy2.2 <- cor(actual2.2) #98.15%
# actuals predicteds
# actuals    1.0000000  0.9815201
# predicteds 0.9815201  1.0000000

#model 2.3 
p2.3 <- predict(fit2.3, test2)
#calculate prediction accuracy and error rates
actual2.3 <- cbind(actuals=test2$percent_non_convert,predicteds = p2.3)
correlation_accuracy2.3 <- cor(actual2.3) #98.17%
# actuals predicteds
# actuals    1.0000000  0.9816505
# predicteds 0.9816505  1.0000000

#Part 2: fit model for data after 15min

#include all variables
model3 <- all_new2%>%
  select(month,state,audience,device,language,time_difference_second,percent_non_convert, within_0.25_day)%>%
  filter(percent_non_convert != 0 & within_0.25_day == "False")%>%
  mutate( month = as.factor(month),
          state = as.factor(state),
          audience = as.factor(audience),
          device = as.factor(device),
          language = as.factor(language))
model3$month <- relevel(model3$month, ref = "11")
model3$language <- relevel(model3$language, ref = "English")


#split train and test 
set.seed(456)
sample3<- sample.split(model3,SplitRatio = 0.8)
train3 <- subset(model3,sample3 ==TRUE)
test3 <- subset(model3,sample3 ==FALSE)

#fit_model
fit3.1 <-lm(percent_non_convert ~ time_difference_second+month+state+device+audience+language, data = train3)
summary(fit3.1)

# Call:
#   lm(formula = percent_non_convert ~ time_difference_second + month + 
#        state + device + audience + language, data = train3)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0073423 -0.0028166 -0.0001806  0.0032978  0.0078352 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.581e-01  1.570e-03 100.689   <2e-16 ***
#   time_difference_second -1.166e-06  2.037e-08 -57.250   <2e-16 ***
#   month12                -9.013e-04  7.928e-04  -1.137   0.2581    
# stateWashington         1.370e-03  8.601e-04   1.593   0.1142    
# deviceDesktop          -2.129e-03  1.174e-03  -1.813   0.0727 .  
# deviceiOS              -1.245e-03  1.312e-03  -0.949   0.3449    
# audienceInMarket       -1.020e-03  8.247e-04  -1.237   0.2187    
# languageSpanish        -2.376e-03  1.233e-03  -1.928   0.0565 .  
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.004027 on 106 degrees of freedom
# Multiple R-squared:  0.9721,	Adjusted R-squared:  0.9703 
# F-statistic: 528.3 on 7 and 106 DF,  p-value: < 2.2e-16

#variable importance
varImp(fit3.1, scale = FALSE)
# Overall
# time_difference_second 57.2504503
# month12                 1.1368881
# stateWashington         1.5927676
# deviceDesktop           1.8129151
# deviceiOS               0.9488023
# audienceInMarket        1.2373484
# languageSpanish         1.9280753

#regrouping variables
train3<- train3%>%
  mutate( device_new = ifelse(device == "iOS" | device == "Android", "mobile", "desktop"))
train3$device_new<- factor(train3$device_new)
train3$device_new <- relevel(train3$device_new, ref = "mobile")

test3<- test3%>%
  mutate( device_new = ifelse(device == "iOS" | device == "Android", "mobile", "desktop"))
test3$device_new<- factor(test3$device_new)

fit3.2 <-lm(percent_non_convert ~ time_difference_second+month+state+device_new+audience+language, data = train3)
summary(fit3.2)
# Call:
#   lm(formula = percent_non_convert ~ time_difference_second + month + 
#        state + device_new + audience + language, data = train3)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0073039 -0.0029976 -0.0002097  0.0032883  0.0077356 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.574e-01  1.363e-03 115.412   <2e-16 ***
#   time_difference_second -1.166e-06  2.036e-08 -57.278   <2e-16 ***
#   month12                -9.423e-04  7.912e-04  -1.191    0.236    
# stateWashington         1.305e-03  8.569e-04   1.523    0.131    
# device_newdesktop      -1.362e-03  8.519e-04  -1.599    0.113    
# audienceInMarket       -9.481e-04  8.208e-04  -1.155    0.251    
# languageSpanish        -2.557e-03  1.217e-03  -2.101    0.038 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.004025 on 107 degrees of freedom
# Multiple R-squared:  0.9719,	Adjusted R-squared:  0.9703 
# F-statistic: 616.8 on 6 and 107 DF,  p-value: < 2.2e-16

#final model after applying model selection
fit3.3 <-lm(percent_non_convert ~ time_difference_second+language, data = train3)
summary(fit3.3)

# Call:
#   lm(formula = percent_non_convert ~ time_difference_second + language, 
#      data = train3)
# 
# Residuals:
#   Min         1Q     Median         3Q        Max 
# -0.0085614 -0.0032414 -0.0001494  0.0035242  0.0074184 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)             1.567e-01  1.251e-03 125.275  < 2e-16 ***
#   time_difference_second -1.175e-06  1.976e-08 -59.462  < 2e-16 ***
#   languageSpanish        -3.197e-03  1.114e-03  -2.869  0.00493 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.004104 on 111 degrees of freedom
# Multiple R-squared:  0.9697,	Adjusted R-squared:  0.9692 
# F-statistic:  1776 on 2 and 111 DF,  p-value: < 2.2e-16

#apply model on test data


#model 3.1 
p3.1 <- predict(fit3.1, test3)
#calculate prediction accuracy and error rates
actual3.1 <- cbind(actuals=test3$percent_non_convert,predicteds = p3.1)
correlation_accuracy3.1 <- cor(actual3.1) #98.17%
# actuals predicteds
# actuals    1.0000000  0.9827121
# predicteds 0.9827121  1.0000000

#model 3.2
p3.2 <- predict(fit3.2, test3)
#calculate prediction accuracy and error rates
actual3.2 <- cbind(actuals=test3$percent_non_convert,predicteds = p3.2)
correlation_accuracy3.2 <- cor(actual3.2) #98.33%
# actuals predicteds
# actuals    1.0000000  0.9833755
# predicteds 0.9833755  1.0000000

#model 3.3 
p3.3 <- predict(fit3.3, test3)
#calculate prediction accuracy and error rates
actual3.3 <- cbind(actuals=test3$percent_non_convert,predicteds = p3.3)
correlation_accuracy3.3 <- cor(actual3.3) #98.40%
# actuals predicteds
# actuals    1.0000000  0.9839876
# predicteds 0.9839876  1.0000000

#Part 1: fit model for data within 15min



#model1
exp((-5.191e-02 )+1*(-4.459e-06)+1*(-7.568e-03)+0*(-1.526e-02)+0*(-4.436e-03)+0*(-5.855e-03)+0*(4.425e-03))*1.097

exp((-4.365e-02)+time_difference_second*(-4.459e-06)+month11*(-7.568e-03)+stateMississippi*(-1.526e-02)
    +stateSouth Carolina*(-7.832e-03)+typeAudio*(-7.378e-03)+languageSpanish*(-8.257e-03))*1.044

exp((-4.365e-02)+0*(-4.459e-06)+0*(-7.568e-03)+0*(-1.526e-02)+0*(-7.832e-03)+0*(-7.378e-03)+0*(-8.257e-03))*1.044

#model2_a
exp((-9.541e-03)+21600*(-8.591e-05))

#model3.3
(1.564e-01)+0*-1.171e-06+1*(-2.229e-03)


