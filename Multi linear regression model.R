####################################################
## Auto Mining I HW1
## Subhashchandra Babu Madineni   UBIT = 50373860
## Created on 14th Sept
## Edited: 
#################################################################

### Cleaning The Environment#####################################
rm(list= ls())
setwd("C:/University At Buffalo Fall 2020 Classes/EAS 506-CDA 541Stastical Data Mining/R for STA")

#install.packages('plotrix')
library('plotrix')
#----------------getting ISLR Package and the Auto dataset in it-----------#
#install.packages("ISLR")
library("ISLR")

#-----------------Cleaning the categorical Auto----------------#


#Removing the Name colums as it serves no Purpose
Auto <- Auto[,-9]

library(caTools)
set.seed(432)
split <- sample.split(mpg, SplitRatio = 0.8)

Training_set <- subset(Auto, split = TRUE)
Test_set <- subset(Auto, split = FALSE)

##feature scaling the Auto Science

Training_set[,] <- scale(Training_set[,])
Test_set[,] = scale(Test_set[,])


#------------Applying Multi-linear Regression to the Auto-------#######
#regressor = lm(formula = Training_set$mpg ~ 
#                Training_set$cylinders  + Training_set$displacement +Training_set$horsepower+Training_set$weight+Training_set$acceleration+Training_set$year+Training_set$origin,
#             data = Training_set)
regressor = lm(formula = mpg ~.,data = Training_set)

##predicting the test set
pred = predict(regressor, newdata = Test_set)





