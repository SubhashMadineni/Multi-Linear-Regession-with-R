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
#----------------getting ISLR Package and the Auto Autoset in it-----------#
#install.packages("ISLR")
library("ISLR")

?Auto          ##to get details of Auto Autoset

#Auto <- Auto  ##storing Auto Autoset in "Auto" variable

#####investigating the Autoset by plotting he Auto####


#scatterplot between mpg and year
plot(Auto$year,Auto$mpg, xlab = 'Year',
     ylab = " miles per gallon", main = "mileage", type  = 'l' )

#scatterplot between mpg and year
plot(Auto$cylinders,Auto$mpg, xlab = 'No of Cylienders',
     ylab = " MPG", main = "mpg vs Cylienders" )

#scatterplot between mpg and year
plot(Auto$cylinders,Auto$displacement,
     xlab = 'No of Cylienders',
     ylab = " displacement",
     main = "cylienders vs displacement" )


#Barcharts between mpg and year
plot(Auto$cylinders,Auto$mpg, xlab = 'No of Cylienders',
     ylab = " MPG", main = "mpg vs Cylienders" )
##box plot of count of cyliners
count <- table(Auto$cylinders)
barplot(count, xlab = " No of Cylienders", ylab = "Count", 
        main= " Count for no of cylienders",
        legend = rownames(count),col = c("black", "red"))



########creating pie chart
slices <- table(Auto$origin)
lbls = c('Origin 1', "Origin 2", "Origin3")
percentage = round(slices/sum(slices) * 100)
lbls = paste(lbls, "", percentage, '%', seq = "")
pie3D(slices, labels = lbls,
      main = "Pie chart of Origin Count")

#-----------------Cleaning the categorical Auto----------------#


#Removing the Name colums as it serves no Purpose
Auto <- Auto[,-9]

library(caTools)
set.seed(432)
split = sample.split(Auto$mpg, SplitRatio = 0.8)

Training_set <- subset(Auto, split = TRUE)
Test_set <- subset(Auto, split = FALSE)

##feature scaling the Auto Science

Training_set[,] <- scale(Training_set[,])
Test_set[,] = scale(Test_set[,])


###Writing the data into a file
write.table(Auto, file = 'Auto_data_Procesed.txt', sep = '\t',row.names = TRUE)



#------------Applying Multi-linear Regression to the Auto-------#######
regressor = lm(formula = mpg ~.,data = Training_set)


summary(regressor)
##predicting the test set
y_pred = predict(regressor, newdata = Test_set)
y_pred

