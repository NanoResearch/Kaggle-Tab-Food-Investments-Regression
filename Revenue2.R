library("dplyr")
library("ggvis")
library("data.table")
library("psych")
library("randomForest")
library("caret")
library("ggplot2")
library("corrplot")
library("lattice")
library("DBI")
library("rJava")
library("lubridate")
library(e1071)

install.packages("installr")
setInternet2(TRUE)
installr::updateR()

#Model2 is svm model, MY best model so far. Extracted month and day of week
##### UPDATE = 1/12/2015 - Model 4 is the best model. Samme as Model 2 with certain variables removed

#Load Train data into Train1 variable
Train1 <- read.csv("train.csv", stringsAsFactors = F) # READ DATA FILE. NO FACTORS!

str(Train1) # Check the contents of the data.frame
describe(Train1) #Descp stats of the data frame

Train2 <- Train1 %>% select(-Id) # Remove Id
Train2$Open.Date <- mdy(Train1$Open.Date) # Convert date to POXIsct type
Train2$Dow <- wday(Train2$Open.Date) # Day of week
Train2$Month <- month(Train2$Open.Date) # Month

str(Train2) 
Train2$Open.Date <- as.Date(Train2$Open.Date) #convert to date type
Train2$Open.Date <- Sys.Date() - Train2$Open.Date #Covnert to age
Train2$Open.Date <- as.numeric(Train2$Open.Date) #Convert to number

Train3 <- Train2 %>% select(-City,-City.Group, -Type) # Remove City, City.Group and Type, basically all factors
Train3$Month <- as.factor(Train3$Month) #Month to categorical variable
Train3$Dow <- as.factor(Train3$Dow) #Date of week to categorical variable

corrplot(cor(Train3), method = "pie") #Correlation matrix

model1 <- randomForest(revenue ~ . , data = Train3) #random Forest model
model2 <- svm(revenue ~ . ,data = Train3, type = "nu-regression") #SVM model nu-regression
model3 <- svm(revenue ~ . ,data = Train3, type = "eps-regression") #SVM model eps-regression, nu is better

Train4 <- Train3 %>% select(-P31, -P30, -P35, -P37, -P7) #From Revenue1.R , multicollinearity elimination ###TEST MODEL

model4 <- svm(revenue ~ ., data = Train4, type = "nu-regression") #SVM model with no correlation variables ##TEST MODEL

#Do NOT TOUCH ANYTHING UNTIL BEFORE POINT!!! Model 2 is the best model

Train4 <- as.data.table(Train3) #convert to data table
Train4 <- Train4[(revenue - mean(revenue))/sd(revenue) <= 3,]
Train4 <- Train4[, revenue := sqrt(revenue)] #sqrt transform revenue


model4 <-svm(revenue ~ . ,data = Train4, type = "nu-regression") #SVM model nu-regression

#Model4 is an okay model, but not better than model2.

Train5 <- cbind(Train3,Train1$City.Group) #include city group
Train5 <- as.data.table(Train5) # Convert to data table, because data table is awesome
Train5 <- Train5[(revenue - mean(revenue))/sd(revenue) <= 3,] #Remove outliers
setnames(Train5, "Train1$City.Group", "City.Group") #Change variable name for obvious reasons
Traintemp <- Train5[,.(Open.Date,P2,P3,P4,P13,P26,P27,P28,P29, City.Group, revenue, Dow, Month)] #Select all other variables
Train5 <- Train5[,lapply(.SD,as.factor), .SDcols = paste0("P",c(1,5,6,7,8,9,10,11,12,14:25,30:37))] #Factorize other variables
Train5 <- cbind(Traintemp,Train5)


str(Train5)

boxplot(Train5$P2) #outliers
boxplot(Train5$P3) #outliers
boxplot(Train5$P4)
boxplot(Train5$P14)
boxplot(Train5$P26)
boxplot(Train5$P27)
boxplot(Train5$P28)
boxplot(Train5$P29)

model5 <- svm(sqrt(revenue) ~ . ,data = Train5) #SVM model nu-regression


#======================================================================================================
                                                  #TEST ENVIRONMENT
#======================================================================================================

#Load Test data into Test1 variable
Test1 <- read.csv("Test.csv", stringsAsFactors = F) # READ DATA FILE. NO FACTORS!

str(Test1) # Check the contents of the data.frame
describe(Test1) #Descp stats of the data frame

Test2 <- Test1 %>% select(-Id) # Remove Id
Test2$Open.Date <- mdy(Test1$Open.Date) # Convert date to POXIsct type
Test2$Dow <- wday(Test2$Open.Date) # Day of week
Test2$Month <- month(Test2$Open.Date) # Month

str(Test2) 
Test2$Open.Date <- as.Date(Test2$Open.Date) #convert to date type
Test2$Open.Date <- Sys.Date() - Test2$Open.Date #Covnert to age
Test2$Open.Date <- as.numeric(Test2$Open.Date) #Convert to number

Test3 <- Test2 %>% select(-City,-City.Group, -Type) # Remove City, City.Group and Type, basically all factors
Test3$Month <- as.factor(Test3$Month) #Month to categorical variable
Test3$Dow <- as.factor(Test3$Dow) #Date of week to categorical variable

str(Test3)
str(Train3)

Test3$revenue <- predict(model3, Test3) # Prediction

write.csv(Test3$revenue, "S3.csv") # Write to csv

#Do NOT TOUCH ANYTHING UNTIL THIS POINT!!! Model 2 is the best model

Test4 <- as.data.table(Test3) #convert to data table

Test4 <- Test_3 %>% select(-P31, -P30, -P35, -P37, -P7) # FROM Revenue1.R, remove multicollinearity #### TEST MODEL

Test4$revenue <- predict(model4, Test4) # Predict Model 4 with eliminated variables. #### TEST MODEL
write.csv(Test4$revenue, "Test_new.csv") # Write out the prediction to a csv file #### TEST MODEL

Test4$revenue <- predict(model4, Test4)
Test4$revenue <- Test4$revenue^2
write.csv(Test4$revenue, "S4.csv")

Test5 <- cbind(Test3,City.Group = Test1$City.Group) #include city group
Test5 <- as.data.table(Test5) # Convert to data table, because data table is awesome
Testtemp <- Test5[,.(Open.Date,P2,P3,P4,P13,P26,P27,P28,P29, City.Group, Dow, Month)] #Select all other variables
Test5 <- Test5[,lapply(.SD,as.factor), .SDcols = paste0("P",c(1,5,6,7,8,9,10,11,12,14:25,30:37))] #Factorize other variables
Test5 <- cbind(Testtemp,Test5)

str(Test5)
str(Train5)

Test5$revenue <- 0
Test5$revenue <- predict(model5, Test5)

Test5$revenue <- Test5$revenue^2

write.csv(Test5$revenue,"S5.csv")
