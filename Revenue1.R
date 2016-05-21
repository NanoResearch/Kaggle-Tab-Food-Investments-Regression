
set.seed(13123)

Train_1 <- read.csv("train.csv", stringsAsFactors = FALSE)
Train_1$City <- as.factor(Train_1$City)
Train_1$City.Group <- as.factor(Train_1$City.Group)
Train_1$Type <- as.factor(Train_1$Type)
Train_2 <- as.data.table(Train_1)
Train_2 <- Train_2[, Id := NULL]

Test_1 <- read.csv("test.csv", stringsAsFactors = FALSE)
Test_1$City <- as.factor(Test_1$City)
Test_1$City.Group <- as.factor(Test_1$City.Group)
Test_1$Type <- as.factor(Test_1$Type)
Test_2 <- as.data.table(Test_1)
Test_2 <- Test_2[, Id := NULL]

Train_2$Open.Date <- as.Date(Train_2$Open.Date, format = "%m/%d/%y") - Sys.Date()
Train_2$Open.Date <- as.numeric(Train_2$Open.Date)
Test_2$Open.Date <- as.Date(Test_2$Open.Date, format = "%m/%d/%y") - Sys.Date()
Test_2$Open.Date <- as.numeric(Test_2$Open.Date)

Train_3 <- Train_2 %>% select(-City, -Type, -City.Group)
Test_3 <- Test_2 %>% select(-City, -Type, -City.Group)

model2 <- lm(revenue ~ . , data = Train_3)
model3 <- randomForest(revenue ~ . , data = Train_3)

#corrplot(cor(Train_3$P25, Train_3$P24), order = "hclust", bg = "black", title = "Mutlicollinearity Matrix", method = "shade")

Train_4 <- Train_3 %>% select(-P33, -P18, -P36, -P25, -P24, -P10, -P12, -P28, -P15)
Train_4 <- Train_4 %>% mutate(revenue = log(revenue)) #SQUARE ROOT TRANSFORM ON REVENUE
Test_4 <- Test_3 %>% select(-P33, -P18, -P36, -P25, -P24, -P10, -P12, -P28, -P15)

model4 <- lm(revenue~., data = Train_4)
model5 <- randomForest(revenue ~ . , data = Train_4)


#Train_5 <- Train_4 %>% select(-P34, -P31, -P32, -P16, -P9)
#Test_5 <- Test_4 %>% select(-P34, -P31, -P32, -P16, -P9)

Train_5 <- Train_3 %>% select(-P31, -P30, -P35, -P37, -P7)
Test_5 <- Test_3 %>% select(-P31, -P30, -P35, -P37, -P7)
model6 <- randomForest(revenue ~ . , Train_5)
model7 <- randomForest(sqrt(revenue) ~ . , Train_5)
model10 <- svm(revenue ~ . , data = Train_5)

Test_5$revenue <- predict(model6,Test_5)

#model5 is the best model so far, I tried removing a few more highly correlated variables in Train_5, didn't work)
#model5 is rank 1350, model6 is rank 894. I've managed to improve my models, note model6 doesn't originally include
#log transformation. I've done it as an experiment against sqrt transformation. Best model has no transformation. Surprise


Train_6 <- Train_2 %>% select(-P9, -P10, -P12, -P13, -P14, -P15, -P16, -P17, -P18,-P24, -P25, -P26, -P27, -P30,-P31, -P32,-P33, -P34, -P36, -P37, -City, -City.Group, -Type)
Test_6 <- Test_2 %>% select(-P9, -P10, -P12, -P13, -P14, -P15, -P16, -P17, -P18,-P24, -P25, -P26, -P27, -P30,-P31, -P32,-P33, -P34, -P36, -P37, -City, -City.Group, -Type)

Train_6 <- Train_6[!((revenue - mean(revenue))/sd(revenue)) > 3]

Train_6 <- Train_6[, lapply(.SD,sqrt), by = .(Open.Date,revenue)]
Train_6 <- Train_6[ , revenue := log(revenue)]
Test_6 <- Test_6[, lapply(.SD,sqrt), by = .(Open.Date)]


#omitted
Train_6$Type = as.character(Train_6$Type)
Train_6[Type == "DT", ':=' (Type = "FC")]
Train_6$Type = as.factor(Train_6$Type)
Test_6$Type = as.character(Test_6$Type) #Convert from Factor to String
Test_6[Type=="MB", ':=' (Type = "FC")] #CODE TO UPDATE COLUMNS WITHOUT SPLITTING!!! EXTREMELY HELPFUL
Test_6[Type=="DT", ':=' (Type = "IL")]
Test_6$Type = as.factor(Test_6$Type) #Convert from String to Factor
#omitted

model8 <- randomForest(revenue ~ . , Train_6)
model9 <- svm(revenue ~ . , Train_6)

Test_6$revenue <- predict(model8, Test_6)
Test_6$revenue <- predict(model9, Test_6)
Test_6$revenue <- exp(Test_6$revenue)
Test_6$revenue <- predict(model8, Test_6)

RMSE(Test_6$revenue,Test_5$revenue)

write.csv(Test_6$revenue, "Sample15.csv")

#Model 8 and Model9, Included Type, removed some factors. Next try - including type, removing outliers in Test Revenue

#Test_6$revenue <- predict(model9,Test_6) 
#write.csv(Test_6, "Sample9.csv")
#Test_6$revenue <- Test_6$revenue^2
#write.csv(Test_6, "Sample9.csv")

#files used - Sample14.csv

varImpPlot(model6, sort = T, n.var = 20, col = "blue") #Plots variable importance, Random Forest Function

#fill := "prussian", size := "20", shape := "square" , stroke := "darkgreen", strokeDash := "4" 
#Parameters for layer_points()

Train_5 %>% ggvis(~revenue, ~Open.Date, fill := "skyblue", size := "35", shape := "square") %>% layer_points() #Fancy scatterplot
Train_5 %>% ggvis(~log(P17), ~log(revenue), fill:= "orange", stroke :="red") %>% layer_points() %>% layer_lines() # Important
Train_5 %>% ggvis(~P17, ~log(revenue), fill:= "orange", stroke :="red") %>% layer_points() %>% layer_lines() 
Train_5 %>% ggvis(~log(P17), fill:= "orange", stroke :="red") %>% layer_histograms() # Histogram plot
Train_5 %>% ggvis(~revenue, fill:= "orange", stroke :="red") %>% layer_densities() # Density plot
Train_5 %>% ggvis(~log(P12), fill:= "orange", stroke :="red") %>% layer_densities() #Density plot log


# set1 <- Train_3 %>% select(P18, P16, P36, P32, P34, P25, P24, P26) - Latest Data
