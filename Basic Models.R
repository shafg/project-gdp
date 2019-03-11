TD_GDP<-read.csv("C:/Users/shafa/Downloads/TD_GDP.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))
sapply(TD_GDP, function(x) sum(is.na(x)))
library('Rcpp')
library('ggplot2')
library('class')
library('prediction')
library('FNN')
library("gmodels")
library('useful')
library('randomForest')
library('pmml')
library('caret')

set.seed(100)

# MODEL 1: (VAL~NAICS) Simple Linear Regression 
train_data<-TD_GDP[TD_GDP$Year != 2017,] #choosing test data
test_data<-TD_GDP[TD_GDP$Year == 2017,] #choosing train data

model1 <-lm(VAL~NAICS, data = train_data) #linear model
prediction1 <- predict(model1, interval = "prediction", newdata = test_data)

pred1<-prediction1[,"fit"] #calculating errors
pred_vs_act1<-cbind.data.frame(pred1, test_data$VAL)
pred_vs_act1$errors<- (pred_vs_act1$pred - pred_vs_act1$`test_data$VAL`)
View(pred_vs_act1)
hist(pred_vs_act1$errors, breaks = 50, main = "errors") #plotting errors
summary(model1)
rmse1 <- sqrt(sum((prediction1[,"fit"] - test_data$VAL)^2)/nrow(test_data))
rmse1

#rmse1 = [1] 1770.083. 
#Residual standard error: 939.1 on 32487 degrees of freedom
#Multiple R-squared:  0.9948,	Adjusted R-squared:  0.9948 
#F-statistic: 2.284e+04 on 272 and 32487 DF,  p-value: < 2.2e-16

#MODEL 2:(VAL~NAICS+MONTH) Simple Regression with two variables kept in sight
##(errors are slightly less)

TD_GDP$NAICS<-as.factor(TD_GDP$NAICS)

model2<-lm(VAL~NAICS+Month, data=train_data)
prediction2 <- predict(model2, interval="prediction", newdata =test_data)

pred2<-prediction2[,"fit"] #calculating errors
pred_vs_act2<-cbind.data.frame(pred2, test_data$VAL)
pred_vs_act2$errors<- (pred_vs_act2$pred - pred_vs_act2$`test_data$VAL`)
View(pred_vs_act2)

rmse2 <- sqrt(sum((prediction2[,"fit"] - test_data$VAL)^2)/nrow(test_data))
errors2 <- prediction2[,"fit"] - test_data$VAL
summary(model2)
hist(errors2, breaks = 50, main = "errors") #plotting errors

# rmse2 = [1] 1768.294, a little better.
#Residual standard error: 937.5 on 32486 degrees of freedom
#Multiple R-squared:  0.9948,	Adjusted R-squared:  0.9948 
#F-statistic: 2.284e+04 on 273 and 32486 DF,  p-value: < 2.2e-16

#MODEL 3: KNN (better predicted Values)
train_data<-TD_GDP[TD_GDP$Year != 2017,]
test_data<-TD_GDP[TD_GDP$Year == 2017,]

train.set_new <- train_data[-3]
test.set_new <- test_data[-3]

train_labels <- train_data$VAL
test_labels <- test_data$VAL

gdp_pred <- knn.reg(train = train.set_new, test = test.set_new, y = train_labels, k = 10)
rmse <- sqrt(sum((test_data$VAL - gdp_pred$pred)^2)/length(test_data$VAL))
rmse

#[1] 249.0747

#MODEL 4 : RandomForest choosing 100 trees and all the dataset (my research tells me it can not handle more than 53 categories)
myformula<- VAL~NAICS
gdpx<-build.x(myformula, data = TD_GDP)
gdpy<-build.y(myformula, data = TD_GDP)
gdpForest1<- randomForest(x=gdpx, y=gdpy, ntree = 100)
gdpForest1

RF_pred1=predict(gdpForest1, gdpx)
RF_pred1

##result
#Call:
#  randomForest(x = gdpx, y = gdpy, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 91

#Mean of squared residuals: 1073627
#% Var explained: 99.38

#MODEL 5: RandomForest using training data only (10 years)
myformula<- VAL~NAICS
gdpx<-build.x(myformula, data = train_data)
gdpy<-build.y(myformula, data = train_data)
gdpForest2<- randomForest(x=gdpx, y=gdpy, ntree = 100)
importance(gdpForest2)
gdpForest2$predicted

RF_pred2=predict(gdpForest2, gdpx)
RF_pred2
#Result:
#Call:
# randomForest(x = gdpx, y = gdpy, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 91

#Mean of squared residuals: 889193.9
#% Var explained: 99.47

##> confMatrix <- confusionMatrix(RF_pred,train_data$VAL)
##Error: `data` and `reference` should be factors with the same levels.

#MODEL 6: RandomForest using training data only and changing factors as numeric (not much luck with this too)
TD_GDP$NAICS<-as.numeric(as.factor(TD_GDP$NAICS))

train_data<-TD_GDP[TD_GDP$Year != 2017,]
test_data<-TD_GDP[TD_GDP$Year == 2017,]

myformula<- VAL~NAICS
gdpx<-build.x(myformula, data = train_data)
gdpy<-build.y(myformula, data = train_data)
gdpForest3<- randomForest(x=gdpx, y=gdpy, ntree = 100)
RF_pred3=predict(gdpForest3, gdpx)
RF_pred3

#Call:
#  randomForest(x = gdpx, y = gdpy, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 91

#Mean of squared residuals: 890362.5
#% Var explained: 99.47


