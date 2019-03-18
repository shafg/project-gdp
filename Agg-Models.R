
library('dplyr')
library('FNN')
library('useful')
library('randomForest')

##Working on forecasts for Aggregates of all industries.

#I have extracted 21 years for this as for 20 aggregates, only 1 has 11 years worth of data, 
#rest have for 21 years, I will choose along 21 or 11 as I go best suited for my models. 

#All industries is the total:
#Link for info: "https://www.statcan.gc.ca/eng/statistical-programs/document/1301_D2_T9_V1"
#we can easily extract 21 years worth of data for aggregates of industries, 
#except one (Management of companies and enterprises)

IND_GDP<-read.csv("C:/Users/shafa/Downloads/IND_GDP.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))
View(table(IND_GDP$NAICS)) #21 years * 12 months = 252 should be the total number of entry for each industry. 

#special agreggates that make the part of "All industries":
#1:Agriculture, forestry, fishing and hunting
#2:Mining, quarrying, and oil and gas extraction
#3:Utilities
#4:Construction
#5:Manufacturing
#6:Wholesale trade
#7:Retail trade
#8:Transportation and warehousing
#9:Information and cultural industries
#10:Finance and insurance
#11:Real estate and rental and leasing
#12:Professional, scientific and technical services
#13:Management of companies and enterprises
#14:Administrative and support, waste management and remediation services
#15:Educational services
#16:Health care and social assistance
#17:Arts, entertainment and recreation
#18:Accommodation and food services
#19:Other services (except public administration)
#20:Public administration

AGG_GDP<-filter(IND_GDP, NAICS %in% c("All industries" , "Agriculture, forestry, fishing and hunting" ,
                                  "Mining, quarrying, and oil and gas extraction" , "Utilities" ,
                                  "Construction", "Manufacturing" , "Wholesale trade" , "Retail trade" ,
                                  "Transportation and warehousing" , "Information and cultural industries" ,
                                  "Finance and insurance" , "Real estate and rental and leasing",
                                  "Professional, scientific and technical services", 
                                  "Management of companies and enterprises" ,
                                  "Administrative and support, waste management and remediation services" , "Educational services",
                                  "Health care and social assistance" , "Arts, entertainment and recreation",
                                  "Accommodation and food services" , "Other services (except public administration)",
                                  "Public administration"))
View(AGG_GDP)
str(AGG_GDP)
View(table(AGG_GDP$NAICS)) #observed just "Management of companies and enterprises" has 132 enteries for 11 years.
set.seed(100)

# MODEL 1a: (VAL~NAICS) Simple Linear Regression (using the 21 years)
train_data<-AGG_GDP[AGG_GDP$Year != 2017,] #choosing test data
test_data<-AGG_GDP[AGG_GDP$Year == 2017,] #choosing train data

model1 <-lm(VAL~NAICS, data = train_data) #linear model
prediction1 <- predict(model1, interval = "prediction", newdata = test_data)

pred1<-prediction1[,"fit"] #calculating errors

pred_vs_act1<-cbind.data.frame(pred1, test_data$VAL)
pred_vs_act1$errors<- (pred_vs_act1$pred - pred_vs_act1$`test_data$VAL`)
View(pred_vs_act1)

hist(pred_vs_act1$errors, breaks = 50, main = "errors") #plotting errors
summary(model1)
rmse1 <- sqrt(sum((prediction1[,"fit"] - test_data$VAL)^2)/nrow(test_data))
rmse1 #[1] 6317.509

# MODEL 1b: (VAL~NAICS) Simple Linear Regression (using the 11 years)
AGG_GDP1<-AGG_GDP[AGG_GDP$Year >= 2007,]
View(table(AGG_GDP1$NAICS))

train_data<-AGG_GDP1[AGG_GDP1$Year != 2017,] #choosing test data
test_data<-AGG_GDP1[AGG_GDP1$Year == 2017,] #choosing train data

modely <-lm(VAL~NAICS, data = train_data) #linear model
prediction2 <- predict(modely, interval = "prediction", newdata = test_data)

pred2<-prediction2[,"fit"] #calculating errors

pred_vs_act2<-cbind.data.frame(pred2, test_data$VAL)
pred_vs_act2$errors<- (pred_vs_act2$pred - pred_vs_act2$`test_data$VAL`)
View(pred_vs_act2)

hist(pred_vs_act2$errors, breaks = 50, main = "errors") #plotting errors
summary(modely)
rmse2 <- sqrt(sum((prediction2[,"fit"] - test_data$VAL)^2)/nrow(test_data))
rmse2 #[1] 3691.144 #predictions have improved

#MODEL 2a: KNN #using 21 years
#taking the non-numeric values out of the dataset while training
AGG_GDP$NAICS<-as.factor(AGG_GDP$NAICS)
train_data<-AGG_GDP[AGG_GDP$Year != 2017,]
test_data<-AGG_GDP[AGG_GDP == 2017,]

train.set_new <- train_data[-3]
test.set_new <- test_data[-3]

train_labels <- train_data$VAL
test_labels <- test_data$VAL

GDP_pred <- knn.reg(train = train.set_new, test = test.set_new, y = train_labels, k = 10)
rmse <- sqrt(sum((test_data$VAL - GDP_pred$pred)^2)/length(test_data$VAL))
rmse #859.5114

#MODEL 2b: KNN #using 11 years
AGG_GDP1$NAICS<-as.factor(AGG_GDP1$NAICS)
train_data<-AGG_GDP1[AGG_GDP1$Year != 2017,]
test_data<-AGG_GDP1[AGG_GDP1 == 2017,]

train.set_new <- train_data[-3]
test.set_new <- test_data[-3]

train_labels <- train_data$VAL
test_labels <- test_data$VAL

GDP_pred2 <- knn.reg(train = train.set_new, test = test.set_new, y = train_labels, k = 10)
rmse_b <- sqrt(sum((test_data$VAL - GDP_pred2$pred)^2)/length(test_data$VAL))
rmse_b #859.5711 (not a significant difference)


#MODEL 2c: KNN, when converting the industry into numeric rather than factor
dataset <- AGG_GDP  
dataset$NAICS<-as.numeric(dataset$NAICS)
train_data<-dataset[dataset$Year != 2017,]
test_data<-dataset[dataset == 2017,]

prediction <- knn.reg(train_data, 
                      test = test_data,
                      dataset$VAL, k = 10)  
rmse_c <- sqrt(sum((test_data$VAL - prediction$pred)^2)/length(test_data$VAL))
rmse_c #859.5116 (not much of a difference) 

#MODEL 3: RandomForest
#Using 21 years and 100 trees
#training and testing the whole data

myformula<- AGG_GDP$VAL~AGG_GDP$NAICS
gdpx<-build.x(myformula, data = AGG_GDP)
gdpy<-build.y(myformula, data = AGG_GDP)
gdpForest1<- randomForest(x=gdpx, y=gdpy, ntree = 100)
gdpForest1

RF_pred1=predict(gdpForest1, gdpx)
RF_pred1

rmse3 <- sqrt(sum((AGG_GDP$VAL - RF_pred1)^2)/length(AGG_GDP$VAL))
rmse3 #3726.665

#ntree=100
#Call:
# randomForest(x = gdpx, y = gdpy, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 7

#Mean of squared residuals: 14032894
#% Var explained: 97.71
 
#MODEL 3b: RandomForest
#Using 11 years and 100 trees
#training and testing the whole data

myformula1<- AGG_GDP1$VAL~AGG_GDP1$NAICS
gdpx1<-build.x(myformula1, data = AGG_GDP1)
gdpy1<-build.y(myformula1, data = AGG_GDP1)
gdpForest2<- randomForest(x=gdpx1, y=gdpy1, ntree = 100)
gdpForest2

RF_pred2=predict(gdpForest2, gdpx1)
RF_pred2

rmse4 <- sqrt(sum((AGG_GDP1$VAL - RF_pred1)^2)/length(AGG_GDP1$VAL))
rmse4

#ntree=100
#Call:
#  randomForest(x = gdpx1, y = gdpy1, ntree = 100) 
#Type of random forest: regression
#Number of trees: 100
#No. of variables tried at each split: 7

#Mean of squared residuals: 4747089
#% Var explained: 99.34

#MODEL 3c: RandomForest
#Using 21 years and unlimited trees
#training and testing the whole data

gdpForest3<- randomForest(x=gdpx, y=gdpy)
gdpForest3

RF_pred3=predict(gdpForest3, gdpx)
RF_pred3

rmse5 <- sqrt(sum((AGG_GDP$VAL - RF_pred3)^2)/length(AGG_GDP$VAL))
rmse5 #3724.359

#ntree=500
#Call:
#  randomForest(x = gdpx, y = gdpy) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 7

#Mean of squared residuals: 13977128
#% Var explained: 97.72

#MODEL 3d: RandomForest
#Using 11 years and unlimited trees
#training and testing the whole data

gdpForest4<- randomForest(x=gdpx1, y=gdpy1)
gdpForest4

RF_pred4=predict(gdpForest4, gdpx1)
RF_pred4

rmse4 <- sqrt(sum((AGG_GDP1$VAL - RF_pred4)^2)/length(AGG_GDP1$VAL))
rmse4 #2185.181

#ntree=500
#Call:
#  randomForest(x = gdpx1, y = gdpy1) 
#Type of random forest: regression
#Number of trees: 500
#No. of variables tried at each split: 7

#Mean of squared residuals: 4844636
#% Var explained: 99.32


