IND_GDP<-read.csv("C:/Users/shafa/Downloads/IND_GDP.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))
#Yearly GDP Forecast, using Auto ARIMA
IND_GDP_2017<- subset(IND_GDP, Year == "2017" & Month == 12) #Filtering the forecasting year
AGG_GDP_2017<-filter(IND_GDP_2017, NAICS %in% c("All industries" , "Agriculture, forestry, fishing and hunting" ,
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


AGG_GDP_2017[,"PRED"] <- c(142499.9, 1361.137,12896.62, 3373.529 ,9194.559, 14131.78, 8020.650, 8942.129 ,
6393.349 ,4270.568 ,10556.25, 18469.78, 8086.295, 944.9100 ,3513.319 ,8061.681,9789.892 ,1070.7234,
2904.980 , 2682.702, 8851.800)

AGG_GDP_2017[,"ERROR"] <- AGG_GDP_2017$VAL - AGG_GDP_2017$PRED

View(AGG_GDP_2017)
TOTAL_ERROR <- sum(AGG_GDP_2017$ERROR)
TOTAL_ERROR# [1] 4733.447

RMSE_VAR<- RMSE(AGG_GDP_2017$VAL, AGG_GDP_2017$PRED)
RMSE_VAR #[1] 667.9292

VAR_ER<- RMSE(AGG_GDP_2017$VAL, AGG_GDP_2017$PRED)/mean(AGG_GDP_2017$VAL) #prediction error rate
VAR_ER #[1] 0.04824252

