
IND_GDP<-read.csv("C:/Users/shafa/Downloads/IND_GDP.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))
library(ggplot2)
library(scales)
library(reshape2)
library(useful)
library(forecast)
library(zoo)
library("vars")
library(dplyr)
library(prediction)
library("Hmisc")



IND_GDP_Yr <- subset(IND_GDP, Month == 12 & Year!= "2017")

AGG_GDP<-filter(IND_GDP_Yr, NAICS %in% c("All industries" , "Agriculture, forestry, fishing and hunting" ,
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

summary(gdpTS)

describe(AGG_GDP)
AGG_GDP$NAICS<-as.factor(AGG_GDP$NAICS)

gdpCast<-dcast(AGG_GDP, Year~NAICS, value.var  = "VAL")
gdpTS <- ts(data=gdpCast[, -1], start=min(gdpCast$Year), end = max(gdpCast$Year))
View(gdpTS)

#writing CSV 
#write.csv(gdpTS, file = "C:/Users/shafa/Downloads/GDP_TS.csv", row.names = FALSE)

#taking out Management of companies, since the data is only for 11 years, I will calculate it separately
gdpTS1 <- gdpTS[, which(colnames(gdpTS) != "Management of companies and enterprises")]

gdpTS1 <- gdpTS[,1:10] #first 10 inudustries, var model can take upto 10 variables

gdpPred1 <- VAR(gdpTS1)
var_fcst1<-forecast(gdpPred1, h = 5)

var_fcst1$forecast

gdpTS2 <- gdpTS[,12:21] # next 10 industries.

gdpPred2 <- VAR(gdpTS2)
var_fcst2<-forecast(gdpPred2, h = 5)

var_fcst2$forecast

gdpTS3 <- gdpTS[,c(11,4)] #taking "all industries" in the mix, since VAR works with more than one variable.
gdpTS3 <- window(gdpTS3, start=2007)

gdpPred3 <- VAR(gdpTS3)
var_fcst3<-forecast(gdpPred3, h = 5)

var_fcst3$forecast
autoplot(var_fcst1$forecast$Accommodation.and.food.services, xlab = "YEAR", ylab = "GDP", main = "Accommodation and food services")
autoplot(var_fcst1$forecast$Administrative.and.support..waste.management.and.remediation.services, xlab = "YEAR", ylab = "GDP",
         main = "Administrative and support,waste management and remediation services")
autoplot(var_fcst1$forecast$Agriculture..forestry..fishing.and.hunting, xlab = "YEAR", ylab = "GDP", main = "Agriculture, forestry, fishing and hunting")
autoplot(var_fcst1$forecast$Arts..entertainment.and.recreation, xlab = "YEAR", ylab = "GDP", main = "Arts, entertainment and recreation")
autoplot(var_fcst1$forecast$Construction, xlab = "YEAR", ylab = "GDP", main = "Construction")
autoplot(var_fcst1$forecast$Educational.services, xlab = "YEAR", ylab = "GDP", main = "Educational services")
autoplot(var_fcst1$forecast$Finance.and.insurance, xlab = "YEAR", ylab = "GDP", main = "Finance and insurance")
autoplot(var_fcst1$forecast$Health.care.and.social.assistance, xlab = "YEAR", ylab = "GDP", main = "Health care and social assistance")
autoplot(var_fcst1$forecast$Information.and.cultural.industries, xlab = "YEAR", ylab = "GDP", main = "Information and cultural industries")
autoplot(var_fcst2$forecast$Manufacturing, xlab = "YEAR", ylab = "GDP", main = "Manufacturing")
autoplot(var_fcst2$forecast$Mining..quarrying..and.oil.and.gas.extraction, xlab = "YEAR", ylab = "GDP", main = "Mining, quarrying and oil and gas extraction")
autoplot(var_fcst2$forecast$Other.services..except.public.administration., xlab = "YEAR", ylab = "GDP", main = "Other services except public administration")
autoplot(var_fcst2$forecast$Professional..scientific.and.technical.services, xlab = "YEAR", ylab = "GDP", main = "Professional, scientific and 
         technical services")
autoplot(var_fcst2$forecast$Public.administration, xlab = "YEAR", ylab = "GDP", main = "Public administration")
autoplot(var_fcst2$forecast$Real.estate.and.rental.and.leasing, xlab = "YEAR", ylab = "GDP", main = "Real estate, rental and leasing")
autoplot(var_fcst2$forecast$Retail.trade, xlab = "YEAR", ylab = "GDP", main = "Retail trade")
autoplot(var_fcst2$forecast$Transportation.and.warehousing, xlab = "YEAR", ylab = "GDP", main = "Transportation and warehousing")
autoplot(var_fcst2$forecast$Utilities, xlab = "YEAR", ylab = "GDP", main = "Utilities")
autoplot(var_fcst2$forecast$Wholesale.trade, xlab = "YEAR", ylab = "GDP", main = "Wholesale trade")
autoplot(var_fcst3$forecast$Management.of.companies.and.enterprises, xlab = "YEAR", ylab = "GDP", main = "Management of Companies and Enterprises")
autoplot(var_fcst3$forecast$All.industries, xlab = "YEAR", ylab = "GDP", main = "All industries")



#The Results of the forecasts for the next 5 years for the aggregate industries:

#$Accommodation.and.food.services
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       2904.980 2850.101 2959.860 2821.050 2988.911
#2018       2982.219 2886.303 3078.134 2835.529 3128.908
#2019       3049.639 2923.802 3175.475 2857.188 3242.090
#2020       3130.194 2988.778 3271.610 2913.917 3346.472
#2021       3208.892 3062.591 3355.193 2985.145 3432.640

#$Administrative.and.support..waste.management.and.remediation.services
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       3513.319 3417.395 3609.242 3366.616 3660.022
#2018       3427.464 3291.139 3563.789 3218.974 3635.955
#2019       3292.589 3131.604 3453.575 3046.383 3538.795
#2020       3198.027 3023.139 3372.916 2930.558 3465.496
#2021       3161.639 2972.756 3350.521 2872.768 3450.510

#$Agriculture..forestry..fishing.and.hunting
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       1361.137 1307.855 1414.420 1279.649 1442.626
#2018       1433.009 1365.969 1500.049 1330.480 1535.538
#2019       1462.925 1382.792 1543.059 1340.372 1585.479
#2020       1479.346 1391.155 1567.538 1344.470 1614.223
#2021       1470.228 1374.762 1565.694 1324.225 1616.230

#$All.industries
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       142499.9 139676.2 145323.5 138181.5 146818.3
#2018       143199.8 139385.9 147013.8 137367.0 149032.7
#2019       143444.6 139039.2 147849.9 136707.2 150181.9
#2020       143902.3 139247.1 148557.4 136782.9 151021.7
#2021       145052.3 140209.5 149895.1 137645.9 152458.7

#$Arts..entertainment.and.recreation
#Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#2017      1070.7234 1037.5019 1103.945 1019.9154 1121.531
#2018      1050.2647 1000.5004 1100.029  974.1568 1126.373
#2019      1028.0898  960.3841 1095.796  924.5428 1131.637
#2020       986.9980  909.1431 1064.853  867.9293 1106.067
#2021       975.5414  891.4209 1059.662  846.8902 1104.193

#$Construction
#Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95
#2017       9194.559 8876.348  9512.769 8707.898  9681.219
#2018       9231.846 8780.069  9683.623 8540.913  9922.779
#2019       9423.016 8924.674  9921.358 8660.868 10185.165
#2020       9779.655 9242.765 10316.544 8958.552 10600.757
#2021      10172.962 9600.077 10745.847 9296.810 11049.113

#$Educational.services
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       8061.681 7856.144 8267.217 7747.340 8376.021
#2018       8012.129 7759.012 8265.246 7625.020 8399.238
#2019       8037.155 7765.285 8309.026 7621.366 8452.945
#2020       8098.723 7808.748 8388.699 7655.244 8542.202
#2021       8160.387 7851.005 8469.769 7687.228 8633.547

#$Finance.and.insurance
#Point Forecast     Lo 80    Hi 80     Lo 95    Hi 95
#2017       10556.25 10301.326 10811.18 10166.375 10946.13
#2018       10644.90 10252.027 11037.78 10044.051 11245.75
#2019       10592.54 10113.406 11071.67  9859.768 11325.31
#2020       10553.46 10009.092 11097.83  9720.920 11386.00
#2021       10566.28  9969.319 11163.25  9653.304 11479.27

#$Health.care.and.social.assistance
#Point Forecast     Lo 80     Hi 80     Lo 95     Hi 95
#2017       9789.892  9717.681  9862.103  9679.454  9900.329
#2018      10112.777 10022.975 10202.579  9975.437 10250.117
#2019      10331.347 10208.241 10454.452 10143.073 10519.620
#2020      10447.381 10268.166 10626.596 10173.296 10721.466
#2021      10476.579 10229.266 10723.893 10098.346 10854.812

#$Information.and.cultural.industries
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       4270.568 4178.726 4362.409 4130.108 4411.027
#2018       4188.797 4052.090 4325.503 3979.721 4397.872
#2019       4067.208 3906.546 4227.871 3821.496 4312.920
#2020       3966.372 3789.661 4143.083 3696.116 4236.628
#2021       3900.180 3710.113 4090.247 3609.498 4190.862

#$Manufacturing
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       14131.78 13322.02 14941.54 12893.36 15370.20
#2018       14487.35 13559.72 15414.98 13068.66 15906.04
#2019       15516.19 14417.62 16614.77 13836.07 17196.32
#2020       16267.87 14979.30 17556.44 14297.17 18238.57
#2021       16789.02 15343.01 18235.03 14577.53 19000.50

#$Mining..quarrying..and.oil.and.gas.extraction
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       12896.62 12613.47 13179.78 12463.58 13329.67
#2018       12176.54 11711.35 12641.73 11465.10 12887.98
#2019       13781.65 13111.72 14451.59 12757.07 14806.24
#2020       13870.43 13061.69 14679.17 12633.57 15107.29
#2021       14114.46 13251.65 14977.27 12794.91 15434.01

#$Other.services..except.public.administration.
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       2682.702 2642.988 2722.415 2621.965 2743.438
#2018       2725.365 2647.289 2803.442 2605.957 2844.773
#2019       2828.119 2731.220 2925.018 2679.925 2976.313
#2020       2864.146 2758.979 2969.313 2703.307 3024.985
#2021       2903.934 2793.852 3014.017 2735.577 3072.291

#$Professional..scientific.and.technical.services
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       8086.295 7915.743 8256.847 7825.458 8347.132
#2018       8183.312 7955.416 8411.208 7834.775 8531.849
#2019       8512.274 8260.674 8763.874 8127.485 8897.062
#2020       8719.977 8453.654 8986.301 8312.670 9127.284
#2021       8899.472 8627.270 9171.674 8483.175 9315.769

#$Public.administration
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       8851.800 8672.360 9031.240 8577.370 9126.230
#2018       8759.265 8524.569 8993.961 8400.329 9118.202
#2019       8515.878 8232.243 8799.513 8082.096 8949.661
#2020       8321.623 7962.658 8680.587 7772.634 8870.612
#2021       8170.786 7716.877 8624.695 7476.593 8864.980

#$Real.estate.and.rental.and.leasing
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       18469.78 18337.50 18602.06 18267.48 18672.08
#2018       18947.64 18765.71 19129.57 18669.40 19225.87
#2019       19347.42 19131.54 19563.31 19017.25 19677.60
#2020       19736.38 19501.12 19971.64 19376.57 20096.18
#2021       20093.91 19835.85 20351.96 19699.25 20488.57

#$Retail.trade
#Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95
#2017       8942.129 8662.801  9221.458 8514.933  9369.326
#2018       8904.671 8554.904  9254.439 8369.748  9439.594
#2019       9203.711 8812.201  9595.222 8604.948  9802.475
#2020       9303.223 8878.022  9728.424 8652.934  9953.512
#2021       9569.917 9121.577 10018.256 8884.241 10255.592

#$Transportation.and.warehousing
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       6393.349 6263.562 6523.136 6194.857 6591.842
#2018       6363.122 6192.436 6533.808 6102.081 6624.163
#2019       6628.516 6438.670 6818.361 6338.172 6918.859
#2020       6821.762 6622.522 7021.001 6517.052 7126.472
#2021       7014.087 6804.855 7223.319 6694.094 7334.080

#$Utilities
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       3373.529 3250.908 3496.151 3185.996 3561.063
#2018       3365.669 3202.157 3529.181 3115.599 3615.739
#2019       3405.778 3229.409 3582.146 3136.045 3675.510
#2020       3237.802 3037.981 3437.623 2932.202 3543.402
#2021       3319.824 3103.735 3535.914 2989.343 3650.305

#$Wholesale.trade
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       8020.650 7800.766 8240.535 7684.365 8356.935
#2018       8279.676 8018.920 8540.432 7880.884 8678.468
#2019       8453.061 8164.057 8742.065 8011.067 8895.054
#2020       8734.143 8427.684 9040.602 8265.454 9202.832
#2021       8840.107 8523.530 9156.684 8355.944 9324.270

#$Management.of.companies.and.enterprises
#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       944.9100 902.0603 987.7597 879.3770 1010.443
#2018       941.5475 896.8897 986.2053 873.2492 1009.846
#2019       940.7743 895.7725 985.7762 871.9500 1009.599
#2020       939.0968 894.0654 984.1281 870.2272 1007.966
#2021       937.6349 892.5469 982.7230 868.6787 1006.591

