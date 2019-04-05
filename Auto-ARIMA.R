library('zoo')
library('ggplot2')
library('forecast')
library('useful')
library('scales')

IND_GDP<-read.csv("C:/Users/shafa/Downloads/IND_GDP.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))

#Yearly GDP Forecast, using Auto ARIMA
IND_GDP_2017<- subset(IND_GDP, Year == "2017" & Month == 12) #Filtering the forecasting year
IND_GDP <- subset(IND_GDP, Year!= "2017") #taking out the forecasting year 
View(table(IND_GDP$NAICS)) #240 enteries for complete 20 yrs (12 mon * 20 years, taking out the 2017)
IND_GDP_Yr <- subset(IND_GDP, Month == 12) #only taking the annual year end GDP

##All industries
All_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "All industries"]
All_ind<- ts(All_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(All_ind, ylab = "GDP", xlab = "Year", title = "Industry GDP per year", acf = T)

#recession in 2007 and 2008 figures dropped

ind_best<- auto.arima(x = All_ind)
ind_best

#Series:  using 20 years
#  ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#2720.368
#s.e.   406.126

#sigma^2 estimated as 3308322:  log likelihood=-169.06
#AIC=342.12   AICc=342.87   BIC=344.01

#Forecasting next 5 years
forecast_all_ind5<-forecast(ind_best, h = 5)
autoplot(forecast_all_ind5, main = "All Industries GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       143010.4 140679.4 145341.4 139445.4 146575.3
#2018       145730.7 142434.2 149027.3 140689.2 150772.3
#2019       148451.1 144413.7 152488.5 142276.5 154625.8
#2020       151171.5 146509.5 155833.4 144041.6 158301.3
#2021       153891.8 148679.6 159104.1 145920.4 161863.3

forecast_all_ind5[["mean"]]
# 2017 - [1] 143010.4
# All Industry Real GDP 2017, 145375

## 1: Agriculture, forestry, fishing and hunting
Affh_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Agriculture, forestry, fishing and hunting"]
Affh_ind<- ts(Affh_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(Affh_ind, ylab = "GDP", xlab = "Year", title = "Agriculture, forestry, fishing and hunting/Year", acf = T)

Affh_best<- auto.arima(x = Affh_ind)
Affh_best

#Series:  
#  ARIMA(0,1,0) 

#sigma^2 estimated as 3937:  log likelihood=-105.6
#AIC=213.21   AICc=213.44   BIC=214.15

forecast_Affh<-forecast(Affh_best, h = 5)
autoplot(forecast_Affh, main = "Agriculture, forestry, fishing and hunting GDP Forecast",xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
#2017           1230 1149.586 1310.414 1107.0170 1352.983
#2018           1230 1116.277 1343.723 1056.0758 1403.924
#2019           1230 1090.718 1369.282 1016.9872 1443.013
#2020           1230 1069.171 1390.829  984.0340 1475.966
#2021           1230 1050.188 1409.812  955.0017 1504.998

forecast_Affh[["mean"]]
# 2017 - [1] 1230
#Agriculture, forestry, fishing and hunting Real GDP 2017, 1248

## 2: Mining, quarrying, and oil and gas extraction
mqogg_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Mining, quarrying, and oil and gas extraction"]
mqogg_ind<- ts(mqogg_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(mqogg_ind, ylab = "GDP", xlab = "Year", title = "Mining, quarrying, and oil and gas extraction/Year", acf = T)

mqogg_best<- auto.arima(x = mqogg_ind)
mqogg_best

#Series:  
#ARIMA(0,1,0) 

#sigma^2 estimated as 329774:  log likelihood=-147.67
#AIC=297.34   AICc=297.57   BIC=298.28

forecast_mqogg<-forecast(mqogg_best, h = 5)
autoplot(forecast_mqogg, main = "Mining, quarrying, and oil and gas extraction GDP Forecast",
         xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80     Lo 95    Hi 95
#2017          11928 11192.06 12663.94 10802.472 13053.53
#2018          11928 10887.22 12968.78 10336.263 13519.74
#2019          11928 10653.31 13202.69  9978.529 13877.47
#2020          11928 10456.11 13399.89  9676.945 14179.06
#2021          11928 10282.38 13573.62  9411.243 14444.76

forecast_mqogg[["mean"]]
# 2017 - [1] 11928
#Mining, quarrying, and oil and gas extraction Real GDP 2017, 12749

## 3: Utilities
u_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Utilities"]
u_ind<- ts(u_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(u_ind, ylab = "GDP", xlab = "Year", title = "Utilities/Year", acf = T)

u_best<- auto.arima(x = u_ind)
u_best

#Series:  
#  ARIMA(0,1,1) with drift 

#Coefficients:
#  ma1    drift
#-0.6671  34.3364
#s.e.   0.2879  10.4774

#sigma^2 estimated as 13720:  log likelihood=-116.7
#AIC=239.4   AICc=241   BIC=242.23

forecast_u<-forecast(u_best, h = 5)
autoplot(forecast_u, main = "Utilities GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       3647.384 3497.273 3797.495 3417.809 3876.959
#2018       3681.720 3523.508 3839.932 3439.756 3923.685
#2019       3716.057 3550.139 3881.975 3462.307 3969.806
#2020       3750.393 3577.112 3923.675 3485.382 4015.404
#2021       3784.730 3604.385 3965.074 3508.916 4060.543

forecast_u[["mean"]]
#2017 [1] 3647.384
# Utilities Real GDP 2017, 3766


## 4: Construction
c_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Construction"]
c_ind<- ts(c_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(c_ind, ylab = "GDP", xlab = "Year", title = "Construction/Year", acf = T)

c_best<- auto.arima(x = c_ind)
c_best

#Series:  
#  ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#208.1053
#s.e.   68.5625

#sigma^2 estimated as 94278:  log likelihood=-135.26
#AIC=274.52   AICc=275.27   BIC=276.41

forecast_c<-forecast(c_best, h = 5)
autoplot(forecast_c, main = "Construction GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80     Hi 80    Lo 95    Hi 95
#2017       9541.105 9147.607  9934.603 8939.302 10142.91
#2018       9749.211 9192.721 10305.701 8898.133 10600.29
#2019       9957.316 9275.758 10638.874 8914.962 10999.67
#2020      10165.421 9378.425 10952.417 8961.815 11369.03
#2021      10373.526 9493.638 11253.414 9027.854 11719.20

forecast_c[["mean"]]
# 2017 - [1]  9541.105
# Construction Real GDP, 9758

## 5: Manufacturing
m_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Manufacturing"]
m_ind<- ts(m_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(m_ind, ylab = "GDP", xlab = "Year", title = "Manufacturing/Year", acf = T)

m_best<- auto.arima(x = m_ind)
m_best

#Series:  
#  ARIMA(1,0,0) with non-zero mean 

#Coefficients:
#  ar1        mean
#0.6927  13189.0774
#s.e.  0.1580    454.7048

#sigma^2 estimated as 517414:  log likelihood=-159.22
#AIC=324.44   AICc=325.94   BIC=327.42

forecast_m<-forecast(m_best, h = 5)
autoplot(forecast_m, main = "Manufacturing GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       13277.00 12355.16 14198.84 11867.16 14686.83
#2018       13249.98 12128.57 14371.38 11534.94 14965.02
#2019       13231.26 12025.77 14436.75 11387.63 15074.90
#2020       13218.30 11974.48 14462.12 11316.04 15120.56
#2021       13209.32 11947.52 14471.12 11279.56 15139.08

forecast_m[["mean"]]
#2017 - [1] 13277.00
#Manufacturing Real GDP 2017, 13895

## 6: Wholesale trade
wt_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Wholesale trade"]
wt_ind<- ts(wt_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(wt_ind, ylab = "GDP", xlab = "Year", title = "Wholesale trade/Year", acf = T)

wt_best<- auto.arima(x = wt_ind)
wt_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#194.4211
#s.e.   39.7377

#sigma^2 estimated as 31670:  log likelihood=-124.9
#AIC=253.79   AICc=254.54   BIC=255.68

forecast_wt<-forecast(wt_best, h = 5)
autoplot(forecast_wt, main = "Wholesale Trade GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       7836.421 7608.355 8064.487 7487.624 8185.218
#2018       8030.842 7708.308 8353.376 7537.569 8524.115
#2019       8225.263 7830.241 8620.285 7621.130 8829.397
#2020       8419.684 7963.552 8875.816 7722.091 9117.278
#2021       8614.105 8104.134 9124.076 7834.172 9394.038

forecast_wt[["mean"]]
#2017 - [1] 7836.421
# Wholesale trade Real GDP 2017, 8085

## 7: Retail trade
rt_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Retail trade"]
rt_ind<- ts(rt_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(rt_ind, ylab = "GDP", xlab = "Year", title = "Retail trade/Year", acf = T)

rt_best<- auto.arima(x = rt_ind)
rt_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#218.3158
#s.e.   44.1719

#sigma^2 estimated as 39133:  log likelihood=-126.91
#AIC=257.81   AICc=258.56   BIC=259.7

forecast_rt<-forecast(rt_best, h = 5)
autoplot(forecast_rt, main = "Retail Trade GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95
#2017       9278.316 9024.800  9531.832 8890.597  9666.035
#2018       9496.632 9138.106  9855.157 8948.314 10044.949
#2019       9714.947 9275.845 10154.050 9043.398 10386.497
#2020       9933.263 9426.231 10440.295 9157.825 10708.701
#2021      10151.579 9584.700 10718.458 9284.613 11018.545

forecast_rt[["mean"]]
# 2017 - [1]  9278.316
#Retail trade Real GDP 2017, 9406

## 8: Transportation and warehousing
tw_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Transportation and warehousing"]
tw_ind<- ts(tw_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(tw_ind, ylab = "GDP", xlab = "Year", title = "Transportation and warehousing/Year", acf = T)

tw_best<- auto.arima(x = tw_ind)
tw_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#123.0526
#s.e.   26.0506

#sigma^2 estimated as 13611:  log likelihood=-116.87
#AIC=237.75   AICc=238.5   BIC=239.63

forecast_tw<-forecast(tw_best, h = 5)
autoplot(forecast_tw, main = "Transportation and warehousing GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       6414.053 6264.538 6563.567 6185.390 6642.715
#2018       6537.105 6325.660 6748.550 6213.728 6860.483
#2019       6660.158 6401.192 6919.124 6264.103 7056.213
#2020       6783.211 6484.182 7082.239 6325.886 7240.535
#2021       6906.263 6571.939 7240.587 6394.959 7417.568


forecast_tw[["mean"]]
# 2017 - [1] 6414.053
#Transportation and warehousing Real GDP 2017, 6588


## 9: Information and cultural industries
ic_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Information and cultural industries"]
ic_ind<- ts(ic_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(ic_ind, ylab = "GDP", xlab = "Year", title = "Information and cultural industries/Year", acf = T)

ic_best<- auto.arima(x = ic_ind)
ic_best

#Series:  
#ARIMA(0,2,1) 

#Coefficients:
#  ma1
#-0.5169
#s.e.   0.1687

#sigma^2 estimated as 7271:  log likelihood=-105.21
#AIC=214.41   AICc=215.21   BIC=216.19

forecast_ic<-forecast(ic_best, h = 5)
autoplot(forecast_ic, main = "Information and cultural industries GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       4309.010 4199.730 4418.290 4141.880 4476.139
#2018       4326.020 4130.545 4521.495 4027.066 4624.973
#2019       4343.029 4052.547 4633.511 3898.775 4787.283
#2020       4360.039 3965.040 4755.039 3755.939 4964.139
#2021       4377.049 3868.404 4885.694 3599.143 5154.955

forecast_ic[["mean"]]
#2017 - [1] 4309.010
#Information and cultural industries Real GDP 2017, 4331

## 10: Finance and insurance
fi_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Finance and insurance"]
fi_ind<- ts(fi_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(fi_ind, ylab = "GDP", xlab = "Year", title = "Finance and insurance/Year", acf = T)

fi_best<- auto.arima(x = fi_ind)
fi_best

#Series:  
#  ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#233.2105
#s.e.   41.7450

#sigma^2 estimated as 34951:  log likelihood=-125.83
#AIC=255.66   AICc=256.41   BIC=257.55

forecast_fi<-forecast(fi_best, h = 5)
autoplot(forecast_fi, main = "Finance and insurance GDP Forecast", xlab = "Year", ylab = "GDP")


#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       10421.21 10181.62 10660.80 10054.79 10787.63
#2018       10654.42 10315.59 10993.25 10136.22 11172.62
#2019       10887.63 10472.65 11302.61 10252.97 11522.29
#2020       11120.84 10641.66 11600.02 10388.00 11853.68
#2021       11354.05 10818.31 11889.79 10534.71 12173.39

forecast_fi[["mean"]]
#2017 - [1] 10421.21
#Finance and insurance Real GDP 2017, 10374

## 11: Real estate and rental and leasing
rrl_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Real estate and rental and leasing"]
rrl_ind<- ts(rrl_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(rrl_ind, ylab = "GDP", xlab = "Year", title = "Real estate and rental and leasing/Year", acf = T)

rrl_best<- auto.arima(x = rrl_ind)
rrl_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#426.8947
#s.e.   24.8510

#sigma^2 estimated as 12391:  log likelihood=-115.98
#AIC=235.95   AICc=236.7   BIC=237.84

forecast_rrl<-forecast(rrl_best, h = 5)
autoplot(forecast_rrl, main = "Real estate and rental and leasing GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       18556.89 18414.24 18699.55 18338.72 18775.07
#2018       18983.79 18782.05 19185.53 18675.25 19292.33
#2019       19410.68 19163.60 19657.77 19032.80 19788.57
#2020       19837.58 19552.27 20122.89 19401.24 20273.92
#2021       20264.47 19945.49 20583.46 19776.63 20752.32

forecast_rrl[["mean"]]
#2017 - [1] 18556.89
#Real estate and rental and leasing Real GDP 2017, 18665

## 12: Professional, scientific and technical services

psts_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Professional, scientific and technical services"]
psts_ind<- ts(psts_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(psts_ind, ylab = "GDP", xlab = "Year", title = "Professional, scientific and technical services/Year", acf = T)

psts_best<- auto.arima(x = psts_ind)
psts_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#212.3684
#s.e.   40.7609

#sigma^2 estimated as 33322:  log likelihood=-125.38
#AIC=254.76   AICc=255.51   BIC=256.65

forecast_psts<-forecast(psts_best, h = 5)
autoplot(forecast_psts, main = "Professional, scientific and technical services GDP Forecast", 
         xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       7946.368 7712.431 8180.306 7588.591 8304.145
#2018       8158.737 7827.899 8489.575 7652.764 8664.710
#2019       8371.105 7965.913 8776.297 7751.417 8990.793
#2020       8583.474 8115.598 9051.349 7867.920 9299.028
#2021       8795.842 8272.741 9318.943 7995.828 9595.856

forecast_psts[["mean"]]
# 2017 - [1] 7946.368
#Professional, scientific and technical services Real GDP 2017, 8063

## 13: Administrative and support, waste management and remediation services
aswr_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Administrative and support, waste management and remediation services"]
aswr_ind<- ts(aswr_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(aswr_ind, ylab = "GDP", xlab = "Year", title = "Administrative and support, waste management and remediation services/Year", acf = T)

aswr_best<- auto.arima(x = aswr_ind)
aswr_best

#Series:  
#ARIMA(0,2,1) 

#Coefficients:
#  ma1
#-0.6173
#s.e.   0.2408

#sigma^2 estimated as 9457:  log likelihood=-107.66
#AIC=219.31   AICc=220.11   BIC=221.09

forecast_aswr<-forecast(aswr_best, h = 5)
autoplot(forecast_aswr, main = "Administrative and support, waste management and remediation services GDP Forecast", 
         xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       3556.088 3431.464 3680.712 3365.492 3746.684
#2018       3567.176 3354.520 3779.833 3241.946 3892.406
#2019       3578.264 3272.285 3884.243 3110.310 4046.219
#2020       3589.352 3182.805 3995.900 2967.592 4211.113
#2021       3600.440 3085.908 4114.973 2813.531 4387.350

forecast_aswr[["mean"]]
# 2017 - [1] 3556.088
#Administrative and support, waste management and remediation services Real GDP 2017, 3505

## 14: Educational services
es_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Educational services"]
es_ind<- ts(es_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(es_ind, ylab = "GDP", xlab = "Year", title = "Educational services/Year", acf = T)

es_best<- auto.arima(x = es_ind)
es_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#160.7368
#s.e.   41.1820

#sigma^2 estimated as 34015:  log likelihood=-125.57
#AIC=255.15   AICc=255.9   BIC=257.04

forecast_es<-forecast(es_best, h = 5)
autoplot(forecast_es, main = "Educational services GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       8357.737 8121.380 8594.094 7996.260 8719.214
#2018       8518.474 8184.214 8852.733 8007.268 9029.679
#2019       8679.211 8269.828 9088.593 8053.114 9305.307
#2020       8839.947 8367.233 9312.662 8116.993 9562.901
#2021       9000.684 8472.174 9529.195 8192.397 9808.971

forecast_es[["mean"]]
# 2017 - [1] 8357.737
#Educational services Real GDP 2017, 8447

## 15: Health care and social assistance
hcsa_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Health care and social assistance"]
hcsa_ind<- ts(hcsa_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(hcsa_ind, ylab = "GDP", xlab = "Year", title = "Health care and social assistance/Year", acf = T)

hcsa_best<- auto.arima(x = hcsa_ind)
hcsa_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#158.4211
#s.e.   15.3391

#sigma^2 estimated as 4721:  log likelihood=-106.81
#AIC=217.62   AICc=218.37   BIC=219.51

forecast_hcsa<-forecast(hcsa_best, h = 5)
autoplot(forecast_hcsa, main = "Health care and social assistance GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast     Lo 80     Hi 80    Lo 95     Hi 95
#2017       9607.421  9519.366  9695.476 9472.753  9742.089
#2018       9765.842  9641.314  9890.370 9575.392  9956.292
#2019       9924.263  9771.748 10076.779 9691.011 10157.515
#2020      10082.684  9906.575 10258.794 9813.348 10352.021
#2021      10241.105 10044.209 10438.002 9939.978 10542.233

forecast_hcsa[["mean"]]
#2017 - [1]  9607.421
#Health care and social assistance Real GDP 2017, 9715

## 16: Arts, entertainment and recreation
aer_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Arts, entertainment and recreation"]
aer_ind<- ts(aer_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(aer_ind, ylab = "GDP", xlab = "Year", title = "Arts, entertainment and recreation/Year", acf = T)

aer_best<- auto.arima(x = aer_ind)
aer_best

#Series:  
#ARIMA(0,1,0) 

#sigma^2 estimated as 2392:  log likelihood=-100.87
#AIC=203.74   AICc=203.97   BIC=204.68

forecast_aer<-forecast(aer_best, h = 5)
autoplot(forecast_aer, main = "Arts, entertainment and recreation GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017           1046 983.3242 1108.676 950.1456 1141.854
#2018           1046 957.3630 1134.637 910.4414 1181.559
#2019           1046 937.4423 1154.558 879.9753 1212.025
#2020           1046 920.6483 1171.352 854.2911 1237.709
#2021           1046 905.8526 1186.147 831.6630 1260.337

forecast_aer[["mean"]]
# 2017 - [1] 1046
#Arts, entertainment and recreation Real GDP 2017, 1086

## 17: Accommodation and food services
afs_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Accommodation and food services"]
afs_ind<- ts(afs_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(afs_ind, ylab = "GDP", xlab = "Year", title = "Accommodation and food services/Year", acf = T)

afs_best<- auto.arima(x = afs_ind)
afs_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#55.1053
#s.e.  14.4918

#sigma^2 estimated as 4212:  log likelihood=-105.73
#AIC=215.46   AICc=216.21   BIC=217.35

forecast_afs<-forecast(afs_best, h = 5)
autoplot(forecast_afs, main = "Accommodation and food services GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       2925.105 2841.932 3008.278 2797.903 3052.308
#2018       2980.211 2862.586 3097.835 2800.319 3160.102
#2019       3035.316 2891.256 3179.376 2814.995 3255.637
#2020       3090.421 2924.075 3256.767 2836.016 3344.826
#2021       3145.526 2959.546 3331.507 2861.093 3429.959

forecast_afs[["mean"]]
# 2017 - [1] 2925.105
#Accommodation and food services Real GDP 2017, 3016

## 18: Other services (except public administration)
os_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Other services (except public administration)"]
os_ind<- ts(os_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(os_ind, ylab = "GDP", xlab = "Year", title = "Other services/Year", acf = T)

os_best<- auto.arima(x = os_ind)
os_best

#Series:  
#ARIMA(0,2,1) 

#Coefficients:
#  ma1
#-0.7172
#s.e.   0.1805

#sigma^2 estimated as 2706:  log likelihood=-96.51
#AIC=197.03   AICc=197.83   BIC=198.81

forecast_os<-forecast(os_best, h = 5)
autoplot(forecast_os, main = "Other services GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017       2709.205 2642.543 2775.867 2607.254 2811.156
#2018       2725.410 2616.984 2833.837 2559.586 2891.234
#2019       2741.615 2591.123 2892.107 2511.457 2971.773
#2020       2757.820 2563.323 2952.318 2460.362 3055.278
#2021       2774.025 2533.170 3014.880 2405.669 3142.381

forecast_os[["mean"]]
#2017 - [1] 2709.205
#Other services (except public administration) Real GDP 2017, 2733

## 19: Public administration
pa_ind<- IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Public administration"]
pa_ind<- ts(pa_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year))

plotTimesSeries(pa_ind, ylab = "GDP", xlab = "Year", title = "Public administration/Year", acf = T)

pa_best<- auto.arima(x = pa_ind)
pa_best

#Series:  
#ARIMA(0,1,0) with drift 

#Coefficients:
#  drift
#145.7895
#s.e.   39.2661

#sigma^2 estimated as 30924:  log likelihood=-124.67
#AIC=253.34   AICc=254.09   BIC=255.23

forecast_pa<-forecast(pa_best, h = 5)
autoplot(forecast_pa, main = "Public administration GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80     Hi 80    Lo 95     Hi 95
#2017       8923.789 8698.426  9149.153 8579.126  9268.453
#2018       9069.579 8750.867  9388.291 8582.151  9557.007
#2019       9215.368 8825.027  9605.710 8618.393  9812.344
#2020       9361.158 8910.431  9811.885 8671.830 10050.486
#2021       9506.947 9003.019 10010.876 8736.256 10277.639

forecast_pa[["mean"]]
#2017 - [1] 8923.789
#Public administration Real GDP 2017, 9043

## 20: Management of companies and enterprises (has only 11 years of complete data)
IND_GDP_Yr<-IND_GDP_Yr[IND_GDP_Yr$Year >= 2007,]

plotTimesSeries(moce_ind, ylab = "GDP", xlab = "Year", title = "Management of companies and enterprises/Year", acf = T)

moce_ind<<-IND_GDP_Yr$VAL[IND_GDP_Yr$NAICS == "Management of companies and enterprises"] #isolating all industries
moce_ind<- ts(moce_ind, start = min(IND_GDP_Yr$Year), end = max(IND_GDP_Yr$Year)) #converting to timestamp
moce_best<- auto.arima(x = moce_ind)
moce_best

#Series:  
#ARIMA(0,0,0) with non-zero mean 

#Coefficients:
#  mean
#947.6000
#s.e.    9.2771

#sigma^2 estimated as 956.3:  log likelihood=-47.98
#AIC=99.96   AICc=101.67   BIC=100.56

forecast_moce<-forecast(moce_best, h = 5)
autoplot(forecast_moce, main = "Management of companies and enterprises GDP Forecast", xlab = "Year", ylab = "GDP")

#Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
#2017          947.6 907.9699 987.2301 886.9909 1008.209
#2018          947.6 907.9699 987.2301 886.9909 1008.209
#2019          947.6 907.9699 987.2301 886.9909 1008.209
#2020          947.6 907.9699 987.2301 886.9909 1008.209
#2021          947.6 907.9699 987.2301 886.9909 1008.209

forecast_moce[["mean"]]
#2017 - [1] 947.6
#Management of companies and enterprises Real 2017 GDP, 902

View(IND_GDP_2017)
