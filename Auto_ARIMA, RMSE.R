
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


forecast_all_ind5[["mean"]]
# 2017 - [1] 143010.4

forecast_Affh[["mean"]]
# 2017 - [1] 1230

forecast_mqogg[["mean"]]
# 2017 - [1] 11928

forecast_u[["mean"]]
#2017 [1] 3647.384

forecast_c[["mean"]]
# 2017 - [1]  9541.105

forecast_m[["mean"]]
#2017 - [1] 13277.00

forecast_wt[["mean"]]
#2017 - [1] 7836.421

forecast_rt[["mean"]]
# 2017 - [1]  9278.316

forecast_tw[["mean"]]
# 2017 - [1] 6414.053

forecast_ic[["mean"]]
#2017 - [1] 4309.010

forecast_fi[["mean"]]
#2017 - [1] 10421.21

forecast_rrl[["mean"]]
#2017 - [1] 18556.89

forecast_psts[["mean"]]
# 2017 - [1] 7946.368

forecast_moce[["mean"]]
#2017 - [1] 947.6

forecast_aswr[["mean"]]
# 2017 - [1] 3556.088

forecast_es[["mean"]]
# 2017 - [1] 8357.737

forecast_hcsa[["mean"]]
#2017 - [1]  9607.421

forecast_aer[["mean"]]
# 2017 - [1] 1046

forecast_afs[["mean"]]
# 2017 - [1] 2925.105

forecast_os[["mean"]]
#2017 - [1] 2709.205

forecast_pa[["mean"]]
#2017 - [1] 8923.789

AGG_GDP_2017[,"PRED"] <- c(143010.4, 1230 ,11928 , 3647.384, 9541.105,  13277.00 , 7836.421 , 9278.316 ,6414.053,4309.010 ,
10421.21, 18556.89,7946.368,947.6,3556.088, 8357.737,9607.421, 1046,2925.105,2709.205,8923.789)

AGG_GDP_2017[,"ERROR"] <- AGG_GDP_2017$VAL - AGG_GDP_2017$PRED

View(AGG_GDP_2017)
TOTAL_ERROR <- sum(AGG_GDP_2017$ERROR)
TOTAL_ERROR

RMSE_ARIMA <- RMSE(AGG_GDP_2017$VAL, AGG_GDP_2017$PRED)
RMSE_ARIMA #[1] 572.9606

ARIME_ER<- RMSE(AGG_GDP_2017$VAL, AGG_GDP_2017$PRED)/mean(AGG_GDP_2017$VAL) #prediction error rate
ARIME_ER #[1] 0.04138322
