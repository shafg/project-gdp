#RMSE Comparision: 


MODEL = c("ARIMA", "VAR", "RF", "LM", "KNN" )
RMSEs = c(572.9606, 667.9292,915.5825,995,1049.888  )
RMSE_Model <- as.data.frame(cbind(MODEL, RMSEs))

View(RMSE_Model)
str(RMSE_Model)
