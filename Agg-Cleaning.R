

library(dplyr)
library(stringr)
library("Hmisc")

##Working on forecasts for Aggregates of all industries.

#I have extracted 21 years for this as for 20 aggregates, only 1 has 11 years worth of data, 
#rest have for 21 years, I will choose along 21 or 11 as I go best suited for my models. 

#All industries is the total:
#Link for info: "https://www.statcan.gc.ca/eng/statistical-programs/document/1301_D2_T9_V1"
#we can easily extract 21 years worth of data for aggregates of industries, 
#except one (Management of companies and enterprises)


#Real Raw Data
GDP<-read.csv("C:/Users/shafa/Downloads/DS1/36100491.csv", header = TRUE, sep = ",", stringsAsFactors = F, na.strings = c("","NA","?"))
names(GDP) <- c("REF_DT","GEO", "DGUID", "Seas.Adjst", "Prices", "NAICS", "Release", "UOM", "UOM_ID", 
                "SCLR_FCTOR", "SCLR_ID", "VCTOR", "CORD", "VAL", "STATUS", "SMBL", "TRMNTD", "DECMLS")
#Breaking the Year and Month string
GDP$Year <- substr(GDP$REF_DT,1,4)
GDP$Month <- substr(GDP$REF_DT,6,8)

#Removing the incomplete 2018 Data

#Year_18<-GDP[GDP$Year == "2018", ]
#nrow(Year_18)
GDP_1 <- subset(GDP, Year!="2018")
#View(GDP_1[1:2000,])

#Choosing the only attributes/columns we need
GDP_2<-GDP_1[,c(1,19,20,4,5,6,7,14)]
#View(GDP_2[1:200,])
#Removing NAs, as we have mutiple duplicates, we dont need to treat the NAs or fill in the missing,
#the removal is best here
sapply(GDP_2, function(x) sum(is.na(x)))
GDP_2<-GDP_2[complete.cases(GDP_2),]

#Taking notice of Seasonaly Adjusted and Trading Adjusted Data
table(GDP_2$Prices)
table(GDP_2$Seas.Adjst)
TD_GDP<-GDP_2[GDP_2$Seas.Adjst == "Trading-day adjusted",]
#SA_GDP<-GDP_2[GDP_2$Seas.Adjst != "Trading-day adjusted",]

#we are looking for dates that have the complete data from 1997-2017, if not we'd choose the years
#with all the industry values in them.
#our focus is the same trading day adjusted value.. corresponding to the same Release date over the years
#over the months. 

#1: First we will take out the Release dates which are effective Nov 30th, as we have seen in the notes
#it's incomplete
TD_GDP<-TD_GDP[TD_GDP$Release != "December 21, 2018" & TD_GDP$Release != "November 30, 2018",]

#2: breaking the Years in the Release for my inquiry to find the most released dates.
TD_GDP$Rel_Yr <- str_sub(TD_GDP$Release,-4,-1)

TD_GDP <- subset(TD_GDP, Rel_Yr =="2018")
#View(table(TD_GDP$Release))

#3:Choosing the most recent Release date for my observation.
TD_GDP<-TD_GDP[TD_GDP$Release == "October 31, 2018",]


#4:Final attributes we now need to convert it inot an annual time series
IND_GDP<-TD_GDP[,c(2,3,6,8)]
IND_GDP$Year<-as.numeric(TD_GDP$Year)
IND_GDP$Month<-as.numeric(TD_GDP$Month)
str(IND_GDP)

#Writing CSV with the new dataframes
write.csv(IND_GDP, file = "C:/Users/shafa/Downloads/IND_GDP.csv", row.names = FALSE)


#5: The industry annual data from 1997 onwards, we will now use the data from, we will now use Month == 12,
#as the anual GDP Value and forecast the next years on its basis. 

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

View(table(AGG_GDP$NAICS)) #observed just "Management of companies and enterprises" has 132 enteries for 11 years.
set.seed(100)

AGG_GDP <- subset(AGG_GDP, Month == 12) #only taking the annual year end GDP

