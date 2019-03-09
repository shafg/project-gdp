library(dplyr)
library(stringr)

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
#View(TD_GDP[1:2000 ,])
table(TD_GDP$Rel_Yr)
TD_GDP <- subset(TD_GDP, Rel_Yr =="2018")
#View(table(TD_GDP$Release))

#3:Choosing the most recent Release date for my observation.
TD_GDP<-TD_GDP[TD_GDP$Release == "October 31, 2018",]
View(TD_GDP)
table(TD_GDP$Year)
View(table(TD_GDP$NAICS))

#4:The years with full data for each industry are from 2007 onwards, we will now use the data from these 
#years only, 10 years to build the model, 2017 as the test year.

TD_GDP<-TD_GDP[TD_GDP$Year >= 2007,]
table(TD_GDP$Year)
View(table(TD_GDP$NAICS))

#Final attributes we now need
TD_GDP<-TD_GDP[,c(2,3,6,8)]
View(TD_GDP)

#changing type of the attributes
TD_GDP$Year<-as.numeric(TD_GDP$Year)
str(TD_GDP)
TD_GDP$Month<-as.numeric(TD_GDP$Month)