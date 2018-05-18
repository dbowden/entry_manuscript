library(dplyr)
library(countrycode)

#single-age data from here: https://esa.un.org/unpd/wpp/Download/Standard/CSV/
#specific dataset: https://esa.un.org/unpd/wpp/DVD/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2017_PopulationBySingleAgeSex.csv
#all figures in thousands
pop <- read.csv("~/Documents/Data/WPP2017_PopulationBySingleAgeSex.csv")

#convert age group from factor to numeric
pop$AgeGrp <- as.numeric(levels(pop$AgeGrp))[pop$AgeGrp]

#Remove NAs for 80+ category
pop$AgeGrp <- ifelse(is.na(pop$AgeGrp), pop$AgeGrpStart, pop$AgeGrp)

#Aggregate to country-years
popsum <- pop %>% 
  group_by(Location, Time) %>% 
  summarize(totpop=sum(PopTotal), youthpop=sum(PopTotal[AgeGrp >= 15 & AgeGrp <= 24]), adultpop=sum(PopTotal[AgeGrp >= 15]))

#calculate youth bulge measure
popsum$youthbulge <- popsum$youthpop / popsum$adultpop

#add countrycodes
popsum$GWNoA <- countrycode(popsum$Location, "country.name", "cown")

popsum <- select(ungroup(popsum), GWNoA, Year=Time, youth_bulge=youthbulge)

write.csv(popsum, "youth_bulge_updated.csv", row.names = F)

rm(pop, popsum)
