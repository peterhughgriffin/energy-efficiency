
library(tidyverse) # For filtering


# Import Data
setwd("C:/Users/Peter/Documents/GIS/HousingStock/Data/LSOA dataset_web 2")   # set working directory
Housing = read.csv("LSOA dataset_web.csv")  # read csv file 

FuelPov = read.csv("FuelPovertySubRegion.csv")  # read csv file 

# Filter Data

Northumberland_Housing<- filter(Housing,LA.Name == 'North Tyneside') # Filter dataset to just that from Northumberland

Northumberland_FuelPov<- filter(FuelPov,LA.Name == 'Northumberland') # Filter dataset to just that from Northumberland


attach(Northumberland_Housing)
summary(IMD)
Households<-sum(Total.Households)
OffGas<-sum(No.of.off.gas)

barplot(No.of.off.gas)
detach(Northumberland_Housing)

attach(Northumberland_FuelPov)


barplot(Estimated.number.of.fuel.poor.households)
detach(Northumberland_FuelPov)

# attach(LSOA)
# summary(IMD)
# detach(LSOA)




