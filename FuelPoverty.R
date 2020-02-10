library(waffle) # For square (or Waffle) plots
library(ggplot2)

# Import data
setwd("C:/Users/Peter/Documents/GIS/HousingStock/Data/Fuel_Poverty_2019")   # set working directory

Tenure = read.csv("Tenure.csv")  # read csv file 
HouseholdSize = read.csv("HouseholdSize.csv")  # read csv file 
HouseholdType = read.csv("HouseholdType.csv")  # read csv file 

# Calculate the percentages ----

Share_FP<-list(`Share_FuelPoor_pc`=100*Tenure$Number.fuel.poor..000s./Tenure$Number.fuel.poor..000s.[length(Tenure$Number.fuel.poor..000s.)])
# Add it to the rest of the data
Tenure_Exp<-cbind(Tenure,Share_FP)


Len<-nrow(Tenure_Exp)
Wid<-ncol(Tenure_Exp)
Temp<-Tenure_Exp[-Len,]

p<-barplot(Temp$Share_FuelPoor_pc[order(Temp$Share_FuelPoor_pc,decreasing = TRUE)],
           main="Average CO2 Emissions by Household Type",
           ylab="Emissions (kg/m2/year)",
           col=topo.colors(4))
axis(1,labels=FALSE,at=seq(0.05,8.2,1.22))
text(p+0.5, -3,
     labels=Temp[,1],
     srt=20, adj=1, xpd=TRUE)
text(p+0.5, 3+Temp$Share_FuelPoor_pc[order(Temp$Share_FuelPoor_pc,decreasing = TRUE)],
     labels=paste(round(Temp$Share_FuelPoor_pc[order(Temp$Share_FuelPoor_pc,decreasing = TRUE)],1),'%'),
     srt=0, adj=1, xpd=TRUE)

