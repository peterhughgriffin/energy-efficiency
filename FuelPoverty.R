# Import data
setwd("C:/Users/Peter/Documents/GIS/HousingStock/Data/Fuel_Poverty_2019")   # set working directory

Tenure = read.csv("Tenure.csv")  # read csv file 
HouseholdSize = read.csv("HouseholdSize.csv")  # read csv file 
HouseholdType = read.csv("HouseholdType.csv")  # read csv file 

# Calculate the percentages ----

# Add in Percentage Share columns

# Tenure
Share_FP<-list(`Share_FuelPoor_pc`=100*Tenure$Number.fuel.poor..000s./Tenure$Number.fuel.poor..000s.[length(Tenure$Number.fuel.poor..000s.)])
# Add it to the rest of the data
Tenure_Exp<-cbind(Tenure,Share_FP)

# Size
Share_FP<-list(`Share_FuelPoor_pc`=100*HouseholdSize$Number.fuel.poor..000s./HouseholdSize$Number.fuel.poor..000s.[length(HouseholdSize$Number.fuel.poor..000s.)])
# Add it to the rest of the data
HouseholdSize_Exp<-cbind(HouseholdSize,Share_FP)

# Type
Share_FP<-list(`Share_FuelPoor_pc`=100*HouseholdType$Number.fuel.poor..000s./HouseholdType$Number.fuel.poor..000s.[length(HouseholdType$Number.fuel.poor..000s.)])
# Add it to the rest of the data
HouseholdType_Exp<-cbind(HouseholdType,Share_FP)

# Plotting ----

#Plot Function

Plot_FP<-function(Table,Column,Lab_Title,Lab_x,Lab_y,Lab_tilt,Align,Order,LabStr){
  Len<-nrow(Table)
  Wid<-ncol(Table)
  Temp<-Table[-Len,]
  if (Order){
    Temp<-Temp[order(Temp[Column],decreasing = TRUE),]
  }
  
  x11(width=9, height=6, pointsize=12)
  par(mar=c(10,5,2,5))
  
  p<-barplot(Temp[,Column],
             width=Wid/Len,
             main=Lab_Title,
             ylab=Lab_y,
             xlab=Lab_x,
             col=heat.colors(Len),
             ylim=c(0,max(Temp[,Column])+max(Temp[,Column])/5),
             xlim=c(0,Wid),
             cex.axis=2,
             cex.lab=2)
  axis(1,labels=FALSE,at=p)
  text(p+Align, -max(Temp[,Column])/12,
       labels=Temp[,1],
       srt=Lab_tilt, adj=1, xpd=TRUE,cex=2)
  text(p+0.6, max(Temp[,Column])/12+Temp[,Column],
       labels=paste(round(Temp[,Column],1),LabStr),
       srt=0, adj=1, xpd=TRUE,cex=2)
}


# # Percentage Fuel Poor
# Plot_FP(Tenure_Exp,6,"Fuel Poverty by Tenure","","Proportion Fuel Poor (%)",7,0.6,TRUE,'%')
# Plot_FP(HouseholdSize_Exp,6,"Fuel Poverty by Household Size","Household Size","Proportion Fuel Poor (%)",0,0.07,FALSE,'%')
# Plot_FP(HouseholdType_Exp,6,"Fuel Poverty by Household Type","","Proportion Fuel Poor (%)",15,0.5,TRUE,'%')

# Number Fuel Poor
Plot_FP(Tenure_Exp,5,"Fuel Poverty by Tenure","","Number of Fuel Poor",7,0.6,TRUE,'')
Plot_FP(HouseholdSize_Exp,5,"Fuel Poverty by Household Size","Household Size","Number of Fuel Poor",0,0.07,FALSE,'')
# Plot_FP(HouseholdType_Exp,5,"Fuel Poverty by Household Type","","Number of Fuel Poor",15,0.5,TRUE,'')

# Percentage Fuel Poor
Plot_FP(Tenure_Exp,3,"Fuel Poverty by Tenure","","Proportion Fuel Poor (%)",7,0.6,TRUE,'%')
Plot_FP(HouseholdSize_Exp,3,"Fuel Poverty by Household Size","Household Size","Proportion Fuel Poor (%)",0,0.07,FALSE,'%')
# Plot_FP(HouseholdType_Exp,3,"Fuel Poverty by Household Type","","Proportion Fuel Poor (%)",15,0.5,TRUE,'%')
