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

Plot_FP<-function(Table,Lab_Title,Lab_x,Lab_y,Lab_tilt,Align,Order){
  Len<-nrow(Table)
  Wid<-ncol(Table)
  Temp<-Table[-Len,]
  if (Order){
    Temp<-Temp[order(Temp$Share_FuelPoor_pc,decreasing = TRUE),]
  }
  
  x11(width=9, height=6, pointsize=12)
  par(mar=c(10,5,2,5))
  
  p<-barplot(Temp$Share_FuelPoor_pc,
             width=Wid/Len,
             main=Lab_Title,
             ylab=Lab_y,
             xlab=Lab_x,
             col=heat.colors(Len),
             ylim=c(0,max(Temp$Share_FuelPoor_pc)+10),
             xlim=c(0,Wid),
             cex.axis=2,
             cex.lab=2)
  axis(1,labels=FALSE,at=p)
  text(p+Align, -3,
       labels=Temp[,1],
       srt=Lab_tilt, adj=1, xpd=TRUE,cex=2)
  text(p+0.5, 3+Temp$Share_FuelPoor_pc,
       labels=paste(round(Temp$Share_FuelPoor_pc,1),'%'),
       srt=0, adj=1, xpd=TRUE,cex=2)
}

Plot_FP(Tenure_Exp,"Fuel Poverty by Tenure","","Proportion Fuel Poor (%)",7,0.6,TRUE)
Plot_FP(HouseholdSize_Exp,"Fuel Poverty by Household Size","Household Size","Proportion Fuel Poor (%)",0,0.07,FALSE)
# Plot_FP(HouseholdType_Exp,"Fuel Poverty by Household Type","","Proportion Fuel Poor (%)",15,0.5,TRUE)



