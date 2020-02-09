## ********* Import Libraries *********----
library(httr)
library(rlist)
library(plyr)

## ********* Set API access Details *********----
# Grab EPC data from API
url  <- "https://epc.opendatacommunities.org/api/v1/domestic/search?postcode="
# Get the API credentials
Creds = read.csv("Credentials.csv")
api_key<-paste(Creds[1,1])
user<-paste(Creds[1,2])
pass<-paste(Creds[1,3])

## ********* Grab Post Codes *********----

# Got Post codes of the LSOA in question from here: https://www.doogal.co.uk/LSOA.php?code=E01027381
# Import this data
setwd("C:/Users/Peter/Documents/GIS/HousingStock/Data/Northumberland")   # set working directory
PostCodes = read.csv("Northumberland 003C postcodes.csv")  # read csv file 


URLList<-paste(url,PostCodes[,1],sep="")
URLList<-gsub(" ", "", URLList, fixed = TRUE) # Remove space from post code

for (url in URLList){
    # Grab EPC data from API
    EPC <- GET(url, authenticate(user, pass, type = "basic"),
               add_headers(accept="text/csv", auth_appkey = api_key, "COLUMN_NAME" = "CURRENT_ENERGY_RATING"))
    stop_for_status(EPC)
    Result = as.data.frame(content(EPC))
    if (exists("EPCs")){
      EPCs<-rbind(EPCs,Result)
    }    else {
      EPCs<-Result
    }
}

## ********* Clean Data *********----

# Remove older EPCs from duplicated addresses
EPCs<-EPCs[!duplicated(EPCs$address) & order(EPCs$`inspection-date`,decreasing = TRUE),]

# Remove data with 'False' EPC rating
EPCs<-EPCs[EPCs$`current-energy-rating` != "FALSE" & EPCs$`potential-energy-rating` != "FALSE", ]

## ********* Plot EPCs *********----
# Set colours for plotting EPCs
EPC_col<-c("#007f3d","#2c9f29","#9dcb3c","#fff200","#f7af1d","#ed6823","#e31d23")

# Current EPCs
EPCs_Current<-table(EPCs[7])

barplot(EPCs_Current, main="Distribution of Current EPC ratings",
        xlab="EPC Rating", ylab="Number of Households",col=EPC_col)

# Potential EPCs
EPCs_Potential<-table(EPCs[8])

barplot(EPCs_Potential, main="Distribution of Potential EPC ratings",
        xlab="EPC Rating", ylab="Number of Households",col=EPC_col)

## ********* Examine Energy Efficiency Values *********----

# Calculate the improvement of energy efficiency 
Improvement<-list(`energy-efficiency-improvement`=EPCs$`potential-energy-efficiency`-EPCs$`current-energy-efficiency`)
# Add it to the rest of the data
EPCs_Expanded<-cbind(EPCs,Improvement)


# Produce boxplots
boxplot(EPCs_Expanded$`energy-efficiency-improvement`)

boxplot(EPCs_Expanded$`current-energy-efficiency`)

boxplot(EPCs_Expanded$`potential-energy-efficiency`)

# Compare energy efficiency distributions of Current, Potential and improvement
boxplot(EPCs_Expanded$`current-energy-efficiency`,
        EPCs_Expanded$`potential-energy-efficiency`,
        EPCs_Expanded$`energy-efficiency-improvement`,
        names=c('Current','Potential','Improvement'),
        main="Distribution of Current, Potential and Improvement for all Properties",
        ylab="Efficiency",
        ylim=c(0,130))


N_OffGas<-length(subset(EPCs_Expanded,`mains-gas-flag`=='N')[,1])
N_OnGas<-length(subset(EPCs_Expanded,`mains-gas-flag`=='Y')[,1])

# Compare the potential improvement for homes on and off the gas grid
boxplot(subset(EPCs_Expanded,`mains-gas-flag`=='Y')$`energy-efficiency-improvement`,
        subset(EPCs_Expanded,`mains-gas-flag`=='N')$`energy-efficiency-improvement`,
        names=c(paste('On the Gas Grid',' (',N_OnGas,' homes)',sep=''),paste('Off the Gas Grid',' (',N_OffGas,' homes)',sep='')),
        main="Potential Efficiency Improvement On and Off the Gas Grid",
        ylab="Efficiency",
        ylim=c(0,130))

## ********* Examine Carbon Emission Values *********----

# Calculate the improvement of emissions
Improvement<-list(`co2-emissions-improvement`=EPCs$`co2-emissions-potential`-EPCs$`co2-emissions-current`)
# Add it to the rest of the data
EPCs_Expanded<-cbind(EPCs_Expanded,Improvement)


# Produce boxplots
boxplot(EPCs_Expanded$`co2-emissions-improvement`)

boxplot(EPCs_Expanded$`co2-emissions-current`)

boxplot(EPCs_Expanded$`co2-emissions-potential`)

# Compare CO2 Emissions distributions of Current, Potential and improvement
boxplot(EPCs_Expanded$`co2-emissions-current`,
        EPCs_Expanded$`co2-emissions-potential`,
        -EPCs_Expanded$`co2-emissions-improvement`,
        names=c('Current','Potential','Improvement'),
        main="Distribution of Current, Potential and Improvement for all Properties",
        ylab="Emissions (Tonnes/year)")


N_OffGas<-length(subset(EPCs_Expanded,`mains-gas-flag`=='N')[,1])
N_OnGas<-length(subset(EPCs_Expanded,`mains-gas-flag`=='Y')[,1])

# Compare the potential improvement for homes on and off the gas grid
boxplot(-subset(EPCs_Expanded,`mains-gas-flag`=='Y')$`co2-emissions-improvement`,
        -subset(EPCs_Expanded,`mains-gas-flag`=='N')$`co2-emissions-improvement`,
        names=c(paste('On the Gas Grid',' (',N_OnGas,' homes)',sep=''),paste('Off the Gas Grid',' (',N_OffGas,' homes)',sep='')),
        main="Potential Co2 Emissions Improvement On and Off the Gas Grid",
        ylab="Emissions (Tonnes/year)")


# Compare the Emissions for homes on and off the gas grid
boxplot(subset(EPCs_Expanded,`mains-gas-flag`=='Y')$`co2-emissions-current`,
        subset(EPCs_Expanded,`mains-gas-flag`=='N')$`co2-emissions-current`,
        names=c(paste('On the Gas Grid',' (',N_OnGas,' homes)',sep=''),paste('Off the Gas Grid',' (',N_OffGas,' homes)',sep='')),
        main="Potential Co2 Emissions Improvement On and Off the Gas Grid",
        ylab="Emissions (Tonnes/year)")

## ********* Plot Carbon Emissions for house type *********----
AveEmissions_Form <- ddply(EPCs_Expanded, .(`built-form`), function(x) mean(x$`co2-emiss-curr-per-floor-area`) )

AveEmissions_Form<-AveEmissions_Form[order(AveEmissions_Form$V1,decreasing = TRUE),]


# Angled Labels
p<-barplot(subset(AveEmissions_Form,`built-form`!='NO DATA!')$`V1`,
           main="Average CO2 Emissions by Household Type",
           ylab="Emissions (kg/m2/year)",
           col=topo.colors(6))
axis(1,labels=FALSE,at=seq(0.02,8.2,1.22))
text(p+0.4, -8,
     labels=subset(AveEmissions_Form,`built-form`!='NO DATA!')$`built-form`,
     srt=20, adj=1, xpd=TRUE)

# Labels straight up
p<-barplot(subset(AveEmissions_Form,`built-form`!='NO DATA!')$`V1`,
           main="Average CO2 Emissions by Household Type",
           ylab="Emissions (kg/m2/year)",
           col=topo.colors(6))

axis(1,labels=FALSE,lwd.ticks=0)
text(p, 130,
     labels=subset(AveEmissions_Form,`built-form`!='NO DATA!')$`built-form`,
     srt=90, adj=1, xpd=TRUE)

## ********* Plot Carbon Emissions for house type *********----
AveEmissions_Type <- ddply(EPCs_Expanded, .(`property-type`), function(x) mean(x$`co2-emiss-curr-per-floor-area`) )

AveEmissions_Type<-AveEmissions_Type[order(AveEmissions_Form$V1,decreasing = TRUE),]


# Angled Labels
p<-barplot(subset(AveEmissions_Type,`property-type`!='NO DATA!')$`V1`,
           main="Average CO2 Emissions by Property Type",
           ylab="Emissions (kg/m2/year)",
           col=topo.colors(6))
axis(1,labels=FALSE,at=seq(0.02,6,1.22))
text(p+0.2, -8,
     labels=subset(AveEmissions_Type,`property-type`!='NO DATA!')$`property-type`,
     srt=0, adj=1, xpd=TRUE)

# Labels straight up
p<-barplot(subset(AveEmissions_Type,`property-type`!='NO DATA!')$`V1`,
           main="Average CO2 Emissions by Property Type",
           ylab="Emissions (kg/m2/year)",
           col=topo.colors(6))

axis(1,labels=FALSE,lwd.ticks=0)
text(p, 70,
     labels=subset(AveEmissions_Type,`property-type`!='NO DATA!')$`property-type`,
     srt=90, adj=1, xpd=TRUE)



