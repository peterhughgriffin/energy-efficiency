library(httr)
library(rlist)
library(ggplot2)

## ********* Set API access Details *********
# Grab EPC data from API
url  <- "https://epc.opendatacommunities.org/api/v1/domestic/search?postcode="
# Get the API credentials
Creds = read.csv("Credentials.csv")
api_key<-paste(Creds[1,1])
user<-paste(Creds[1,2])
pass<-paste(Creds[1,3])

## ********* Grab Post Codes *********

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

## ********* Clean Data *********

# Remove older EPCs from duplicated addresses
EPCs<-EPCs[!duplicated(EPCs$address) & order(EPCs$`inspection-date`,decreasing = TRUE),]

# Remove data with 'False' EPC rating
EPCs<-EPCs[EPCs$`current-energy-rating` != "FALSE" & EPCs$`potential-energy-rating` != "FALSE", ]

## ********* Plot EPCs *********
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

