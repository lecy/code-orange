
###################################################
############## DATA STEPS SECTION #################
###################################################

#setwd("C:/Users/snhalbritter/Documents/GitHub/code-orange/data")

#Read in Data
dat.1 <- read.csv("https://raw.githubusercontent.com/lecy/code-orange/master/data/code%20violations.csv")
code.severity <- read.csv("https://raw.githubusercontent.com/lecy/code-orange/master/data/Severity.csv")
parcels <- read.csv( "https://raw.githubusercontent.com/lecy/code-orange/master/data/parcels.csv" )

#Merge Violations and Severity; Make the NA in severity 0
code.violations <- merge(dat.1, code.severity, by.x = "Code", by.y = "Row.Labels", all.x=T )
code.violations$Severity[is.na(code.violations$Severity)] <- "FALSE" 

#Convert to time class
code.violations$Violation.Date <- as.Date(code.violations$Violation.Date,format = "%m/%d/%Y")
code.violations$Complaint.Close.Date <- as.Date(code.violations$Complaint.Close.Date, format = "%m/%d/%Y")
code.violations$Complaint.Date <- as.Date(code.violations$Complaint.Date, "%m/%d/%Y")
code.violations$Comply.By.Date <- as.Date(code.violations$Comply.By.Date, format = "%m/%d/%Y")

#Create new variable representing time between dates 
#and get rid of negative amounts (due to incorrect data entry)

code.violations$TimeBetweenOCB <- code.violations$Comply.By.Date - code.violations$Violation.Date
code.violations$TimeBetweenOCB[ code.violations$TimeBetweenOCB < 0 ] <- NA

code.violations$TimeBetweenCV <- code.violations$Complaint.Date - code.violations$Violation.Date
code.violations$TimeBetweenCV[ code.violations$TimeBetweenCV < 0 ] <- NA

code.violations$TimeBetweenOC <- code.violations$Complaint.Close.Date - code.violations$Violation.Date
code.violations$TimeBetweenOC[ code.violations$TimeBetweenOC < 0 ] <- NA

#this makes the NA in severity 0
code.violations$TimeBetweenOC[ is.na(code.violations$TimeBetweenOC) ] <- 9999 

#Dropping Locations with NA latitude or longitude
cv <- code.violations[ !( is.na(code.violations$lon) | is.na(code.violations$lat)) , ]

#Writing back to code.violations with a merge on parcels
code.violations <- merge( cv, parcels, by.x="Identifier", by.y="SBL" )

#save final formatted and merge data as RDS
saveRDS( code.violations, "code.violations.final.rds" )


# write.csv( code.violations, "code.violations.final.csv", row.names=FALSE )
