

# DATA STEPS

code.violations <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/code%20violations.csv")
code.severity <- read.csv("https://raw.githubusercontent.com/subartle/orangespot/master/data/Severity.csv")

code.violations <- merge(code.violations, code.severity, by.x = "Code", by.y = "Row.Labels", all.x=T )
code.violations$Severity[is.na(code.violations$Severity)] <- "FALSE"  #this makes the NA in severity 0



#Convert to time class
code.violations$Violation.Date <- as.Date(code.violations$Violation.Date,format = "%m/%d/%Y")
code.violations$Complaint.Close.Date <- as.Date(code.violations$Complaint.Close.Date, format = "%m/%d/%Y")
code.violations$Complaint.Date <- as.Date(code.violations$Complaint.Date, "%m/%d/%Y")
code.violations$Comply.By.Date <- as.Date(code.violations$Comply.By.Date, format = "%m/%d/%Y")


# create new variables representing time between dates 
# and get rid of negative amounts (due to incorrect data entry)

code.violations$TimeBetweenOCB <- code.violations$Comply.By.Date - code.violations$Violation.Date
code.violations$TimeBetweenOCB[ code.violations$TimeBetweenOCB < 0 ] <- NA

code.violations$TimeBetweenCV <- code.violations$Complaint.Date - code.violations$Violation.Date
code.violations$TimeBetweenCV[ code.violations$TimeBetweenCV < 0 ] <- NA

code.violations$TimeBetweenOC <- code.violations$Complaint.Close.Date - code.violations$Violation.Date
code.violations$TimeBetweenOC[ code.violations$TimeBetweenOC < 0 ] <- NA

code.violations$TimeBetweenOC[ is.na(code.violations$TimeBetweenOC) ] <- 9999  #this makes the NA in severity 0

# code.violations <- mutate(code.violations, TimeBetweenOCB = code.violations$Comply.By.Date - code.violations$Violation.Date)
# code.violations$TimeBetweenOCB[code.violations$TimeBetweenOCB < 0 ] <- NA
# code.violations <- mutate(code.violations, TimeBetweenCV = (code.violations$Complaint.Date - code.violations$Violation.Date) )
# code.violations$TimeBetweenCV[code.violations$TimeBetweenCV < 0 ] <- NA
# code.violations <- mutate(code.violations, TimeBetweenOC = (code.violations$Complaint.Close.Date - code.violations$Violation.Date))
# code.violations$TimeBetweenOC[code.violations$TimeBetweenOC < 0 ] <- NA
# code.violations$TimeBetweenOC[is.na(code.violations$TimeBetweenOC)] <- 9999  #this makes the NA in severity 0






parcels <- read.csv( "https://raw.githubusercontent.com/hectorlca/Code-Violations/master/data/parcels.csv" )

cv <- code.violations[ !( is.na(code.violations$lon) | is.na(code.violations$lat)) , ]

code.violations <- merge( cv, parcels, by.x="Identifier", by.y="SBL" )
# merged <- merge( cv, parcels, by.x="Identifier", by.y="SBL" )


# setwd( "C:/Users/jdlecy/Documents/GitHub/orangespot/data" )

saveRDS( code.violations, "code.violations.final.rds" )


# write.csv( code.violations, "code.violations.final.csv", row.names=FALSE )
